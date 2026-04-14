# run_all.R
# Chạy toàn bộ thực nghiệm trên tất cả datasets và sinh Table 2 so sánh
# Thứ tự: Iris, Seeds, Wine (nhỏ — verify trước), sau đó các dataset lớn hơn
#
# Ghi chú thời gian ước tính:
#   Iris   (150×4×3)  : ~5-10 phút  (20 patterns × 50 inits × 7 ratios)
#   Seeds  (210×7×3)  : ~10-20 phút
#   Wine   (178×13×3) : ~15-25 phút (nhiều chiều hơn → Sigma inversion chậm hơn)
#   Segment+          : vài giờ — nên dùng parallel

# ── Setup ────────────────────────────────────────────────────────────────────
SCRIPT_DIR <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)
R_DIR <- file.path(SCRIPT_DIR, "..", "R")

source(file.path(R_DIR, "data_utils.R"))
source(file.path(R_DIR, "gmm_incomplete.R"))
source(file.path(R_DIR, "imputation_baseline.R"))
source(file.path(R_DIR, "evaluation.R"))

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a[1])) a else b

# ── Parameters ───────────────────────────────────────────────────────────────
MISSING_RATIOS <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
N_INITS        <- 50
N_PATTERNS     <- 20    # bài báo: 20 random patterns
SEED_BASE      <- 42
KM_NSTART      <- 3L    # K-means restarts
RESULTS_DIR    <- file.path(SCRIPT_DIR, "..", "results")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)

DATASET_NAMES  <- c("iris", "seeds", "wine")
# Thêm vào khi muốn chạy đầy đủ:
# DATASET_NAMES <- c("iris", "seeds", "wine", "alcoholqcm", "segment",
#                    "electricalgrid", "avila", "letter")

# ─────────────────────────────────────────────────────────────────────────────
# Hàm chạy thực nghiệm cho một dataset
# ─────────────────────────────────────────────────────────────────────────────
run_experiment_dataset <- function(dataset_name,
                                   X_orig, labels, k,
                                   missing_ratios = MISSING_RATIOS,
                                   n_inits        = N_INITS,
                                   n_patterns     = N_PATTERNS,
                                   seed_base      = SEED_BASE,
                                   verbose        = TRUE) {

  n <- nrow(X_orig); d <- ncol(X_orig)
  methods <- c("Proposed", "Mean", "Zero", "EM")
  metrics <- c("ACC", "NMI", "Fscore", "PUR")

  results_by_ratio <- list()

  for (ratio in missing_ratios) {
    if (verbose) cat(sprintf("\n  [%s] ratio=%.0f%%\n", dataset_name, ratio*100))

    res_all <- lapply(methods, function(m) {
      matrix(NA_real_, n_patterns * n_inits, 4, dimnames = list(NULL, metrics))
    })
    names(res_all) <- methods

    run_idx <- 1
    for (pat in seq_len(n_patterns)) {
      seed_pat <- seed_base + pat * 1000 + round(ratio * 100)
      X_miss   <- generate_missing(X_orig, ratio, seed = seed_pat)
      miss_mat <- is.na(X_miss)
      fills    <- prepare_all_fillings(X_miss)
      miss_empty <- matrix(FALSE, n, d)

      for (init_i in seq_len(n_inits)) {
        seed_i <- seed_base + pat * 1000 + round(ratio * 100) * 100 + init_i

        # ── Proposed GMM ────────────────────────────────────────────────────
        tryCatch({
          set.seed(seed_i)
          km_c   <- kmeans(fills$data_em, centers=k, nstart=KM_NSTART, iter.max=100)$centers
          result <- gmm_incomplete(fills$data_em, k, miss_mat, km_c, 500, 1e-4)
          res_all$Proposed[run_idx, ] <- compute_metrics(labels, result$labels)
        }, error = function(e) {
          if (verbose) message(sprintf("  Proposed error: %s", e$message))
        })

        # ── GMM + Mean ──────────────────────────────────────────────────────
        tryCatch({
          set.seed(seed_i)
          km_c   <- kmeans(fills$data_mean, centers=k, nstart=KM_NSTART, iter.max=100)$centers
          result <- gmm_incomplete(fills$data_mean, k, miss_empty, km_c, 500, 1e-4)
          res_all$Mean[run_idx, ] <- compute_metrics(labels, result$labels)
        }, error = function(e) NULL)

        # ── GMM + Zero ──────────────────────────────────────────────────────
        tryCatch({
          set.seed(seed_i)
          km_c   <- kmeans(fills$data_zero, centers=k, nstart=KM_NSTART, iter.max=100)$centers
          result <- gmm_incomplete(fills$data_zero, k, miss_empty, km_c, 500, 1e-4)
          res_all$Zero[run_idx, ] <- compute_metrics(labels, result$labels)
        }, error = function(e) NULL)

        # ── GMM + EM ────────────────────────────────────────────────────────
        tryCatch({
          set.seed(seed_i)
          km_c   <- kmeans(fills$data_em, centers=k, nstart=KM_NSTART, iter.max=100)$centers
          result <- gmm_incomplete(fills$data_em, k, miss_empty, km_c, 500, 1e-4)
          res_all$EM[run_idx, ] <- compute_metrics(labels, result$labels)
        }, error = function(e) NULL)

        run_idx <- run_idx + 1
      }  # end inits

      if (verbose && pat %% 5 == 0)
        cat(sprintf("    pattern %d/%d\n", pat, n_patterns))
    }  # end patterns

    # Tổng hợp per-ratio: two-level (mean/SD over patterns, mỗi pattern = mean inits)
    ratio_summary <- lapply(methods, function(m) {
      mat <- res_all[[m]]
      pat_means <- do.call(rbind, lapply(seq_len(n_patterns), function(p) {
        rows <- mat[((p-1)*n_inits + 1):(p*n_inits), , drop=FALSE]
        rows <- rows[complete.cases(rows), , drop=FALSE]
        if (nrow(rows) == 0) return(rep(NA_real_, 4L))
        colMeans(rows)
      }))
      pat_means <- pat_means[complete.cases(pat_means), , drop=FALSE]
      if (nrow(pat_means) == 0) return(rep(NA_real_, 8L))
      c(ACC_mean=mean(pat_means[,1]), ACC_sd=sd(pat_means[,1]),
        NMI_mean=mean(pat_means[,2]), NMI_sd=sd(pat_means[,2]),
        F_mean  =mean(pat_means[,3]), F_sd  =sd(pat_means[,3]),
        PUR_mean=mean(pat_means[,4]), PUR_sd=sd(pat_means[,4]))
    })
    names(ratio_summary) <- methods
    results_by_ratio[[as.character(ratio)]] <- ratio_summary
  }  # end ratios

  results_by_ratio
}

# ─────────────────────────────────────────────────────────────────────────────
# Tính aggregated metrics (average over all missing ratios)
# Khớp cách tính trong bài báo: "average ACC over 10%-70%"
# ─────────────────────────────────────────────────────────────────────────────
aggregate_results <- function(results_by_ratio, methods, missing_ratios) {
  agg <- lapply(methods, function(m) {
    vals <- sapply(as.character(missing_ratios), function(r) {
      v <- results_by_ratio[[r]][[m]]
      v[c("ACC_mean","ACC_sd","NMI_mean","NMI_sd","F_mean","F_sd","PUR_mean","PUR_sd")]
    })  # 8 × n_ratios
    rowMeans(vals, na.rm = TRUE)
  })
  names(agg) <- methods
  agg
}

# ─────────────────────────────────────────────────────────────────────────────
# In bảng kết quả giống Table 2
# ─────────────────────────────────────────────────────────────────────────────
print_table2_row <- function(dataset_name, agg, methods) {
  cat(sprintf("\n%-15s", dataset_name))
  for (m in methods) {
    v <- agg[[m]]
    cat(sprintf("  %5.1f±%-4.1f", v["ACC_mean"]*100, v["ACC_sd"]*100))
  }
  cat("\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# Main loop: chạy từng dataset
# ─────────────────────────────────────────────────────────────────────────────
all_results   <- list()
methods       <- c("Proposed", "Mean", "Zero", "EM")

cat("╔══════════════════════════════════════════════════════╗\n")
cat("║  GMM with Incomplete Data — R Replication            ║\n")
cat("║  Reproducing Table 2 (Zhang et al., 2021)            ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

for (ds_name in DATASET_NAMES) {
  cat(sprintf("\n▶ Dataset: %s\n", toupper(ds_name)))
  cat(strrep("─", 50), "\n")

  ds <- tryCatch(
    load_dataset(ds_name),
    error = function(e) {
      cat(sprintf("  SKIP: %s\n", e$message)); NULL
    }
  )
  if (is.null(ds)) next

  k    <- length(unique(ds$labels))
  cat(sprintf("  n=%d, d=%d, k=%d\n", nrow(ds$X), ncol(ds$X), k))
  t0   <- proc.time()

  res  <- run_experiment_dataset(
    dataset_name  = ds_name,
    X_orig        = ds$X,
    labels        = ds$labels,
    k             = k,
    verbose       = TRUE
  )
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("  Done in %.1f min\n", elapsed / 60))

  all_results[[ds_name]] <- res
  saveRDS(res, file.path(RESULTS_DIR, sprintf("%s_results.rds", ds_name)))

  # In aggregated
  agg <- aggregate_results(res, methods, MISSING_RATIOS)
  cat(sprintf("\n  Aggregated ACC: Proposed=%.1f%%  Mean=%.1f%%  Zero=%.1f%%  EM=%.1f%%\n",
    agg$Proposed["ACC_mean"]*100, agg$Mean["ACC_mean"]*100,
    agg$Zero["ACC_mean"]*100,     agg$EM["ACC_mean"]*100))
}

# ── In Table 2 đầy đủ ────────────────────────────────────────────────────────
cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("  AGGREGATED ACC (%) — Reproducing Table 2\n")
cat("══════════════════════════════════════════════════════════════\n")
cat(sprintf("%-15s  %15s  %10s  %10s  %10s\n",
  "Dataset", "Proposed", "Mean", "Zero", "EM"))
cat(strrep("─", 65), "\n")

expected <- list(
  iris  = c(84.4, 61.3, 67.3, 76.0),
  seeds = c(79.3, 56.6, 53.9, 64.7),
  wine  = c(87.0, 58.0, 74.8, 81.8)
)

for (ds_name in names(all_results)) {
  agg <- aggregate_results(all_results[[ds_name]], methods, MISSING_RATIOS)
  exp <- expected[[ds_name]]

  cat(sprintf("%-15s  %6.1f±%-6.1f  %10.1f  %10.1f  %10.1f\n",
    ds_name,
    agg$Proposed["ACC_mean"]*100, agg$Proposed["ACC_sd"]*100,
    agg$Mean["ACC_mean"]*100,
    agg$Zero["ACC_mean"]*100,
    agg$EM["ACC_mean"]*100))

  if (!is.null(exp)) {
    diff_prop <- abs(agg$Proposed["ACC_mean"]*100 - exp[1])
    status <- if (diff_prop <= 3) "PASS ✓" else sprintf("DIFF=%.1f%%", diff_prop)
    cat(sprintf("  Expected(Paper): %.1f  →  %s\n", exp[1], status))
  }
}

# ── Export CSV ────────────────────────────────────────────────────────────────
table_rows <- list()
for (ds_name in names(all_results)) {
  agg <- aggregate_results(all_results[[ds_name]], methods, MISSING_RATIOS)
  for (m in methods) {
    v <- agg[[m]]
    table_rows[[length(table_rows)+1]] <- data.frame(
      dataset  = ds_name,
      method   = m,
      ACC_mean = round(v["ACC_mean"]*100, 2),
      ACC_sd   = round(v["ACC_sd"]*100, 2),
      NMI_mean = round(v["NMI_mean"]*100, 2),
      NMI_sd   = round(v["NMI_sd"]*100, 2),
      F_mean   = round(v["F_mean"]*100, 2),
      F_sd     = round(v["F_sd"]*100, 2),
      PUR_mean = round(v["PUR_mean"]*100, 2),
      PUR_sd   = round(v["PUR_sd"]*100, 2),
      stringsAsFactors = FALSE
    )
  }
}
if (length(table_rows) > 0) {
  table_df <- do.call(rbind, table_rows)
  csv_path <- file.path(RESULTS_DIR, "table2_replication.csv")
  write.csv(table_df, csv_path, row.names = FALSE)
  cat(sprintf("\nTable saved to: %s\n", csv_path))
}
