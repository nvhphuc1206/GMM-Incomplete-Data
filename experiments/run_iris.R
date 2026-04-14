# run_iris.R
# Thực nghiệm trên Iris dataset — Proposed GMM + 3 baselines
# Kết quả kỳ vọng Table 2 "Ours" (trung bình 10-70%): ACC≈84.4%, NMI≈66.3%

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION (chỉnh tại đây) ════════════════════════════════════════════
QUICK_MODE     <- FALSE   # TRUE = 5 pat × 10 inits (test ~2 phút)
USE_PARALLEL   <- TRUE    # FALSE = sequential (dễ debug hơn, progress đẹp hơn)
N_CORES        <- max(1L, parallel::detectCores() - 1L)

MISSING_RATIOS <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
PAPER_RATIOS   <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

N_INITS    <- if (QUICK_MODE) 10L else 50L
N_PATTERNS <- if (QUICK_MODE)  5L else 20L   # bài báo: 20 random patterns
SEED_BASE  <- 42L
KM_NSTART  <- 3L   # K-means restarts (giảm SD; set 1 để khớp MATLAB gốc)
# ═════════════════════════════════════════════════════════════════════════════

# ── R_DIR ─────────────────────────────────────────────────────────────────────
R_DIR <- tryCatch({
  ofile <- sys.frame(1)$ofile %||% NULL
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "R")
  else                  file.path(getwd(), "..", "R")
}, error = function(e) file.path(getwd(), "..", "R"))
if (!file.exists(file.path(R_DIR, "gmm_incomplete.R")))
  R_DIR <- file.path(getwd(), "..", "R")
if (!file.exists(file.path(R_DIR, "gmm_incomplete.R")))
  stop("Không tìm thấy R/. Hãy setwd() vào experiments/ trước.")

source(file.path(R_DIR, "data_utils.R"))
source(file.path(R_DIR, "gmm_incomplete.R"))
source(file.path(R_DIR, "regem.R"))            # regem_r() — phải load trước imputation_baseline
source(file.path(R_DIR, "imputation_baseline.R"))
source(file.path(R_DIR, "evaluation.R"))

# ── Results / log dirs ────────────────────────────────────────────────────────
RESULTS_DIR <- tryCatch({
  ofile <- sys.frame(1)$ofile %||% NULL
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results")
  else                  file.path(getwd(), "..", "results")
}, error = function(e) file.path(getwd(), "..", "results"))
LOG_DIR <- file.path(RESULTS_DIR, "logs")
for (d_ in c(RESULTS_DIR, LOG_DIR)) if (!dir.exists(d_)) dir.create(d_, recursive = TRUE)

RUN_TAG  <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE <- file.path(LOG_DIR, sprintf("iris_%s.log", RUN_TAG))

# ── Fixed patterns (cải thiện 2: generate once, reuse → giảm SD) ─────────────
# Mục đích: khớp cách bài báo dùng 20 patterns cố định cho mỗi ratio.
# File patterns được tạo lần đầu (theo seed), tái dùng ở các lần chạy sau.
PATTERNS_FILE <- file.path(RESULTS_DIR, "iris_patterns.rds")

# ── Log infrastructure ────────────────────────────────────────────────────────
.log <- function(..., level = "INFO", console = TRUE) {
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- sprintf("[%s] [%-5s] %s", ts, level, paste0(...))
  if (console) { cat(msg, "\n"); flush.console() }
  cat(msg, "\n", file = LOG_FILE, append = TRUE)
  invisible(msg)
}

.fmt_elapsed <- function(sec) {
  if (sec < 60)  return(sprintf("%.1fs", sec))
  if (sec < 3600) return(sprintf("%.1f min", sec / 60))
  sprintf("%.1f h", sec / 3600)
}

.fmt_eta <- function(elapsed_sec, done, total) {
  if (done == 0) return("--")
  remaining <- elapsed_sec / done * (total - done)
  .fmt_elapsed(remaining)
}

# ── Self-test ─────────────────────────────────────────────────────────────────
tryCatch({
  m <- compute_metrics(c(1,1,2,2,3,3), c(2,2,1,1,3,3))
  stopifnot(abs(m["ACC"] - 1.0) < 1e-9)
}, error = function(e) stop("compute_metrics() lỗi: ", e$message))

# ── Load data ─────────────────────────────────────────────────────────────────
ds     <- load_dataset("iris")
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels)); n <- nrow(X_orig); d <- ncol(X_orig)

# ── Load or generate fixed patterns ──────────────────────────────────────────
.load_or_gen_patterns <- function(patterns_file, MISSING_RATIOS, N_PATTERNS,
                                  X_orig, SEED_BASE, n, d) {
  if (file.exists(patterns_file)) {
    saved <- tryCatch(readRDS(patterns_file), error = function(e) NULL)
    if (!is.null(saved) &&
        isTRUE(saved$meta$N_PATTERNS    == N_PATTERNS)  &&
        isTRUE(length(saved$meta$MISSING_RATIOS) == length(MISSING_RATIOS)) &&
        isTRUE(saved$meta$n             == n)            &&
        isTRUE(saved$meta$d             == d)) {
      return(list(patterns = saved$patterns, loaded = TRUE))
    }
  }
  # Generate patterns using same seeds as experiment loop
  pats <- lapply(MISSING_RATIOS, function(ratio) {
    lapply(seq_len(N_PATTERNS), function(pat) {
      seed_pat <- SEED_BASE + pat * 1000L + round(ratio * 100)
      X_miss   <- generate_missing(X_orig, ratio, seed = seed_pat)
      is.na(X_miss)   # logical n×d miss_mat
    })
  })
  names(pats) <- as.character(MISSING_RATIOS)
  saveRDS(list(
    meta     = list(N_PATTERNS    = N_PATTERNS,
                    MISSING_RATIOS = MISSING_RATIOS,
                    n = n, d = d, SEED_BASE = SEED_BASE),
    patterns = pats
  ), patterns_file)
  list(patterns = pats, loaded = FALSE)
}

pat_result <- .load_or_gen_patterns(
  PATTERNS_FILE, MISSING_RATIOS, N_PATTERNS, X_orig, SEED_BASE, n, d)
all_patterns <- pat_result$patterns

# Ước tính thời gian
secs_per_run  <- 0.05   # ~50ms per GMM run (Iris nhỏ)
total_runs    <- length(MISSING_RATIOS) * N_PATTERNS * N_INITS
est_seq_min   <- total_runs * secs_per_run * 4 / 60   # 4 methods
n_workers     <- if (USE_PARALLEL) min(N_CORES, length(MISSING_RATIOS)) else 1L
est_par_min   <- est_seq_min / n_workers

.log("══════════════════════════════════════════════════════")
.log("  GMM Incomplete Data — Iris Experiment")
.log("══════════════════════════════════════════════════════")
.log(sprintf("Dataset   : Iris (n=%d, d=%d, k=%d)", n, d, k))
.log(sprintf("Mode      : %s | %d patterns × %d inits × %d ratios = %s runs",
  if (QUICK_MODE) "QUICK" else "FULL",
  N_PATTERNS, N_INITS, length(MISSING_RATIOS),
  format(total_runs * 4, big.mark = ",")))
.log(sprintf("Parallel  : %s (%d workers)",
  if (USE_PARALLEL) "YES" else "NO", n_workers))
.log(sprintf("KM_NSTART : %d", KM_NSTART))
.log(sprintf("EM fill   : regem (Schneider 2001, GCV ridge, maxit=10)"))
.log(sprintf("Patterns  : %s (%d×%d)",
  if (pat_result$loaded) "loaded from file" else "generated + saved",
  N_PATTERNS, length(MISSING_RATIOS)))
.log(sprintf("Est. time : ~%.0f min (seq) / ~%.0f min (par)",
  ceiling(est_seq_min), ceiling(est_par_min)))
.log(sprintf("Log file  : %s", LOG_FILE))
.log("──────────────────────────────────────────────────────")

# ── Per-pattern worker function ───────────────────────────────────────────────
# Trả về: list với 4 method, mỗi method là ma trận N_INITS × 4 metrics
.run_one_pattern <- function(args) {
  # args = list(pat, ratio, X_orig, labels, k, n, d, N_INITS, SEED_BASE, KM_NSTART)
  pat      <- args$pat
  ratio    <- args$ratio
  X_orig   <- args$X_orig
  labels   <- args$labels
  k        <- args$k; n <- args$n; d <- args$d
  N_INITS  <- args$N_INITS
  SEED_BASE <- args$SEED_BASE
  KM_NSTART <- args$KM_NSTART

  methods    <- c("Proposed", "Mean", "Zero", "EM")
  metrics    <- c("ACC", "NMI", "Fscore", "PUR")
  miss_empty <- matrix(FALSE, n, d)

  res_pat <- lapply(methods, function(m)
    matrix(NA_real_, N_INITS, 4L, dimnames = list(NULL, metrics)))
  names(res_pat) <- methods

  # Use fixed pattern if provided (cải thiện 2: fixed patterns)
  if (!is.null(args$miss_mat)) {
    miss_mat <- args$miss_mat
    X_miss   <- X_orig
    X_miss[miss_mat] <- NA_real_
  } else {
    seed_pat <- SEED_BASE + pat * 1000L + round(ratio * 100)
    X_miss   <- generate_missing(X_orig, ratio, seed = seed_pat)
    miss_mat <- is.na(X_miss)
  }
  fills    <- prepare_all_fillings(X_miss)

  for (init_i in seq_len(N_INITS)) {
    seed_i <- SEED_BASE + pat * 1000L + round(ratio * 100) * 100L + init_i

    # Proposed GMM
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_em, centers=k, nstart=KM_NSTART, iter.max=100)$centers
      result <- gmm_incomplete(fills$data_em, k, miss_mat, km_c, 500, 1e-4)
      res_pat$Proposed[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # GMM + Mean
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_mean, centers=k, nstart=KM_NSTART, iter.max=100)$centers
      result <- gmm_incomplete(fills$data_mean, k, miss_empty, km_c, 500, 1e-4)
      res_pat$Mean[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # GMM + Zero
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_zero, centers=k, nstart=KM_NSTART, iter.max=100)$centers
      result <- gmm_incomplete(fills$data_zero, k, miss_empty, km_c, 500, 1e-4)
      res_pat$Zero[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # GMM + EM
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_em, centers=k, nstart=KM_NSTART, iter.max=100)$centers
      result <- gmm_incomplete(fills$data_em, k, miss_empty, km_c, 500, 1e-4)
      res_pat$EM[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)
  }
  res_pat
}

# ── Cluster setup (nếu parallel) ─────────────────────────────────────────────
cl <- NULL
if (USE_PARALLEL && N_CORES > 1L) {
  n_workers <- min(N_CORES, N_PATTERNS)
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterExport(cl, c("R_DIR", "all_patterns"))
  parallel::clusterEvalQ(cl, {
    source(file.path(R_DIR, "data_utils.R"))
    source(file.path(R_DIR, "gmm_incomplete.R"))
    source(file.path(R_DIR, "regem.R"))
    source(file.path(R_DIR, "imputation_baseline.R"))
    source(file.path(R_DIR, "evaluation.R"))
    NULL
  })
  parallel::clusterExport(cl, ".run_one_pattern")
  .log(sprintf("Cluster ready: %d workers", n_workers))
}

# ── Main experiment loop ──────────────────────────────────────────────────────
methods          <- c("Proposed", "Mean", "Zero", "EM")
results_by_ratio <- list()
t_total_start    <- proc.time()["elapsed"]
ratio_elapsed    <- numeric(length(MISSING_RATIOS))

for (ri in seq_along(MISSING_RATIOS)) {
  ratio    <- MISSING_RATIOS[ri]
  pct      <- round(ratio * 100)
  t_ratio0 <- proc.time()["elapsed"]

  elapsed_so_far <- t_ratio0 - t_total_start
  eta_str <- .fmt_eta(elapsed_so_far, ri - 1L, length(MISSING_RATIOS))

  .log(sprintf("── Ratio %3d%% [%d/%d] | elapsed %s | ETA %s",
    pct, ri, length(MISSING_RATIOS),
    .fmt_elapsed(elapsed_so_far), eta_str))

  # Build args list for each pattern (pass fixed miss_mat if available)
  ratio_str    <- as.character(ratio)
  pattern_args <- lapply(seq_len(N_PATTERNS), function(pat) list(
    pat      = pat,    ratio  = ratio,
    X_orig   = X_orig, labels = labels,
    k = k, n = n, d = d,
    N_INITS   = N_INITS, SEED_BASE = SEED_BASE, KM_NSTART = KM_NSTART,
    miss_mat  = if (!is.null(all_patterns[[ratio_str]])) all_patterns[[ratio_str]][[pat]] else NULL
  ))

  # Run (parallel or sequential)
  if (!is.null(cl)) {
    # ── Parallel: parLapply over patterns ───────────────────────────────────
    # Export pattern_args nào đó không cần thiết vì đã truyền qua args list
    pat_results <- parallel::parLapply(cl, pattern_args, .run_one_pattern)
    cat(sprintf("  [%s] Ratio %3d%% — %d patterns done (parallel)\n",
      format(Sys.time(), "%H:%M:%S"), pct, N_PATTERNS))
    flush.console()
  } else {
    # ── Sequential: vòng lặp pattern với progress \r ─────────────────────
    pat_results <- vector("list", N_PATTERNS)
    for (pat in seq_len(N_PATTERNS)) {
      pat_results[[pat]] <- .run_one_pattern(pattern_args[[pat]])

      # Running ACC của Proposed
      done_rows <- do.call(rbind, lapply(
        pat_results[seq_len(pat)][!sapply(pat_results[seq_len(pat)], is.null)],
        function(x) x$Proposed))
      done_rows <- done_rows[complete.cases(done_rows), , drop = FALSE]
      run_acc <- if (nrow(done_rows) > 0) mean(done_rows[, "ACC"]) * 100 else NA

      cat(sprintf("\r  Pattern %2d/%-2d | inits %d | Proposed ACC so far: %s%%   ",
        pat, N_PATTERNS, N_INITS,
        if (is.na(run_acc)) " -- " else sprintf("%5.1f", run_acc)))
      flush.console()
    }
    cat("\n")
  }

  # ── Aggregate: two-level (khớp MATLAB demo.m) ────────────────────────────
  # Bước 1: với mỗi pattern, lấy mean over N_INITS → per-pattern mean
  # Bước 2: mean ± SD over N_PATTERNS pattern-means
  # (Paper báo SD = std across patterns, không phải std across 1000 runs)
  ratio_summary <- lapply(methods, function(m) {
    pat_means <- do.call(rbind, lapply(pat_results, function(p) {
      rows <- p[[m]]
      rows <- rows[complete.cases(rows), , drop = FALSE]
      if (nrow(rows) == 0) return(rep(NA_real_, 4L))
      colMeans(rows)
    }))
    colnames(pat_means) <- c("ACC", "NMI", "Fscore", "PUR")
    pat_means <- pat_means[complete.cases(pat_means), , drop = FALSE]
    n_valid   <- nrow(pat_means)
    if (n_valid == 0) {
      .log(sprintf("  WARNING: method=%s ratio=%d%% — 0 valid patterns!", m, pct), level = "WARN")
      return(rep(NA_real_, 9L))
    }
    c(ACC_mean = mean(pat_means[,1]), ACC_sd = sd(pat_means[,1]),
      NMI_mean = mean(pat_means[,2]), NMI_sd = sd(pat_means[,2]),
      F_mean   = mean(pat_means[,3]), F_sd   = sd(pat_means[,3]),
      PUR_mean = mean(pat_means[,4]), PUR_sd = sd(pat_means[,4]),
      n_valid  = n_valid)
  })
  names(ratio_summary) <- methods

  results_by_ratio[[as.character(ratio)]] <- ratio_summary

  # ── Print + log ratio result ──────────────────────────────────────────────
  ratio_elapsed[ri] <- proc.time()["elapsed"] - t_ratio0
  v   <- ratio_summary$Proposed
  msg <- sprintf(
    "  Proposed: ACC=%5.1f±%4.1f%% | NMI=%5.1f±%4.1f%% | F=%5.1f±%4.1f%% | PUR=%5.1f±%4.1f%% | n=%d | %.1fs",
    v["ACC_mean"]*100, v["ACC_sd"]*100,
    v["NMI_mean"]*100, v["NMI_sd"]*100,
    v["F_mean"]*100,   v["F_sd"]*100,
    v["PUR_mean"]*100, v["PUR_sd"]*100,
    v["n_valid"], ratio_elapsed[ri])
  .log(msg)

  # Log tất cả methods
  for (m in methods[-1]) {
    vv <- ratio_summary[[m]]
    .log(sprintf("  %-12s ACC=%5.1f±%4.1f%%  NMI=%5.1f±%4.1f%%",
      paste0(m, ":"), vv["ACC_mean"]*100, vv["ACC_sd"]*100,
      vv["NMI_mean"]*100, vv["NMI_sd"]*100), console = FALSE)
  }
}

# ── Dọn cluster ───────────────────────────────────────────────────────────────
if (!is.null(cl)) parallel::stopCluster(cl)

# ── Tổng thời gian ────────────────────────────────────────────────────────────
total_elapsed <- proc.time()["elapsed"] - t_total_start
.log("──────────────────────────────────────────────────────")
.log(sprintf("Total elapsed: %s (avg %.1fs/ratio)",
  .fmt_elapsed(total_elapsed), mean(ratio_elapsed)))

# ── Hàm aggregate ─────────────────────────────────────────────────────────────
.agg <- function(results_list, ratios, methods) {
  lapply(methods, function(m) {
    vals <- sapply(as.character(ratios), function(r) {
      results_list[[r]][[m]][c("ACC_mean","ACC_sd","NMI_mean","NMI_sd",
                                "F_mean","F_sd","PUR_mean","PUR_sd")]
    })
    rowMeans(vals, na.rm = TRUE)
  }) |> setNames(methods)
}

# ── Print per-ratio summary table ─────────────────────────────────────────────
.log("\n══ PER-RATIO RESULTS (Proposed) ══")
hdr <- sprintf("%-6s  %-14s  %-14s  %-14s  %-14s  %s",
  "Ratio", "ACC (%)", "NMI (%)", "F-score (%)", "PUR (%)", "Time")
.log(hdr); .log(strrep("-", nchar(hdr)))
for (ri in seq_along(MISSING_RATIOS)) {
  r <- MISSING_RATIOS[ri]
  v <- results_by_ratio[[as.character(r)]]$Proposed
  .log(sprintf("%-6.0f  %5.1f ± %-5.1f  %5.1f ± %-5.1f  %5.1f ± %-5.1f  %5.1f ± %-5.1f  %s",
    r*100,
    v["ACC_mean"]*100, v["ACC_sd"]*100,
    v["NMI_mean"]*100, v["NMI_sd"]*100,
    v["F_mean"]*100,   v["F_sd"]*100,
    v["PUR_mean"]*100, v["PUR_sd"]*100,
    .fmt_elapsed(ratio_elapsed[ri])))
}

# ── So sánh Table 2 (10-70%) ──────────────────────────────────────────────────
.log("\n══ SO SÁNH TABLE 2 — Trung bình 10%-70% ══")
.log(sprintf("%-12s  %13s  %13s  %13s  %13s",
  "Method", "ACC (%)", "NMI (%)", "F-score (%)", "PUR (%)"))
.log(strrep("-", 60))

agg_paper <- .agg(results_by_ratio, PAPER_RATIOS, methods)
for (m in methods) {
  v <- agg_paper[[m]]
  .log(sprintf("%-12s  %5.1f ± %-4.1f  %5.1f ± %-4.1f  %5.1f ± %-4.1f  %5.1f ± %-4.1f",
    m,
    v["ACC_mean"]*100, v["ACC_sd"]*100,
    v["NMI_mean"]*100, v["NMI_sd"]*100,
    v["F_mean"]*100,   v["F_sd"]*100,
    v["PUR_mean"]*100, v["PUR_sd"]*100))
}
.log("Expected (Table 2 'Ours'): ACC≈84.4%, NMI≈66.3%, F≈84.8%, PUR≈84.6%")
diff_acc <- abs(agg_paper$Proposed["ACC_mean"]*100 - 84.4)
.log(sprintf("Proposed ACC diff vs paper: %.1f pp  %s",
  diff_acc, if (diff_acc <= 3) "[PASS]" else "[> 3pp — kiểm tra lại]"))

# ── Ratio=0 riêng ─────────────────────────────────────────────────────────────
if ("0" %in% names(results_by_ratio)) {
  .log("\n── Ratio=0% (complete data baseline) ──")
  for (m in methods) {
    v <- results_by_ratio[["0"]][[m]]
    .log(sprintf("  %-12s: ACC=%5.1f ± %4.1f%%", m, v["ACC_mean"]*100, v["ACC_sd"]*100))
  }
}

# ── Lưu kết quả ──────────────────────────────────────────────────────────────
saveRDS(results_by_ratio, file.path(RESULTS_DIR, "iris_results.rds"))

# TSV per-ratio (long format)
rows <- list()
for (r in MISSING_RATIOS) {
  for (m in methods) {
    v <- results_by_ratio[[as.character(r)]][[m]]
    rows[[length(rows)+1]] <- data.frame(
      dataset = "iris", method = m, missing_pct = round(r*100),
      ACC_mean = round(v["ACC_mean"]*100, 2), ACC_sd = round(v["ACC_sd"]*100, 2),
      NMI_mean = round(v["NMI_mean"]*100, 2), NMI_sd = round(v["NMI_sd"]*100, 2),
      F_mean   = round(v["F_mean"]*100, 2),   F_sd   = round(v["F_sd"]*100, 2),
      PUR_mean = round(v["PUR_mean"]*100, 2), PUR_sd = round(v["PUR_sd"]*100, 2),
      stringsAsFactors = FALSE)
  }
}
df_long <- do.call(rbind, rows)
write.table(df_long, file.path(RESULTS_DIR, "iris_per_ratio.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE)

# TSV Table 2 comparison (10-70%)
rows_agg <- lapply(methods, function(m) {
  v <- agg_paper[[m]]
  data.frame(dataset="iris", method=m, ratios="10-70%",
    ACC_mean=round(v["ACC_mean"]*100,2), ACC_sd=round(v["ACC_sd"]*100,2),
    NMI_mean=round(v["NMI_mean"]*100,2), NMI_sd=round(v["NMI_sd"]*100,2),
    F_mean=round(v["F_mean"]*100,2),     F_sd=round(v["F_sd"]*100,2),
    PUR_mean=round(v["PUR_mean"]*100,2), PUR_sd=round(v["PUR_sd"]*100,2),
    stringsAsFactors = FALSE)
})
write.table(do.call(rbind, rows_agg), file.path(RESULTS_DIR, "iris_table2.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE)

.log("──────────────────────────────────────────────────────")
.log(sprintf("Saved: %s/iris_per_ratio.tsv", RESULTS_DIR))
.log(sprintf("Saved: %s/iris_table2.tsv",    RESULTS_DIR))
.log(sprintf("Pats : %s",                    PATTERNS_FILE))
.log(sprintf("Log  : %s", LOG_FILE))
.log(sprintf("DONE. Total time: %s", .fmt_elapsed(total_elapsed)))
