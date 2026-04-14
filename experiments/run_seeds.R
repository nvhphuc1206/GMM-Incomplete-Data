# run_seeds.R
# Thực nghiệm trên Seeds dataset (210×7, 3 classes)
# Kết quả kỳ vọng (Table 2 "Ours"): ACC≈79.3%, NMI≈55.1%, F≈80.3%, PUR≈79.9%
#
# Seeds dataset: https://archive.ics.uci.edu/ml/datasets/seeds
# Lưu vào: data/seeds_dataset.txt (tab-separated, 7 features + 1 label)

R_DIR <- file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "R")
if (!file.exists(file.path(R_DIR, "gmm_incomplete.R"))) {
  R_DIR <- file.path(getwd(), "..", "R")
}

source(file.path(R_DIR, "data_utils.R"))
source(file.path(R_DIR, "gmm_incomplete.R"))
source(file.path(R_DIR, "regem.R"))
source(file.path(R_DIR, "imputation_baseline.R"))
source(file.path(R_DIR, "dk_kmeans.R"))
source(file.path(R_DIR, "evaluation.R"))

# ── Load Seeds ────────────────────────────────────────────────────────────────
cat("Loading Seeds dataset...\n")
# Thử load từ package mlbench
if (requireNamespace("mlbench", quietly = TRUE)) {
  # mlbench không có seeds, load từ URL
}

# Download nếu cần
data_dir <- file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "data")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
seeds_path <- file.path(data_dir, "seeds_dataset.txt")
if (!file.exists(seeds_path)) {
  cat("Downloading Seeds dataset from UCI...\n")
  tryCatch(
    download.file(
      "https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",
      seeds_path
    ),
    error = function(e) stop("Cannot download Seeds. Please download manually to data/seeds_dataset.txt")
  )
}

df     <- read.table(seeds_path, header = FALSE)
X_orig <- as.matrix(df[, 1:7])
labels <- as.integer(df[, 8])
k      <- length(unique(labels))
n      <- nrow(X_orig); d <- ncol(X_orig)
cat(sprintf("  n=%d, d=%d, k=%d\n", n, d, k))

# ── Parameters ───────────────────────────────────────────────────────────────
MISSING_RATIOS <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
N_INITS        <- 50
N_PATTERNS     <- 20    # bài báo: 20 random patterns
SEED_BASE      <- 42
KM_NSTART      <- 3L    # K-means restarts để giảm SD
methods        <- c("Proposed", "Mean", "Zero", "EM", "DK_Mean", "DK_Zero", "DK_EM")
metrics        <- c("ACC", "NMI", "Fscore", "PUR")

# ── Fixed patterns ─────────────────────────────────────────────────────────
RESULTS_DIR <- file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "results")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)
PATTERNS_FILE <- file.path(RESULTS_DIR, "seeds_patterns.rds")

all_patterns <- local({
  if (file.exists(PATTERNS_FILE)) {
    saved <- tryCatch(readRDS(PATTERNS_FILE), error = function(e) NULL)
    if (!is.null(saved) &&
        isTRUE(saved$meta$N_PATTERNS == N_PATTERNS) &&
        isTRUE(saved$meta$n          == n)) {
      cat(sprintf("  Loaded fixed patterns from %s\n", PATTERNS_FILE))
      return(saved$patterns)
    }
  }
  cat("  Generating and saving fixed patterns...\n")
  pats <- lapply(MISSING_RATIOS, function(ratio) {
    lapply(seq_len(N_PATTERNS), function(pat) {
      seed_pat <- SEED_BASE + pat * 1000L + round(ratio * 100)
      X_miss   <- generate_missing(X_orig, ratio, seed = seed_pat)
      is.na(X_miss)
    })
  })
  names(pats) <- as.character(MISSING_RATIOS)
  saveRDS(list(meta     = list(N_PATTERNS = N_PATTERNS, n = n, d = d,
                               MISSING_RATIOS = MISSING_RATIOS),
               patterns = pats),
          PATTERNS_FILE)
  cat(sprintf("  Patterns saved: %s\n", PATTERNS_FILE))
  pats
})

results_by_ratio <- list()

for (ratio in MISSING_RATIOS) {
  cat(sprintf("\n--- Missing ratio: %.0f%% ---\n", ratio * 100))

  res_all <- lapply(methods, function(m) matrix(NA, N_PATTERNS * N_INITS, 4,
    dimnames = list(NULL, metrics)))
  names(res_all) <- methods

  run_idx <- 1

  for (pat in seq_len(N_PATTERNS)) {
    # Use fixed pattern (cải thiện 2)
    miss_mat <- all_patterns[[as.character(ratio)]][[pat]]
    X_miss   <- X_orig
    X_miss[miss_mat] <- NA_real_
    fills    <- prepare_all_fillings(X_miss)

    for (init_i in seq_len(N_INITS)) {
      seed_i <- SEED_BASE + pat * 1000 + round(ratio * 100) * 100 + init_i

      # Proposed GMM
      tryCatch({
        set.seed(seed_i)
        km_c   <- kmeans(fills$data_em, centers=k, nstart=KM_NSTART, iter.max=100)$centers
        result <- gmm_incomplete(fills$data_em, k, miss_mat, km_c, 500, 1e-4)
        res_all$Proposed[run_idx, ] <- compute_metrics(labels, result$labels)
      }, error = function(e) NULL)

      # GMM + Mean
      tryCatch({
        set.seed(seed_i)
        km_c   <- kmeans(fills$data_mean, centers=k, nstart=KM_NSTART, iter.max=100)$centers
        result <- gmm_incomplete(fills$data_mean, k, matrix(FALSE,n,d), km_c, 500, 1e-4)
        res_all$Mean[run_idx, ] <- compute_metrics(labels, result$labels)
      }, error = function(e) NULL)

      # GMM + Zero
      tryCatch({
        set.seed(seed_i)
        km_c   <- kmeans(fills$data_zero, centers=k, nstart=KM_NSTART, iter.max=100)$centers
        result <- gmm_incomplete(fills$data_zero, k, matrix(FALSE,n,d), km_c, 500, 1e-4)
        res_all$Zero[run_idx, ] <- compute_metrics(labels, result$labels)
      }, error = function(e) NULL)

      # GMM + EM
      tryCatch({
        set.seed(seed_i)
        km_c   <- kmeans(fills$data_em, centers=k, nstart=KM_NSTART, iter.max=100)$centers
        result <- gmm_incomplete(fills$data_em, k, matrix(FALSE,n,d), km_c, 500, 1e-4)
        res_all$EM[run_idx, ] <- compute_metrics(labels, result$labels)
      }, error = function(e) NULL)

      # DK + Mean
      tryCatch({
        set.seed(seed_i)
        dk_lab <- dk_kmeans(fills$data_mean, k, miss_mat)
        res_all$DK_Mean[run_idx, ] <- compute_metrics(labels, dk_lab)
      }, error = function(e) NULL)

      # DK + Zero
      tryCatch({
        set.seed(seed_i)
        dk_lab <- dk_kmeans(fills$data_zero, k, miss_mat)
        res_all$DK_Zero[run_idx, ] <- compute_metrics(labels, dk_lab)
      }, error = function(e) NULL)

      # DK + EM
      tryCatch({
        set.seed(seed_i)
        dk_lab <- dk_kmeans(fills$data_em, k, miss_mat)
        res_all$DK_EM[run_idx, ] <- compute_metrics(labels, dk_lab)
      }, error = function(e) NULL)

      run_idx <- run_idx + 1
    }

    cat(sprintf("  pattern %d/%d done\r", pat, N_PATTERNS))
  }
  cat("\n")

  # Two-level aggregation: mean/SD over N_PATTERNS (mỗi pattern = mean của N_INITS)
  ratio_summary <- lapply(methods, function(m) {
    mat <- res_all[[m]]
    pat_means <- do.call(rbind, lapply(seq_len(N_PATTERNS), function(p) {
      rows <- mat[((p-1)*N_INITS + 1):(p*N_INITS), , drop=FALSE]
      rows <- rows[complete.cases(rows), , drop=FALSE]
      if (nrow(rows) == 0) return(rep(NA_real_, 4L))
      colMeans(rows)
    }))
    pat_means <- pat_means[complete.cases(pat_means), , drop=FALSE]
    c(ACC_mean=mean(pat_means[,1]), ACC_sd=sd(pat_means[,1]),
      NMI_mean=mean(pat_means[,2]), NMI_sd=sd(pat_means[,2]),
      F_mean  =mean(pat_means[,3]), F_sd  =sd(pat_means[,3]),
      PUR_mean=mean(pat_means[,4]), PUR_sd=sd(pat_means[,4]))
  })
  names(ratio_summary) <- methods
  results_by_ratio[[as.character(ratio)]] <- ratio_summary

  prop <- ratio_summary$Proposed
  cat(sprintf("  Proposed: ACC=%.1f±%.1f%%, NMI=%.1f±%.1f%%\n",
    prop["ACC_mean"]*100, prop["ACC_sd"]*100,
    prop["NMI_mean"]*100, prop["NMI_sd"]*100))
}

# ── Aggregated summary ────────────────────────────────────────────────────────
cat("\n\n══ AGGREGATED RESULTS — Seeds ══\n")
cat(sprintf("%-12s  %12s  %12s  %12s  %12s\n",
  "Method", "ACC(%)", "NMI(%)", "F(%)", "PUR(%)"))
cat(strrep("-", 68), "\n")

for (m in methods) {
  vals <- sapply(MISSING_RATIOS, function(r) {
    v <- results_by_ratio[[as.character(r)]][[m]]
    c(v["ACC_mean"], v["NMI_mean"], v["F_mean"], v["PUR_mean"])
  })
  avg <- rowMeans(vals) * 100
  cat(sprintf("%-12s  %12.1f  %12.1f  %12.1f  %12.1f\n",
    m, avg[1], avg[2], avg[3], avg[4]))
}
cat("\nExpected (Table 2, Seeds, ACC%): Mean=56.6  Zero=53.9  EM=64.7  DK+Mean≈?  DK+Zero≈?  DK+EM≈?  Ours=79.3\n")

# ── Lưu kết quả ──────────────────────────────────────────────────────────────
RESULTS_DIR <- file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "results")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)
saveRDS(results_by_ratio,
  file.path(RESULTS_DIR, "seeds_results_by_ratio.rds"))
cat(sprintf("\nSaved to %s/seeds_results_by_ratio.rds\n", RESULTS_DIR))
