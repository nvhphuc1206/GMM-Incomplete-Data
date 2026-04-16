# run_electricalgrid.R
# Thực nghiệm trên Electrical Grid Stability dataset (10000×13, 2 classes)
# Ước tính thời gian: ~4-8 giờ (sequential) / ~1-2 giờ (parallel)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Electrical+Grid+Stability+Simulated+Data
# Lưu vào : data/Data_for_UCI_named.csv (header = TRUE, 13 features + label cột cuối)
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_electricalgrid.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION ════════════════════════════════════════════════════════════
QUICK_MODE   <- FALSE
USE_PARALLEL <- TRUE    # Bắt buộc TRUE cho dataset rất lớn
N_CORES      <- max(1L, parallel::detectCores() - 1L)
# ═════════════════════════════════════════════════════════════════════════════

# ── Resolve R/ directory ──────────────────────────────────────────────────────
R_DIR <- tryCatch({
  ofile <- sys.frame(1)$ofile %||% NULL
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "R")
  else                  file.path(getwd(), "..", "R")
}, error = function(e) file.path(getwd(), "..", "R"))
if (!file.exists(file.path(R_DIR, "gmm_incomplete.R")))
  R_DIR <- file.path(getwd(), "..", "R")
if (!file.exists(file.path(R_DIR, "gmm_incomplete.R")))
  stop("Không tìm thấy R/. Hãy setwd() vào experiments/ trước.")

# ── Source all modules ────────────────────────────────────────────────────────
source(file.path(R_DIR, "data_utils.R"))
source(file.path(R_DIR, "gmm_incomplete.R"))
source(file.path(R_DIR, "regem.R"))
source(file.path(R_DIR, "imputation_baseline.R"))
source(file.path(R_DIR, "dk_kmeans.R"))
source(file.path(R_DIR, "evaluation.R"))
source(file.path(R_DIR, "experiment_runner.R"))

# ── Results directory ─────────────────────────────────────────────────────────
RESULTS_DIR <- tryCatch({
  ofile <- sys.frame(1)$ofile %||% NULL
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "electricalgrid")
  else                  file.path(getwd(), "..", "results", "electricalgrid")
}, error = function(e) file.path(getwd(), "..", "results", "electricalgrid"))

# ── Load ElectricalGrid dataset ───────────────────────────────────────────────
# Thử load qua load_dataset(); nếu không có file, download từ UCI
ds <- tryCatch(
  load_dataset("electricalgrid"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    local_path <- file.path(data_dir, "Data_for_UCI_named.csv")
    if (!file.exists(local_path)) {
      cat("Downloading ElectricalGrid dataset from UCI...\n")
      tryCatch(
        utils::download.file(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/00471/Data_for_UCI_named.csv",
          local_path, quiet = FALSE),
        error = function(e2)
          stop("Cannot download ElectricalGrid. Please download 'Data_for_UCI_named.csv' from UCI (dataset #471) and save to data/Data_for_UCI_named.csv")
      )
    }
    df   <- read.csv(local_path, header = TRUE)
    lbls <- as.integer(factor(df[, ncol(df)]))
    list(X = as.matrix(df[, seq_len(ncol(df) - 1)]), labels = lbls)
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "electricalgrid",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 2.0,   # ~2s per run (n=10000 — E-step O(n×k×d²) chậm)

  # Điền ACC% từ Table 2 bài báo khi có:
  # paper_expected = list(acc = c(Proposed = ??, Mean = ??, Zero = ??, EM = ??))
  paper_expected = NULL
))
