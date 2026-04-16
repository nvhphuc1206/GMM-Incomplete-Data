# run_avila.R
# Thực nghiệm trên Avila dataset (20871×10, 12 classes)
# Ước tính thời gian: ~8-16 giờ (sequential) / ~2-4 giờ (parallel)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Avila
# Lưu vào : data/avila-tr.txt (CSV không có header, 10 features + 1 label cột cuối)
#           Dùng file training set (avila-tr.txt), KHÔNG phải test set
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_avila.R")

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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "avila")
  else                  file.path(getwd(), "..", "results", "avila")
}, error = function(e) file.path(getwd(), "..", "results", "avila"))

# ── Load Avila dataset ────────────────────────────────────────────────────────
# Thử load qua load_dataset(); nếu không có file, download từ UCI
# UCI cung cấp file zip chứa avila-tr.txt (training) và avila-ts.txt (test)
ds <- tryCatch(
  load_dataset("avila"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    local_path <- file.path(data_dir, "avila-tr.txt")
    if (!file.exists(local_path)) {
      zip_path <- file.path(data_dir, "avila.zip")
      cat("Downloading Avila dataset from UCI...\n")
      tryCatch({
        utils::download.file(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/00459/avila.zip",
          zip_path, quiet = FALSE)
        utils::unzip(zip_path, exdir = data_dir)
        # File sau khi giải nén có thể nằm trong subfolder avila/
        extracted <- list.files(data_dir, pattern = "avila-tr\\.txt",
                                recursive = TRUE, full.names = TRUE)
        if (length(extracted) > 0 && extracted[1] != local_path)
          file.copy(extracted[1], local_path)
      }, error = function(e2)
        stop("Cannot download Avila. Please download avila.zip from UCI (dataset #459), unzip and save avila-tr.txt to data/avila-tr.txt")
      )
    }
    df   <- read.csv(local_path, header = FALSE)
    lbls <- as.integer(factor(df[, ncol(df)]))
    list(X = as.matrix(df[, seq_len(ncol(df) - 1)]), labels = lbls)
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "avila",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 5.0,   # ~5s per run (n=20871, k=12 — rất chậm)

  # Điền ACC% từ Table 2 bài báo khi có:
  # paper_expected = list(acc = c(Proposed = ??, Mean = ??, Zero = ??, EM = ??))
  paper_expected = NULL
))
