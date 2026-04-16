# run_letter.R
# Thực nghiệm trên Letter Recognition dataset (20000×16, 26 classes)
# Ước tính thời gian: ~10-24 giờ (sequential) / ~3-6 giờ (parallel)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Letter+Recognition
# Lưu vào : data/letter-recognition.data (CSV không có header,
#            cột 1 = label chữ cái, cột 2-17 = 16 features)
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_letter.R")

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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "letter")
  else                  file.path(getwd(), "..", "results", "letter")
}, error = function(e) file.path(getwd(), "..", "results", "letter"))

# ── Load Letter dataset ───────────────────────────────────────────────────────
# Thử load qua load_dataset(); nếu không có file, download từ UCI
# File UCI: cột 1 = chữ cái (A-Z), cột 2-17 = 16 features
ds <- tryCatch(
  load_dataset("letter"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    local_path <- file.path(data_dir, "letter-recognition.data")
    if (!file.exists(local_path)) {
      cat("Downloading Letter Recognition dataset from UCI...\n")
      tryCatch(
        utils::download.file(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data",
          local_path, quiet = FALSE),
        error = function(e2)
          stop("Cannot download Letter. Please download 'letter-recognition.data' from UCI (Letter Recognition dataset) and save to data/letter-recognition.data")
      )
    }
    # Cột 1 = label (A-Z), cột 2-17 = 16 features integer
    df   <- read.csv(local_path, header = FALSE)
    lbls <- as.integer(factor(df[, 1]))
    list(X = as.matrix(df[, 2:17]), labels = lbls)
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "letter",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 8.0,   # ~8s per run (n=20000, k=26 — E-step rất nặng)

  # Điền ACC% từ Table 2 bài báo khi có:
  # paper_expected = list(acc = c(Proposed = ??, Mean = ??, Zero = ??, EM = ??))
  paper_expected = NULL
))
