# run_glass.R
# Thực nghiệm trên Glass Identification dataset (214×9, 6 classes)
# Cùng cỡ Seeds — dùng để verify nhanh với k=6 (nhiều class hơn iris/seeds/wine)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Glass+Identification
# Lưu vào : data/glass.data (CSV không có header)
#           Cột 1 = ID (bỏ), cột 2-10 = 9 features, cột 11 = class (1-7, class 4 vắng)
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_glass.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION ════════════════════════════════════════════════════════════
QUICK_MODE   <- FALSE   # TRUE = 5 pat × 10 inits (~1 min test)
USE_PARALLEL <- TRUE
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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "glass")
  else                  file.path(getwd(), "..", "results", "glass")
}, error = function(e) file.path(getwd(), "..", "results", "glass"))

# ── Load Glass dataset ────────────────────────────────────────────────────────
# Thử load qua load_dataset(); nếu không có file, download từ UCI
ds <- tryCatch(
  load_dataset("glass"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    local_path <- file.path(data_dir, "glass.data")
    if (!file.exists(local_path)) {
      cat("Downloading Glass Identification dataset from UCI...\n")
      tryCatch(
        utils::download.file(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
          local_path, quiet = FALSE),
        error = function(e2)
          stop("Cannot download Glass. Please download 'glass.data' from UCI (Glass Identification dataset) and save to data/glass.data")
      )
    }
    # Cột 1 = ID (bỏ), cột 2-10 = 9 features, cột 11 = class label
    df <- read.csv(local_path, header = FALSE)
    list(X = as.matrix(df[, 2:10]),
         labels = as.integer(factor(df[, 11])))
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
# Glass-specific notes:
# - k=6 với lớp nhỏ nhất chỉ 9 mẫu (d=9) → covariance gần singular
# - km_nstart=10 giúp khởi tạo tốt hơn với k lớn và lớp mất cân bằng
# - sigma_shift=1e-6 thay vì default 1e-8 để ổn định số học cho M-step 2
run_experiment(list(
  dataset_name   = "glass",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 0.15,   # ~150ms (k=6, d=9 — nặng hơn Iris ~3x)
  km_nstart      = 10L,    # default=3; tăng cho k=6, lớp mất cân bằng
  sigma_shift    = 1e-6,   # default=1e-8; tăng để ổn định solve(Sigma) với cluster nhỏ

  # Glass không có trong Table 2 bài báo — đây là dataset bổ sung
  paper_expected = NULL
))
