# run_segment.R
# Thực nghiệm trên Image Segmentation dataset (2310×18, 7 classes)
# Ước tính thời gian: ~2-4 giờ (sequential) / ~30-60 min (parallel, 7 workers)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Image+Segmentation
# Lưu vào : data/segment.dat (tab/space-separated, 18 features + 1 label cột cuối)
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_segment.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION ════════════════════════════════════════════════════════════
QUICK_MODE   <- FALSE
USE_PARALLEL <- TRUE    # Bắt buộc TRUE cho dataset lớn
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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "segment")
  else                  file.path(getwd(), "..", "results", "segment")
}, error = function(e) file.path(getwd(), "..", "results", "segment"))

# ── Load Segment dataset ──────────────────────────────────────────────────────
# Thử load qua load_dataset(); nếu không có file, download từ UCI
# File UCI: segmentation.test — 2310 dòng, cột 1 = class name, cột 2-19 = 18 features
ds <- tryCatch(
  load_dataset("segment"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    local_path <- file.path(data_dir, "segment.dat")
    if (!file.exists(local_path)) {
      cat("Downloading Segment dataset from UCI...\n")
      tryCatch(
        utils::download.file(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/image/segmentation.test",
          local_path, quiet = FALSE),
        error = function(e2)
          stop("Cannot download Segment. Please download 'segmentation.test' from UCI (Image Segmentation dataset) and save to data/segment.dat")
      )
    }
    # File có 5 dòng header → skip = 5; cột 1 = class (character), cột 2-19 = features
    df   <- read.table(local_path, header = FALSE, skip = 5, sep = ",",
                       stringsAsFactors = FALSE)
    lbls <- as.integer(factor(df[, 1]))
    # Reorder: label về cuối để khớp với data_utils.R (lưu lại file đã xử lý)
    df_out <- cbind(df[, 2:ncol(df)], lbls)
    write.table(df_out, local_path, row.names = FALSE, col.names = FALSE)
    list(X = as.matrix(df[, 2:ncol(df)]), labels = lbls)
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "segment",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 1.0,   # ~1s per run (n=2310, d=18, k=7 — Sigma inversion chậm)

  # Điền ACC% từ Table 2 bài báo khi có:
  # paper_expected = list(acc = c(Proposed = ??, Mean = ??, Zero = ??, EM = ??))
  paper_expected = NULL
))
