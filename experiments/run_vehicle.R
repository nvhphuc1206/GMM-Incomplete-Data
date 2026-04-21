# run_vehicle.R
# Thực nghiệm trên Vehicle Silhouettes dataset (846×18, 4 classes)
# Classes: bus (218), opel (212), saab (217), van (199)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Statlog+(Vehicle+Silhouettes)
# Lưu vào : data/ theo 1 trong 2 cách:
#   Cách 1 (khuyến nghị): 1 file ghép sẵn → data/vehicle.dat
#   Cách 2: 4 file riêng  → data/bus.dat, data/opel.dat, data/saab.dat, data/van.dat
# Format mỗi file: 18 features (integer, space-separated) + label text ở cột cuối
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_vehicle.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION ════════════════════════════════════════════════════════════
QUICK_MODE   <- FALSE   # TRUE = 5 pat × 10 inits (~2 min test)
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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "vehicle")
  else                  file.path(getwd(), "..", "results", "vehicle")
}, error = function(e) file.path(getwd(), "..", "results", "vehicle"))

# ── Load Vehicle dataset ──────────────────────────────────────────────────────
# UCI zip (mới): https://archive.ics.uci.edu/static/public/149/statlog+vehicle+silhouettes.zip
# Nội dung zip: xaa.dat ... xai.dat (mixed classes, không phải per-class)
# Format mỗi dòng: 18 features (integer) + label text (bus/opel/saab/van)
ds <- tryCatch(
  load_dataset("vehicle"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    combined_path <- file.path(data_dir, "vehicle.dat")
    if (!file.exists(combined_path)) {
      zip_url  <- "https://archive.ics.uci.edu/static/public/149/statlog+vehicle+silhouettes.zip"
      zip_path <- file.path(data_dir, "vehicle_raw.zip")

      cat("Downloading Vehicle Silhouettes dataset from UCI...\n")
      tryCatch(
        utils::download.file(zip_url, zip_path, mode = "wb", quiet = FALSE),
        error = function(e2)
          stop("Cannot download Vehicle dataset. Download manually from:\n",
               "  https://archive.ics.uci.edu/dataset/149/statlog+vehicle+silhouettes\n",
               "Extract xaa.dat...xai.dat and save to data/")
      )

      # Extract tất cả x*.dat từ zip
      extracted <- utils::unzip(zip_path, exdir = data_dir)
      dat_files <- extracted[grepl("^x[a-z]+\\.dat$", basename(extracted))]
      if (length(dat_files) == 0)
        stop("No x*.dat files found in Vehicle zip. Check zip contents.")

      # Ghép tất cả files thành 1
      parts <- lapply(sort(dat_files), function(f) read.table(f, header = FALSE))
      df    <- do.call(rbind, parts)

      # Lưu file ghép, xoá zip và các file tạm
      write.table(df, combined_path, row.names = FALSE, col.names = FALSE, quote = FALSE)
      file.remove(zip_path)
      file.remove(dat_files)
      cat(sprintf("Combined file saved: %s (%d rows)\n", combined_path, nrow(df)))
    }

    df   <- read.table(combined_path, header = FALSE)
    lbls <- as.integer(factor(df[, ncol(df)]))
    X    <- as.matrix(df[, seq_len(ncol(df) - 1L)])
    list(X = X, labels = lbls)
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "vehicle",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 0.15,        # ~150ms (n=846, d=18)
  standardize_baselines = TRUE, # F12 RMS=474 vs F15 RMS=8 → scale ratio ~60x

  # Vehicle không có trong Table 2 bài báo — dataset bổ sung
  paper_expected = NULL
))
