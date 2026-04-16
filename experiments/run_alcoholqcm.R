# run_alcoholqcm.R
# Thực nghiệm trên AlcoholQCM dataset (125×10, 5 classes)
#
# Download: https://archive.ics.uci.edu/ml/datasets/Alcohol+QCM+Sensor+Dataset
# Lưu vào : data/AlcoholQCM.csv (header = TRUE, 10 features + 1 label cột cuối)
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_alcoholqcm.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

download_alcoholqcm_from_uci <- function(dest_csv_path, sensor = "QCM6") {
  data_dir <- dirname(dest_csv_path)
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  zip_path <- file.path(data_dir, "alcohol_qcm_sensor_dataset.zip")
  urls <- c(
    # UCI currently serves dataset downloads under /static/public/<id>/<name>.zip
    "https://archive.ics.uci.edu/static/public/496/alcohol%2Bqcm%2Bsensor%2Bdataset.zip",
    # Fallback if URL decoding differs
    "https://archive.ics.uci.edu/static/public/496/alcohol+qcm+sensor+dataset.zip"
  )

  downloaded <- FALSE
  for (u in urls) {
    tryCatch({
      utils::download.file(u, zip_path, mode = "wb", quiet = FALSE)
      if (file.exists(zip_path) && !is.na(file.info(zip_path)$size) && file.info(zip_path)$size > 0) {
        downloaded <- TRUE
      }
    }, error = function(e) NULL)
    if (downloaded) break
  }
  if (!downloaded) {
    stop(
      "Cannot download AlcoholQCM from UCI.\n",
      "Please download the dataset manually from:\n",
      "  https://archive.ics.uci.edu/dataset/496/alcohol+qcm+sensor+dataset\n",
      "and save a CSV to: ", dest_csv_path
    )
  }

  exdir <- file.path(data_dir, "alcohol_qcm_sensor_dataset")
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  tryCatch(
    utils::unzip(zip_path, exdir = exdir),
    error = function(e) stop("Cannot unzip downloaded AlcoholQCM archive: ", zip_path)
  )

  csvs <- list.files(exdir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(csvs) == 0L) stop("No .csv files found after unzipping: ", zip_path)

  # Prefer a specific sensor file if present (default: QCM6.csv), otherwise pick the first csv.
  sensor_pat <- paste0("(^|[\\\\/])", sensor, "\\.csv$")
  picked <- (csvs[grepl(sensor_pat, csvs, ignore.case = TRUE)][1]) %||% csvs[1]

  df <- tryCatch(
    read.csv(picked, header = TRUE, check.names = TRUE),
    error = function(e) stop("Cannot read extracted AlcoholQCM csv: ", picked)
  )

  utils::write.csv(df, dest_csv_path, row.names = FALSE)
  invisible(dest_csv_path)
}

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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "alcoholqcm")
  else                  file.path(getwd(), "..", "results", "alcoholqcm")
}, error = function(e) file.path(getwd(), "..", "results", "alcoholqcm"))

# ── Load AlcoholQCM dataset ───────────────────────────────────────────────────
# Thử load qua load_dataset(); nếu không có file, download từ UCI
ds <- tryCatch(
  load_dataset("alcoholqcm"),
  error = function(e) {
    data_dir <- tryCatch({
      ofile <- sys.frame(1)$ofile %||% NULL
      if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "data")
      else                  file.path(getwd(), "..", "data")
    }, error = function(e2) file.path(getwd(), "..", "data"))
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

    local_path <- file.path(data_dir, "AlcoholQCM.csv")
    if (!file.exists(local_path)) {
      cat("Downloading AlcoholQCM dataset from UCI...\n")
      download_alcoholqcm_from_uci(local_path, sensor = "QCM6")
    }

    df <- read.csv(local_path, header = TRUE, check.names = TRUE)
    # Cột cuối là label, các cột còn lại là features
    list(X = as.matrix(df[, seq_len(ncol(df) - 1)]),
         labels = as.integer(factor(df[, ncol(df)])))
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "alcoholqcm",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 0.04,   # ~40ms (dataset nhỏ, n=125)

  # Điền ACC% từ Table 2 bài báo khi có:
  # paper_expected = list(acc = c(Proposed = ??, Mean = ??, Zero = ??, EM = ??))
  paper_expected = NULL
))
