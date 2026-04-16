# run_wine.R
# Thực nghiệm trên Wine dataset (178×13, 3 classes)
# Kết quả kỳ vọng Table 2 "Ours" (trung bình 10-70%):
#   ACC≈87.0%  (Mean=58.0, Zero=74.8, EM=81.8)
#
# Wine dataset có sẵn trong R package 'rattle' hoặc UCI
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_wine.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION ════════════════════════════════════════════════════════════
QUICK_MODE   <- FALSE
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
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "wine")
  else                  file.path(getwd(), "..", "results", "wine")
}, error = function(e) file.path(getwd(), "..", "results", "wine"))

# ── Load Wine dataset ─────────────────────────────────────────────────────────
ds <- tryCatch(
  load_dataset("wine"),
  error = function(e) {
    # Fallback: dùng datasets::Wine từ package rattle hoặc download UCI
    if (requireNamespace("rattle", quietly = TRUE)) {
      data("wine", package = "rattle", envir = environment())
      wine_df <- get("wine", envir = environment())
      list(X = scale(as.matrix(wine_df[, -1])),
           labels = as.integer(wine_df[, 1]))
    } else {
      data_dir <- tryCatch({
        ofile2 <- sys.frame(1)$ofile %||% NULL
        if (!is.null(ofile2)) file.path(dirname(dirname(ofile2)), "data")
        else                   file.path(getwd(), "..", "data")
      }, error = function(e2) file.path(getwd(), "..", "data"))

      if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
      wine_path <- file.path(data_dir, "wine.data")

      if (!file.exists(wine_path)) {
        cat("Downloading Wine dataset from UCI...\n")
        utils::download.file(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
          wine_path, quiet = FALSE)
      }
      df <- read.csv(wine_path, header = FALSE)
      list(X = scale(as.matrix(df[, -1])),
           labels = as.integer(df[, 1]))
    }
  }
)
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "wine",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 0.15,   # ~150ms per run (Wine: 13D, Sigma inversion chậm hơn)

  paper_expected = list(
    acc = c(Proposed = 87.0, Mean = 58.0, Zero = 74.8, EM = 81.8)
  )
))
