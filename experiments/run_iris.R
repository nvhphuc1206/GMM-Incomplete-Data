# run_iris.R
# Thực nghiệm trên Iris dataset (150×4, 3 classes)
# Kết quả kỳ vọng Table 2 "Ours" (trung bình 10-70%):
#   ACC≈84.4%  NMI≈66.3%  F≈84.3%  PUR≈84.7%
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("run_iris.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ══ CONFIGURATION ════════════════════════════════════════════════════════════
QUICK_MODE   <- FALSE   # TRUE = 5 pat × 10 inits (~2 min test)
USE_PARALLEL <- TRUE    # FALSE = sequential (nicer live progress)
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

# ── Source all modules (thứ tự quan trọng) ───────────────────────────────────
source(file.path(R_DIR, "data_utils.R"))
source(file.path(R_DIR, "gmm_incomplete.R"))
source(file.path(R_DIR, "regem.R"))              # cần load trước imputation_baseline
source(file.path(R_DIR, "imputation_baseline.R"))
source(file.path(R_DIR, "dk_kmeans.R"))
source(file.path(R_DIR, "evaluation.R"))
source(file.path(R_DIR, "experiment_runner.R"))  # shared infrastructure

# ── Results directory ─────────────────────────────────────────────────────────
RESULTS_DIR <- tryCatch({
  ofile <- sys.frame(1)$ofile %||% NULL
  if (!is.null(ofile)) file.path(dirname(dirname(ofile)), "results", "iris")
  else                  file.path(getwd(), "..", "results", "iris")
}, error = function(e) file.path(getwd(), "..", "results", "iris"))

# ── Load dataset ─────────────────────────────────────────────────────────────
ds     <- load_dataset("iris")
X_orig <- ds$X; labels <- ds$labels
k      <- length(unique(labels))

# ── Run experiment ────────────────────────────────────────────────────────────
run_experiment(list(
  dataset_name   = "iris",
  X_orig         = X_orig,
  labels         = labels,
  k              = k,
  results_dir    = RESULTS_DIR,

  missing_ratios = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
  paper_ratios   = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),

  quick_mode     = QUICK_MODE,
  use_parallel   = USE_PARALLEL,
  n_cores        = N_CORES,
  secs_per_run   = 0.05,   # ~50ms per GMM run on Iris

  paper_expected = list(
    acc = c(Proposed = 84.4, Mean = 61.3, Zero = 67.3, EM = 76.0,
            DK_Mean  = 68.6, DK_Zero = 70.7, DK_EM = 76.0)
  )
))
