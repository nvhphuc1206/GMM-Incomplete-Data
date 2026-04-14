# data_utils.R
# Tiện ích load dữ liệu, chuẩn hóa, và tạo missing mask
# Tái hiện: standardizematrix.m, checkindex.m từ MATLAB gốc

# ─────────────────────────────────────────────────────────────────────────────
# 1. Chuẩn hóa RMS — khớp chính xác standardizematrix.m
#    std = sqrt(mean(x^2)), KHÔNG phải sd() hay scale()
# ─────────────────────────────────────────────────────────────────────────────
standardize_rms <- function(X) {
  # X: ma trận n×d, NA được giữ nguyên
  # Trả về: ma trận cùng chiều, NaN/NA vẫn là NA
  Xnew <- X
  d <- ncol(X)
  for (j in seq_len(d)) {
    obs <- !is.na(X[, j])
    if (sum(obs) < 2) next
    xobs  <- X[obs, j]
    m     <- mean(xobs)                    # mean của observed values
    s     <- sqrt(mean(xobs^2))            # RMS — khớp MATLAB: sqrt(sum2/count)
    if (s > .Machine$double.eps * 100) {
      Xnew[obs, j] <- (xobs - m) / s
    }
  }
  Xnew
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. Tạo missing mask MCAR
#    Khớp behavior trong demo.m: randomly pick n*d*ratio positions
# ─────────────────────────────────────────────────────────────────────────────
generate_missing <- function(X, ratio, seed = NULL) {
  # X: ma trận gốc (không có NA)
  # ratio: tỉ lệ missing [0, 1]
  # Trả về: Xmiss (cùng kích thước, một số entries là NA)
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(X); d <- ncol(X)
  Xmiss <- X
  n_missing <- ceiling(n * d * ratio)  # dùng ceiling như MATLAB indexnum = ceil(...)
  if (n_missing == 0 || ratio == 0) return(Xmiss)

  # Sample ngẫu nhiên n_missing vị trí trong ma trận
  total <- n * d
  pos <- sample(total, min(n_missing, total), replace = FALSE)
  # Chuyển linear index → (row, col) theo column-major (R mặc định)
  rows <- ((pos - 1) %% n) + 1
  cols <- ((pos - 1) %/% n) + 1
  for (i in seq_along(rows)) {
    Xmiss[rows[i], cols[i]] <- NA
  }
  Xmiss
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. Load UCI datasets
#    Trả về list(X = matrix n×d, labels = integer vector n)
# ─────────────────────────────────────────────────────────────────────────────
load_dataset <- function(name) {
  name <- tolower(name)
  switch(name,

    "iris" = {
      data(iris, package = "datasets", envir = environment())
      list(
        X      = as.matrix(iris[, 1:4]),
        labels = as.integer(iris[, 5]),
        info   = list(n = 150, d = 4, k = 3, name = "Iris")
      )
    },

    "seeds" = {
      # UCI Seeds dataset: 210×7, 3 classes
      # Thử load từ file local trước, fallback tải về
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "seeds_dataset.txt"
      )
      if (file.exists(local_path)) {
        df <- read.table(local_path, header = FALSE)
      } else {
        df <- read.table(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",
          header = FALSE
        )
      }
      list(
        X      = as.matrix(df[, 1:7]),
        labels = as.integer(df[, 8]),
        info   = list(n = 210, d = 7, k = 3, name = "Seeds")
      )
    },

    "wine" = {
      # UCI Wine: 178×13, 3 classes
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "wine.data"
      )
      if (file.exists(local_path)) {
        df <- read.csv(local_path, header = FALSE)
      } else {
        df <- read.csv(
          "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
          header = FALSE
        )
      }
      list(
        X      = as.matrix(df[, 2:14]),
        labels = as.integer(df[, 1]),
        info   = list(n = 178, d = 13, k = 3, name = "Wine")
      )
    },

    "alcoholqcm" = {
      # UCI AlcoholQCM: 125×10, 5 classes
      # Cần download thủ công từ UCI
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "AlcoholQCM.csv"
      )
      if (!file.exists(local_path)) {
        stop("AlcoholQCM dataset not found. Download from UCI and save to data/AlcoholQCM.csv")
      }
      df <- read.csv(local_path, header = TRUE)
      list(
        X      = as.matrix(df[, 1:10]),
        labels = as.integer(df[, 11]),
        info   = list(n = 125, d = 10, k = 5, name = "AlcoholQCM")
      )
    },

    "segment" = {
      # UCI Image Segmentation: 2310×18, 7 classes
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "segment.dat"
      )
      if (!file.exists(local_path)) {
        stop("Segment dataset not found. Download from UCI and save to data/segment.dat")
      }
      df   <- read.table(local_path, header = FALSE)
      lbls <- as.integer(factor(df[, ncol(df)]))
      list(
        X      = as.matrix(df[, -ncol(df)]),
        labels = lbls,
        info   = list(n = 2310, d = 18, k = 7, name = "Segment")
      )
    },

    "electricalgrid" = {
      # UCI Electrical Grid Stability: 10000×13, 2 classes
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "Data_for_UCI_named.csv"
      )
      if (!file.exists(local_path)) {
        stop("ElectricalGrid dataset not found. Download from UCI.")
      }
      df   <- read.csv(local_path, header = TRUE)
      lbls <- as.integer(factor(df[, ncol(df)]))
      list(
        X      = as.matrix(df[, 1:13]),
        labels = lbls,
        info   = list(n = 10000, d = 13, k = 2, name = "ElectricalGrid")
      )
    },

    "avila" = {
      # UCI Avila: 20871×10, 12 classes
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "avila-tr.txt"
      )
      if (!file.exists(local_path)) {
        stop("Avila dataset not found. Download from UCI.")
      }
      df   <- read.csv(local_path, header = FALSE)
      lbls <- as.integer(factor(df[, ncol(df)]))
      list(
        X      = as.matrix(df[, 1:10]),
        labels = lbls,
        info   = list(n = nrow(df), d = 10, k = 12, name = "Avila")
      )
    },

    "letter" = {
      # UCI Letter Recognition: 20000×16, 26 classes
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "letter-recognition.data"
      )
      if (!file.exists(local_path)) {
        stop("Letter dataset not found. Download from UCI.")
      }
      df   <- read.csv(local_path, header = FALSE)
      lbls <- as.integer(factor(df[, 1]))
      list(
        X      = as.matrix(df[, 2:17]),
        labels = lbls,
        info   = list(n = 20000, d = 16, k = 26, name = "Letter")
      )
    },

    stop("Unknown dataset: ", name,
         ". Available: iris, seeds, wine, alcoholqcm, segment, electricalgrid, avila, letter")
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Helper: null-coalescing operator
# ─────────────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
