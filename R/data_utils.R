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

read_alcoholqcm_csv <- function(path) {
  if (!file.exists(path)) {
    stop("AlcoholQCM dataset not found: ", path)
  }

  first_line <- readLines(path, n = 1L, warn = FALSE)
  sep <- if (length(first_line) > 0L && grepl(";", first_line, fixed = TRUE)) ";" else ","
  df <- read.table(path, header = TRUE, sep = sep, dec = ".", check.names = TRUE)

  if (ncol(df) == 11L) {
    X <- as.matrix(df[, 1:10])
    lbl_raw <- df[, 11]
    labels <- if (is.numeric(lbl_raw) || is.integer(lbl_raw)) {
      as.integer(lbl_raw)
    } else {
      as.integer(factor(lbl_raw))
    }
    return(list(
      X = X,
      labels = labels,
      info = list(n = nrow(X), d = ncol(X), k = length(unique(labels)), name = "AlcoholQCM")
    ))
  }

  if (ncol(df) == 15L) {
    X <- as.matrix(df[, 1:10])
    label_mat <- as.matrix(df[, 11:15])
    if (!all(label_mat %in% c(0, 1))) {
      stop("AlcoholQCM label columns must be binary one-hot encoded.")
    }
    if (any(rowSums(label_mat) != 1)) {
      stop("AlcoholQCM one-hot label rows must contain exactly one active class.")
    }
    labels <- max.col(label_mat, ties.method = "first")
    return(list(
      X = X,
      labels = labels,
      info = list(n = nrow(X), d = ncol(X), k = length(unique(labels)), name = "AlcoholQCM")
    ))
  }

  # Generic fallback: treat last column as label, all previous columns as numeric features.
  if (ncol(df) >= 2L) {
    Xdf <- df[, seq_len(ncol(df) - 1L), drop = FALSE]
    Xdf[] <- lapply(Xdf, function(col) {
      if (is.numeric(col) || is.integer(col)) return(as.numeric(col))
      as.numeric(as.character(col))
    })
    X <- as.matrix(Xdf)

    lbl_raw <- df[, ncol(df)]
    labels <- if (is.numeric(lbl_raw) || is.integer(lbl_raw)) {
      as.integer(lbl_raw)
    } else {
      as.integer(factor(lbl_raw))
    }
    return(list(
      X = X,
      labels = labels,
      info = list(n = nrow(X), d = ncol(X), k = length(unique(labels)), name = "AlcoholQCM")
    ))
  }

  stop("Unsupported AlcoholQCM format: got ", ncol(df), " columns.")
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
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "seeds_dataset.txt"
      )
      if (!file.exists(local_path)) {
        cat("Downloading Seeds dataset from UCI...\n")
        data_dir <- dirname(local_path)
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
        tryCatch(
          utils::download.file(
            "https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",
            local_path, quiet = FALSE),
          error = function(e)
            stop("Cannot download Seeds. Save 'seeds_dataset.txt' from UCI to data/seeds_dataset.txt")
        )
      }
      df <- read.table(local_path, header = FALSE)
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
      if (!file.exists(local_path)) {
        cat("Downloading Wine dataset from UCI...\n")
        data_dir <- dirname(local_path)
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
        tryCatch(
          utils::download.file(
            "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
            local_path, quiet = FALSE),
          error = function(e)
            stop("Cannot download Wine. Save 'wine.data' from UCI to data/wine.data")
        )
      }
      df <- read.csv(local_path, header = FALSE)
      list(
        X      = as.matrix(df[, 2:14]),
        labels = as.integer(df[, 1]),
        info   = list(n = 178, d = 13, k = 3, name = "Wine")
      )
    },

    "vehicle" = {
      # UCI Vehicle Silhouettes (Statlog): 846×18, 4 classes
      # UCI gồm 4 file riêng: bus.dat, opel.dat, saab.dat, van.dat
      # Mỗi file: 18 features (integer) + label text ở cột cuối
      data_dir <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")), "data"
      )
      combined_path <- file.path(data_dir, "vehicle.dat")

      if (file.exists(combined_path)) {
        df   <- read.table(combined_path, header = FALSE)
        lbls <- as.integer(factor(df[, ncol(df)]))
        X    <- as.matrix(df[, seq_len(ncol(df) - 1L)])
      } else {
        # Đọc từng file class, ghép lại
        class_files <- c("bus.dat", "opel.dat", "saab.dat", "van.dat")
        parts <- lapply(class_files, function(fname) {
          p <- file.path(data_dir, fname)
          if (!file.exists(p))
            stop("Vehicle file not found: ", p,
                 "\nDownload from UCI Statlog Vehicle and save to data/")
          read.table(p, header = FALSE)
        })
        df   <- do.call(rbind, parts)
        lbls <- as.integer(factor(df[, ncol(df)]))
        X    <- as.matrix(df[, seq_len(ncol(df) - 1L)])
      }
      list(
        X      = X,
        labels = lbls,
        info   = list(n = nrow(X), d = 18L, k = 4L, name = "Vehicle")
      )
    },

    "glass" = {
      # UCI Glass Identification: 214×9, 6 classes (class 4 absent in data)
      # Col 1 = ID (dropped), col 2-10 = 9 features, col 11 = label (1-7)
      local_path <- file.path(
        dirname(dirname(sys.frame(1)$ofile %||% ".")),
        "data", "glass.data"
      )
      if (!file.exists(local_path)) {
        cat("Downloading Glass Identification dataset from UCI...\n")
        data_dir <- dirname(local_path)
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
        tryCatch(
          utils::download.file(
            "https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
            local_path, quiet = FALSE),
          error = function(e)
            stop("Cannot download Glass. Save 'glass.data' from UCI to data/glass.data")
        )
      }
      df <- read.csv(local_path, header = FALSE)
      list(
        X      = as.matrix(df[, 2:10]),
        labels = as.integer(factor(df[, 11])),   # remap 1-7 → 1-6
        info   = list(n = 214, d = 9, k = 6, name = "Glass")
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
      read_alcoholqcm_csv(local_path)
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
         ". Available: iris, seeds, wine, glass, vehicle, alcoholqcm, segment, electricalgrid, avila, letter")
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Helper: null-coalescing operator
# ─────────────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
