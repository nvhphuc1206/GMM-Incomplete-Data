# imputation_baseline.R
# Các phương pháp imputation baseline để so sánh với Proposed GMM
# Tái hiện: meanfilling.m, zerofilling.m, DataCompletion.m (regem) từ MATLAB gốc
#
# LƯU Ý (theo EXPERIMENT_v2.md §5):
#   - mean_filling()  : fill trên raw data (CHƯA chuẩn hóa) — khớp MATLAB demo.m
#   - zero_filling()  : fill = 0 trên data ĐÃ chuẩn hóa RMS — tức fill = mean
#   - em_filling()    : Gaussian EM conditional-mean imputation trên data ĐÃ chuẩn hóa

# ─────────────────────────────────────────────────────────────────────────────
# 1. Mean Filling — khớp meanfilling.m
#    Điền NA bằng trung bình cột của observed values
#    Áp dụng trên raw data (trước standardize_rms)
# ─────────────────────────────────────────────────────────────────────────────
mean_filling <- function(X) {
  # X: ma trận n×d, có thể có NA
  # Trả về: Xfill không còn NA
  Xfill <- X
  for (j in seq_len(ncol(X))) {
    obs <- !is.na(X[, j])
    if (any(!obs) && any(obs)) {
      Xfill[!obs, j] <- mean(X[obs, j])
    }
  }
  Xfill
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. Zero Filling — khớp zerofilling.m
#    Điền NA bằng 0 (sau khi data đã được chuẩn hóa RMS, 0 = mean)
# ─────────────────────────────────────────────────────────────────────────────
zero_filling <- function(X) {
  # X: ma trận n×d, có thể có NA (nên là data đã standardize_rms)
  # Trả về: Xfill với NA → 0
  Xfill <- X
  Xfill[is.na(Xfill)] <- 0
  Xfill
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. EM Filling — khớp DataCompletion(X, 'EM') trong MATLAB
#    Gọi regem_r() (Schneider 2001 regularized EM với GCV ridge regression).
#    regem.R phải được source() trước khi gọi hàm này.
#
#    Tham số mặc định khớp MATLAB regem.m:
#      max_iter = 10   (MATLAB default: maxit=10)
#      stagtol  = 5e-2 (MATLAB default: stagtol=5e-2)
# ─────────────────────────────────────────────────────────────────────────────
em_filling <- function(X, max_iter = 10L, stagtol = 5e-2) {
  # X: ma trận n×d, có thể có NA (data đã standardize_rms)
  # Trả về: Xfill không còn NA
  if (exists("regem_r", mode = "function", envir = .GlobalEnv) ||
      exists("regem_r", mode = "function")) {
    return(regem_r(X, max_iter = max_iter, stagtol = stagtol))
  }
  # Fallback: simple conditional-mean EM (nếu regem.R chưa được source)
  warning("regem_r not found — using simple EM fallback. Source R/regem.R for better results.")
  n <- nrow(X); d <- ncol(X)
  Xfill <- X
  for (j in seq_len(d)) {
    nas <- is.na(Xfill[, j])
    if (any(nas) && any(!nas)) Xfill[nas, j] <- mean(Xfill[!nas, j])
  }
  for (iter in seq_len(100L)) {
    Xold      <- Xfill
    mu_est    <- colMeans(Xfill)
    S         <- cov(Xfill)
    lambda    <- max(1e-4, 0.10 * mean(diag(S)))
    Sigma_est <- S + lambda * diag(d)
    for (i in seq_len(n)) {
      mi <- which(is.na(X[i, ]))
      if (length(mi) == 0L) next
      oi <- setdiff(seq_len(d), mi)
      if (length(oi) == 0L) { Xfill[i, mi] <- mu_est[mi]; next }
      Xfill[i, mi] <- mu_est[mi] + as.vector(
        Sigma_est[mi, oi, drop=FALSE] %*% solve(Sigma_est[oi, oi, drop=FALSE],
                                                 Xfill[i, oi] - mu_est[oi]))
    }
    if (max(abs(Xfill - Xold), na.rm = TRUE) < 1e-6) break
  }
  Xfill
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. Hàm tiện ích: chuẩn bị data cho tất cả methods
#    Tái hiện đúng data flow trong demo.m (xem EXPERIMENT_v2.md §5)
#
#    Trả về list:
#      $data_mean : raw data + mean fill (chưa chuẩn hóa)
#      $data_zero : standardized + zero fill
#      $data_em   : standardized + EM fill
#      $data_std  : standardized với NA giữ nguyên (để track missing)
# ─────────────────────────────────────────────────────────────────────────────
prepare_all_fillings <- function(X_raw, standardize_baselines = FALSE) {
  # X_raw: raw data gốc (có NA = missing positions đã được tạo)
  # standardize_baselines: nếu TRUE, Mean/DK_Mean baseline cũng dùng
  #   data đã chuẩn hóa (mean_fill → standardize_rms). Dùng cho datasets
  #   có features khác scale lớn (e.g. Glass: Si~72, Fe~0.06).
  #   Mặc định FALSE để giữ đúng behavior MATLAB gốc (Iris/Seeds/Wine).

  # 1. Mean fill trên raw data (CHƯA chuẩn hóa) — như MATLAB data_mean
  data_mean_raw <- mean_filling(X_raw)

  # 2. Chuẩn hóa RMS (NA được giữ nguyên)
  data_std <- standardize_rms(X_raw)

  # 3. Zero fill trên data đã chuẩn hóa
  data_zero <- zero_filling(data_std)

  # 4. EM fill trên data đã chuẩn hóa
  data_em <- em_filling(data_std)

  # 5. (optional) Mean fill → standardize: công bằng hơn khi features khác scale
  data_mean_std <- if (standardize_baselines) standardize_rms(data_mean_raw) else data_mean_raw

  list(
    data_mean = data_mean_std,   # raw hoặc std tuỳ standardize_baselines
    data_zero = data_zero,
    data_em   = data_em,
    data_std  = data_std
  )
}
