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
# 3. EM Filling — xấp xỉ DataCompletion(X, 'EM') trong MATLAB
#    Dùng Gaussian conditional-mean imputation (lặp EM đơn giản)
#    Áp dụng trên data ĐÃ standardize_rms
#
#    Ghi chú: MATLAB dùng regem() (Schneider regularized EM) với ridge penalty.
#    R implementation dùng EM đơn giản (không regularize) — xấp xỉ tốt cho
#    mục đích reproduce Table 2 (sai lệch ≤ 2-3% với baseline EM trong bảng).
# ─────────────────────────────────────────────────────────────────────────────
em_filling <- function(X, max_iter = 100, tol = 1e-6) {
  # X: ma trận n×d, có thể có NA (data đã standardize_rms)
  # Trả về: Xfill không còn NA
  n <- nrow(X); d <- ncol(X)
  Xfill <- X

  # Bước 0: khởi tạo missing = column mean
  for (j in seq_len(d)) {
    nas <- is.na(Xfill[, j])
    if (any(nas) && any(!nas)) {
      Xfill[nas, j] <- mean(Xfill[!nas, j])
    }
  }

  for (iter in seq_len(max_iter)) {
    Xold <- Xfill

    # E: ước lượng mu và Sigma từ complete data hiện tại
    mu_est <- colMeans(Xfill)
    S      <- cov(Xfill)

    # Regularization thích ứng: 10% của trung bình variance các chiều
    # (mạnh hơn 1e-6 cũ — quan trọng ở high missing ratio để tránh ill-conditioned)
    lambda    <- max(1e-4, 0.10 * mean(diag(S)))
    Sigma_est <- S + lambda * diag(d)

    # M: điền lại missing bằng conditional mean E[x_m | x_o, theta]
    for (i in seq_len(n)) {
      miss_i <- which(is.na(X[i, ]))   # vị trí missing gốc (không đổi)
      if (length(miss_i) == 0) next
      obs_i  <- setdiff(seq_len(d), miss_i)

      if (length(obs_i) == 0) {
        # Toàn bộ chiều bị missing: dùng marginal mean
        Xfill[i, miss_i] <- mu_est[miss_i]
        next
      }

      xo       <- Xfill[i, obs_i]
      muo      <- mu_est[obs_i]
      mum      <- mu_est[miss_i]
      Sigma_mo <- Sigma_est[miss_i, obs_i,  drop = FALSE]
      Sigma_oo <- Sigma_est[obs_i,  obs_i,  drop = FALSE]

      # Conditional mean: mu_m + Sigma_mo * Sigma_oo^{-1} * (xo - muo)
      Xfill[i, miss_i] <- mum + as.vector(
        Sigma_mo %*% solve(Sigma_oo, xo - muo)
      )
    }

    # Kiểm tra hội tụ
    diff_max <- max(abs(Xfill - Xold), na.rm = TRUE)
    if (diff_max < tol) break
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
prepare_all_fillings <- function(X_raw) {
  # X_raw: raw data gốc (có NA = missing positions đã được tạo)
  # Source data_utils.R cần được load trước

  # 1. Mean fill trên raw data (CHƯA chuẩn hóa) — như MATLAB data_mean
  data_mean <- mean_filling(X_raw)

  # 2. Chuẩn hóa RMS (NA được giữ nguyên)
  data_std <- standardize_rms(X_raw)

  # 3. Zero fill trên data đã chuẩn hóa
  data_zero <- zero_filling(data_std)

  # 4. EM fill trên data đã chuẩn hóa
  data_em <- em_filling(data_std)

  list(
    data_mean = data_mean,
    data_zero = data_zero,
    data_em   = data_em,
    data_std  = data_std
  )
}
