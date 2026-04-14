# gmm_incomplete.R
# Core: GMM Clustering with Incomplete Data — Algorithm 1 (Zhang et al., 2021)
#
# Tái hiện trung thành gmm.m từ MATLAB gốc (github.com/Zhangyi1231/GMM-with-Incomplete-Data)
# Xem EXPERIMENT_v2.md để biết các điểm khác so với mô tả trong bài báo.

# ─────────────────────────────────────────────────────────────────────────────
# Internal: tính log-likelihood có trọng số (khớp wdensity.m)
#   Trả về: ma trận N×K, [i,j] = log(alpha_j) + log N(x_i | mu_j, Sigma_j)
# ─────────────────────────────────────────────────────────────────────────────
.compute_log_lh <- function(X, mu, Sigma, Pi) {
  n <- nrow(X); d <- ncol(X); k <- length(Pi)
  log_lh <- matrix(0.0, n, k)
  log_2pi <- d * log(2 * pi)

  for (j in seq_len(k)) {
    S <- Sigma[, , j]
    # Cholesky decomposition — như MATLAB wdensity.m
    L <- tryCatch(
      chol(S),   # R: upper triangular, L'*L = S
      error = function(e) {
        # Fallback: thêm regularization mạnh hơn nếu không PD
        chol(S + 1e-6 * diag(d))
      }
    )
    log_det_sigma <- 2 * sum(log(diag(L)))  # log|Sigma|

    # Mahalanobis: ||(x - mu) * L^{-1}||^2 = (x-mu) Sigma^{-1} (x-mu)'
    Xc <- sweep(X, 2, mu[j, ], "-")   # n×d
    # forwardsolve(t(L), t(Xc)): giải L^T z = Xc^T  (L^T là lower triangular)
    z  <- forwardsolve(t(L), t(Xc))   # d×n
    maha <- colSums(z^2)               # n-vector

    log_lh[, j] <- -0.5 * (maha + log_det_sigma + log_2pi) + log(Pi[j])
  }
  log_lh
}

# ─────────────────────────────────────────────────────────────────────────────
# Internal: E-step — tính posterior và log-likelihood (khớp estep.m)
# ─────────────────────────────────────────────────────────────────────────────
.estep_gmm <- function(log_lh, prob_th = 1e-12) {
  max_ll  <- apply(log_lh, 1, max)
  post    <- exp(log_lh - max_ll)   # tránh underflow (log-sum-exp)
  density <- rowSums(post)
  logpdf  <- log(density) + max_ll
  L       <- sum(logpdf)
  post    <- post / density

  # Set posterior rất nhỏ về 0 (như MATLAB estep.m)
  if (!is.null(prob_th) && prob_th > 0) {
    post[post < prob_th] <- 0
    rs <- rowSums(post)
    rs[rs == 0] <- 1  # tránh chia 0
    post <- post / rs
  }
  list(L = L, gamma = post)
}

# ─────────────────────────────────────────────────────────────────────────────
# Internal: khởi tạo GMM params từ centroids (khớp init_params() trong gmm.m)
# ─────────────────────────────────────────────────────────────────────────────
.init_gmm_params <- function(X, centroids, sigma_shift = 1e-8) {
  n <- nrow(X); d <- ncol(X)
  k <- nrow(centroids)

  # Assign mỗi điểm vào centroid gần nhất (Euclidean)
  dist2 <- function(x, C) rowSums(sweep(C, 2, x, "-")^2)
  dists <- t(apply(X, 1, dist2, C = centroids))  # n×k
  labels <- apply(dists, 1, which.min)

  pMiu   <- centroids
  pPi    <- numeric(k)
  pSigma <- array(0, dim = c(d, d, k))

  for (kk in seq_len(k)) {
    idx_k <- which(labels == kk)
    pPi[kk] <- length(idx_k) / n
    if (length(idx_k) > 1) {
      Xk <- X[idx_k, , drop = FALSE]
      pSigma[, , kk] <- cov(Xk) + sigma_shift * diag(d)
    } else if (length(idx_k) == 1) {
      # Singleton: dùng Sigma của toàn bộ data
      pSigma[, , kk] <- diag(apply(X, 2, var) + sigma_shift)
    } else {
      # Empty cluster: dùng identity
      pSigma[, , kk] <- sigma_shift * diag(d)
      # Re-assign điểm xa nhất vào cluster này
      far_idx <- which.max(rowMeans(dists))
      labels[far_idx] <- kk
      pPi[kk] <- 1 / n
    }
  }
  list(pMiu = pMiu, pPi = pPi, pSigma = pSigma)
}

# ─────────────────────────────────────────────────────────────────────────────
# gmm_incomplete()
#
# Giao diện chính — tái hiện gmm() trong MATLAB
#
# Tham số:
#   X_init    : ma trận n×d đã điền đầy (không có NA) — starting point
#   k         : số cụm
#   miss_mat  : ma trận logical n×d (TRUE = vị trí bị missing trong dữ liệu gốc)
#   centroids : ma trận k×d centroid khởi tạo (từ K-means bên ngoài)
#   max_iter  : số iterations tối đa (default = 500)
#   epsilon   : ngưỡng dừng tương đối (default = 1e-4, khớp MATLAB)
#   sigma_shift: regularization cho Sigma (default = 1e-8, khớp MATLAB)
#
# Trả về: list(labels, gamma, mu, Sigma, alpha, X_imputed, loglik_trace)
# ─────────────────────────────────────────────────────────────────────────────
gmm_incomplete <- function(X_init,
                           k,
                           miss_mat   = NULL,
                           centroids  = NULL,
                           max_iter   = 500,
                           epsilon    = 1e-4,
                           sigma_shift = 1e-8,
                           seed       = NULL) {

  if (!is.null(seed)) set.seed(seed)

  X <- as.matrix(X_init)
  n <- nrow(X); d <- ncol(X)

  # Xử lý missing mask
  if (is.null(miss_mat)) {
    miss_mat <- matrix(FALSE, n, d)
  }
  # Lập danh sách các sample có missing
  miss_rows <- which(rowSums(miss_mat) > 0)

  # Nếu vẫn còn NA trong X (ví dụ truyền raw data), fill bằng column mean
  for (j in seq_len(d)) {
    nas <- is.na(X[, j])
    if (any(nas)) X[nas, j] <- mean(X[!nas, j], na.rm = TRUE)
  }

  # ── Khởi tạo centroids nếu không truyền vào ──────────────────────────────
  if (is.null(centroids)) {
    km         <- kmeans(X, centers = k, nstart = 1, iter.max = 100)
    centroids  <- km$centers
  }

  # ── Khởi tạo GMM parameters ───────────────────────────────────────────────
  params  <- .init_gmm_params(X, centroids, sigma_shift)
  pMiu    <- params$pMiu     # k×d
  pPi     <- params$pPi      # length k
  pSigma  <- params$pSigma   # d×d×k

  # ── Vòng lặp EM ──────────────────────────────────────────────────────────
  Lprev        <- -Inf
  loglik_trace <- numeric(max_iter)
  iters_done   <- max_iter

  for (iter in seq_len(max_iter)) {

    # ── Bước 1: E-step ──────────────────────────────────────────────────────
    log_lh <- .compute_log_lh(X, pMiu, pSigma, pPi)
    es1    <- .estep_gmm(log_lh)
    L      <- es1$L
    pGamma <- es1$gamma
    loglik_trace[iter] <- L

    # ── Bước 2: Kiểm tra hội tụ ─────────────────────────────────────────────
    if (iter > 1 && (L - Lprev) / abs(L + .Machine$double.eps) < epsilon) {
      iters_done <- iter
      break
    }
    Lprev <- L

    # ── Bước 3: M-step 1 — cập nhật mu, Sigma, Pi ──────────────────────────
    Nk <- colSums(pGamma)
    Nk[Nk == 0] <- .Machine$double.eps  # tránh chia 0

    for (kk in seq_len(k)) {
      g_k <- pGamma[, kk]

      # Eq. (9): mu_i
      pMiu[kk, ] <- colSums(g_k * X) / Nk[kk]

      # Eq. (10): Sigma_i
      Xshift <- sweep(X, 2, pMiu[kk, ], "-")   # n×d
      # Weighted covariance: sum_j gamma_j * (x_j - mu)(x_j - mu)^T / Nk
      S <- t(Xshift) %*% (g_k * Xshift) / Nk[kk]
      # Symmetrize + regularize (khớp MATLAB: (S+S')/2 + Sigma_shift*I)
      pSigma[, , kk] <- (S + t(S)) / 2 + sigma_shift * diag(d)
    }
    # Eq. (11): alpha_i
    pPi <- Nk / n

    # ── Bước 4: E-step thứ 2 với params đã update ──────────────────────────
    # (QUAN TRỌNG — đây là điểm khác biệt so với mô tả trong bài báo,
    #  nhưng đúng theo MATLAB gmm.m: gamma dùng trong M-step 2 là gamma mới)
    log_lh2 <- .compute_log_lh(X, pMiu, pSigma, pPi)
    es2     <- .estep_gmm(log_lh2)
    pGamma  <- es2$gamma

    # ── Bước 5: M-step 2 — cập nhật giá trị missing (Eq. 14) ───────────────
    if (length(miss_rows) > 0) {
      for (j in miss_rows) {
        miss_j  <- which(miss_mat[j, ])          # indices chiều bị missing
        num_miss <- length(miss_j)
        obs_j   <- which(!miss_mat[j, ])         # indices chiều quan sát được

        xo <- X[j, obs_j]

        # Tích lũy qua k components
        smm_sum  <- matrix(0, num_miss, num_miss)
        sigu_sum <- numeric(num_miss)
        smox_sum <- numeric(num_miss)

        # Flag: row có toàn bộ features missing (obs_j rỗng)
        all_missing <- (length(obs_j) == 0)

        for (i in seq_len(k)) {
          sigma_inv <- tryCatch(
            solve(pSigma[, , i]),
            error = function(e) solve(pSigma[, , i] + 1e-6 * diag(d))
          )
          smo  <- sigma_inv[miss_j, obs_j,  drop = FALSE]   # num_miss × |obs|
          smm  <- sigma_inv[miss_j, miss_j, drop = FALSE]   # num_miss × num_miss
          mum  <- pMiu[i, miss_j]
          gji  <- pGamma[j, i]

          # Tránh lỗi R khi nhân ma trận (d×0) với vector rỗng
          if (all_missing) {
            smo_muo <- numeric(num_miss)   # không có obs → đóng góp = 0
            smo_xo  <- numeric(num_miss)
          } else {
            muo     <- pMiu[i, obs_j]
            smo_muo <- as.vector(smo %*% muo)
            smo_xo  <- as.vector(smo %*% xo)
          }

          smm_sum  <- smm_sum  + gji * smm
          sigu_sum <- sigu_sum + gji * (smo_muo + as.vector(smm %*% mum))
          smox_sum <- smox_sum + gji * smo_xo
        }

        # Symmetrize (khớp MATLAB: smm_sum = (smm_sum + smm_sum')/2)
        smm_sum <- (smm_sum + t(smm_sum)) / 2

        # Nghiệm giải tích Eq. (14)
        xm_new <- tryCatch(
          solve(smm_sum, sigu_sum - smox_sum),
          error = function(e) {
            solve(smm_sum + 1e-6 * diag(num_miss), sigu_sum - smox_sum)
          }
        )
        X[j, miss_j] <- xm_new
      }
    }

    iters_done <- iter
  }

  # ── Gán nhãn cụm (argmax gamma) ──────────────────────────────────────────
  labels <- apply(pGamma, 1, which.max)

  list(
    labels       = labels,
    gamma        = pGamma,
    mu           = pMiu,
    Sigma        = pSigma,
    alpha        = pPi,
    X_imputed    = X,
    loglik_trace = loglik_trace[seq_len(iters_done)],
    iters        = iters_done
  )
}
