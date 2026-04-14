# dk_kmeans.R
# Dynamic K-means with iterative missing-value imputation.
# Translates: kmeansfilling.m + updatefilling.m từ MATLAB gốc.
#
# Thuật toán (Zhang et al. 2021):
#   1. Khởi tạo K centroids bằng cách chọn ngẫu nhiên K hàng từ data đã fill
#   2. Gán mỗi quan sát vào centroid gần nhất (Euclidean)
#   3. Cập nhật centroids = mean của các quan sát được gán
#   4. Cập nhật giá trị missing: x[i,j] ← centroid[label_i, j]   ← điểm khác biệt với GMM
#   5. Lặp 2-4 đến khi hội tụ
#
# Tham số:
#   X_filled : n×d matrix (đã fill, không có NA) — đầu vào ban đầu
#   k        : số cluster
#   miss_mat : n×d logical (TRUE = vị trí missing gốc)
#   max_iter : số vòng lặp tối đa (mặc định 100, khớp MATLAB)
#   tol      : ngưỡng hội tụ theo relative objective change (mặc định 1e-4)
#
# Trả về: integer vector độ dài n (nhãn cluster 1..k)
#
# Ghi chú về seeding: set.seed() cần được gọi TRƯỚC khi gọi hàm này
# để đảm bảo tái tạo được. Trong run_iris.R đã có set.seed(seed_i).

dk_kmeans <- function(X_filled, k, miss_mat, max_iter = 100L, tol = 1e-4) {
  n <- nrow(X_filled); d <- ncol(X_filled)
  stopifnot(nrow(miss_mat) == n, ncol(miss_mat) == d)

  # Precompute per-row missing indices (chỉ một lần)
  miss_rows <- lapply(seq_len(n), function(i) which(miss_mat[i, ]))

  # ── Khởi tạo centroids: chọn ngẫu nhiên K hàng ───────────────────────────
  init_idx <- sample.int(n, k, replace = FALSE)
  U  <- X_filled[init_idx, , drop = FALSE]   # K×d

  # Working data: bản sao có thể update
  Xw <- X_filled

  labels   <- integer(n)
  obj_prev <- Inf

  for (iter in seq_len(max_iter)) {

    # ── Gán nhãn: nearest centroid (vectorized) ───────────────────────────
    # dist²[i,j] = ||x_i - u_j||² = ||x_i||² - 2 x_i·u_j + ||u_j||²
    D <- rowSums(Xw^2) - 2 * (Xw %*% t(U)) +
         matrix(rowSums(U^2), nrow = n, ncol = k, byrow = TRUE)
    labels <- max.col(-D, ties.method = "first")

    # ── Cập nhật centroids ────────────────────────────────────────────────
    new_U <- matrix(0, k, d)
    for (j in seq_len(k)) {
      idx <- which(labels == j)
      if (length(idx) == 0L) {
        # Empty cluster: reinit về quan sát ngẫu nhiên (tránh degenerate)
        new_U[j, ] <- Xw[sample.int(n, 1L), ]
      } else if (length(idx) == 1L) {
        new_U[j, ] <- Xw[idx, ]
      } else {
        new_U[j, ] <- colMeans(Xw[idx, , drop = FALSE])
      }
    }
    U <- new_U

    # ── Cập nhật filling: missing ← centroid của cluster hiện tại ────────
    # Khớp updatefilling.m: data(i,j) = U(label_i, j)
    for (i in seq_len(n)) {
      mi <- miss_rows[[i]]
      if (length(mi)) Xw[i, mi] <- U[labels[i], mi]
    }

    # ── Kiểm tra hội tụ: relative change của objective ───────────────────
    # Khớp MATLAB: E_in = sum_i ||x_i - U_{label_i}||  (L2, không phải L2²)
    resid <- Xw - U[labels, , drop = FALSE]
    obj   <- sum(sqrt(rowSums(resid^2)))

    if (iter >= 2L &&
        (obj_prev - obj) / max(obj, .Machine$double.eps) <= tol) break
    obj_prev <- obj
  }

  labels
}
