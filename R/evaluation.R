# evaluation.R
# Metrics: ACC, NMI, F-score, PUR
# Tái hiện: myNMIACC.m, bestMap.m, Fmeasure.m, MutualInfo.m, purFuc.m
#
# Không yêu cầu package ngoài — Hungarian và NMI được implement thuần R.

# ─────────────────────────────────────────────────────────────────────────────
# Internal: Hungarian algorithm (LSAP, minimization) — O(n^3)
#   Dịch từ thuật toán Kuhn-Munkres với potential functions
#   solve_LSAP_r(x): x là ma trận n×m cost
#   Trả về: ans[i] = cột được gán cho hàng i (1-indexed)
# ─────────────────────────────────────────────────────────────────────────────
.solve_lsap_r <- function(a) {
  nr <- nrow(a); nc_m <- ncol(a)
  n  <- nr; m <- nc_m
  if (n == 0 || m == 0) return(integer(0))

  INF <- (sum(abs(a)) + 1) * 2

  # Potentials và matching (dùng index +1 để có slot "0" tại vị trí [1])
  u_r   <- numeric(n + 1)   # u_r[i+1] = potential cho hàng i (i = 0..n)
  v_r   <- numeric(m + 1)   # v_r[j+1] = potential cho cột j (j = 0..m)
  p_r   <- integer(m + 1)   # p_r[j+1] = hàng đang matched với cột j
  way_r <- integer(m + 1)   # way_r[j+1] = cột predecessor trên đường tăng

  for (i in seq_len(n)) {
    p_r[1] <- i    # cột "dummy" 0 tạm giữ hàng i làm điểm xuất phát
    j0 <- 0L

    minv <- rep(INF, m + 1)
    used <- logical(m + 1)

    repeat {
      used[j0 + 1] <- TRUE
      i0    <- p_r[j0 + 1]   # hàng matched với cột j0
      delta <- INF
      j1    <- -1L

      for (j in seq_len(m)) {
        if (!used[j + 1]) {
          cur <- a[i0, j] - u_r[i0 + 1] - v_r[j + 1]
          if (cur < minv[j + 1]) {
            minv[j + 1] <- cur
            way_r[j + 1] <- j0
          }
          if (minv[j + 1] < delta) {
            delta <- minv[j + 1]
            j1    <- j
          }
        }
      }

      # Cập nhật potentials
      for (j in 0:m) {
        if (used[j + 1]) {
          u_r[p_r[j + 1] + 1] <- u_r[p_r[j + 1] + 1] + delta
          v_r[j + 1]           <- v_r[j + 1] - delta
        } else {
          minv[j + 1] <- minv[j + 1] - delta
        }
      }

      j0 <- j1
      if (p_r[j0 + 1] == 0L) break   # tìm được đường tăng đến cột tự do
    }

    # Augment: truy vết ngược và gán
    while (j0 != 0L) {
      p_r[j0 + 1] <- p_r[way_r[j0 + 1] + 1]
      j0 <- way_r[j0 + 1]
    }
  }

  # Xây dựng ans[i] = cột được gán cho hàng i
  ans <- integer(n)
  for (j in seq_len(m)) {
    if (p_r[j + 1] != 0L) ans[p_r[j + 1]] <- j
  }
  ans
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. Hungarian matching — khớp bestMap.m
#    Tìm ánh xạ tốt nhất từ predicted clusters → true classes
#    Trả về: pred_labels đã được permute để khớp tốt nhất true_labels
# ─────────────────────────────────────────────────────────────────────────────
best_map <- function(true_labels, pred_labels) {
  L1 <- as.integer(factor(true_labels))
  L2 <- as.integer(factor(pred_labels))

  nc1 <- max(L1); nc2 <- max(L2)
  nc  <- max(nc1, nc2)

  # G[i,j] = |{true==i} ∩ {pred==j}|  (hàng = true class, cột = pred cluster)
  G <- matrix(0L, nc, nc)
  for (i in seq_len(nc1))
    for (j in seq_len(nc2))
      G[i, j] <- sum(L1 == i & L2 == j)

  # Maximize matching ↔ minimize -G
  # mapping[i] = pred cluster được gán cho true class i
  mapping <- .solve_lsap_r(-G)

  # Cần inverse: inv_mapping[j] = true class tương ứng với pred cluster j
  inv_mapping <- integer(nc)
  for (i in seq_len(nc)) {
    if (mapping[i] >= 1L && mapping[i] <= nc)
      inv_mapping[mapping[i]] <- i
  }

  new_L2 <- integer(length(L2))
  for (j in seq_len(nc2)) {
    new_L2[L2 == j] <- inv_mapping[j]
  }
  new_L2
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. Clustering Accuracy (ACC)
# ─────────────────────────────────────────────────────────────────────────────
compute_acc <- function(true_labels, pred_labels) {
  matched <- best_map(true_labels, pred_labels)
  mean(as.integer(factor(true_labels)) == matched)
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. NMI — implement thủ công theo MutualInfo.m (không dùng aricode)
#    NMI = 2 * I(Y;C) / (H(Y) + H(C))
# ─────────────────────────────────────────────────────────────────────────────
compute_nmi <- function(true_labels, pred_labels) {
  if (requireNamespace("aricode", quietly = TRUE)) {
    return(aricode::NMI(true_labels, pred_labels))
  }
  .nmi_manual(true_labels, pred_labels)
}

.nmi_manual <- function(L1, L2) {
  n <- length(L1)
  tab     <- table(L1, L2)
  p_joint <- tab / n
  p1      <- rowSums(p_joint)
  p2      <- colSums(p_joint)

  H1 <- -sum(p1[p1 > 0] * log(p1[p1 > 0]))
  H2 <- -sum(p2[p2 > 0] * log(p2[p2 > 0]))
  if (H1 + H2 == 0) return(1.0)

  MI <- 0
  for (i in seq_len(nrow(p_joint))) {
    for (j in seq_len(ncol(p_joint))) {
      pij <- p_joint[i, j]
      if (pij > 0 && p1[i] > 0 && p2[j] > 0)
        MI <- MI + pij * log(pij / (p1[i] * p2[j]))
    }
  }
  2 * MI / (H1 + H2)
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. F-score — khớp Fmeasure.m (weighted macro F1 theo true class size)
#
#    CP[i,j] = |cluster_i ∩ true_class_j|
#    FMeasure = sum_j(Pj/N * max_i(F[i,j]))
# ─────────────────────────────────────────────────────────────────────────────
compute_fmeasure <- function(true_labels, pred_labels) {
  P <- as.integer(factor(true_labels))
  C <- as.integer(factor(pred_labels))
  N <- length(C)

  p_cls <- sort(unique(P))
  c_cls <- sort(unique(C))
  P_size <- length(p_cls)
  C_size <- length(c_cls)

  # CP[i,j] = count trong predicted cluster i thuộc true class j
  CP <- matrix(0L, C_size, P_size)
  for (i in seq_along(c_cls))
    for (j in seq_along(p_cls))
      CP[i, j] <- sum(C == c_cls[i] & P == p_cls[j])

  Pj <- colSums(CP)    # count per true class
  Ci <- rowSums(CP)    # count per pred cluster
  Ci[Ci == 0] <- 1L

  precision  <- sweep(CP, 1, Ci, "/")
  Pj_safe    <- Pj; Pj_safe[Pj_safe == 0] <- 1
  recall     <- sweep(CP, 2, Pj_safe, "/")

  denom <- precision + recall
  F     <- ifelse(denom > 0, 2 * precision * recall / denom, 0)

  best_F <- apply(F, 2, max)
  sum((Pj / sum(Pj)) * best_F)
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. Purity — khớp purFuc.m
# ─────────────────────────────────────────────────────────────────────────────
compute_purity <- function(true_labels, pred_labels) {
  tab <- table(pred_labels, true_labels)
  sum(apply(tab, 1, max)) / length(true_labels)
}

# ─────────────────────────────────────────────────────────────────────────────
# 6. Hàm tổng hợp — trả về c(ACC, NMI, Fscore, PUR) trong [0,1]
# ─────────────────────────────────────────────────────────────────────────────
compute_metrics <- function(true_labels, pred_labels) {
  matched <- best_map(true_labels, pred_labels)
  L1      <- as.integer(factor(true_labels))

  acc    <- mean(L1 == matched)
  nmi    <- compute_nmi(true_labels, pred_labels)
  fscore <- compute_fmeasure(true_labels, pred_labels)
  pur    <- compute_purity(true_labels, pred_labels)

  c(ACC = acc, NMI = nmi, Fscore = fscore, PUR = pur)
}
