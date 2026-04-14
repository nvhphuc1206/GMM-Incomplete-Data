# regem.R
# R translation of Schneider (2001) regularized EM imputation for MVN data.
# Translates: regem.m, mridge.m, gcvridge.m, gcvfctn.m from MATLAB source.
#
# Main entry point:
#   regem_r(X)  →  completed matrix (same dims, no NAs)
#
# Key parameters (matching MATLAB defaults):
#   max_iter  = 10     (MATLAB default)
#   stagtol   = 5e-2   (convergence: 5% relative change of imputed values)
#   inflation = 1      (no residual covariance inflation)
#   relvar_res= 5e-2   (lower bound for GCV: minimum relative residual variance)
#
# Reference:
#   Schneider, T. (2001). Analysis of incomplete climate data: Estimation of
#   mean values and covariance matrices and imputation of missing values.
#   Journal of Climate, 14, 853–871.

# ── GCV criterion function (gcvfctn.m) ───────────────────────────────────────
# G(h) = [sum(g^2 * fc2) + trS0] / (dof0 + sum(g))^2
# where g = h^2 / (d + h^2)  (filter factors)
.gcvfctn <- function(h, d, fc2, trS0, dof0) {
  filfac <- h^2 / (d + h^2)
  (sum(filfac^2 * fc2) + trS0) / (dof0 + sum(filfac))^2
}

# ── Find optimal h via GCV (gcvridge.m) ──────────────────────────────────────
# F_mat : r×py Fourier coefficient matrix
# d     : r positive eigenvalues of Cxx (largest first)
# trS0  : trace of S0 (residual covariance, h-independent part)
# n_dof : degrees of freedom (n-1)
# r     : number of positive eigenvalues used
# trSmin: lower bound for trace(S_h) (= relvar_res * trace(Cyy))
.gcvridge <- function(F_mat, d, trS0, n_dof, r, trSmin,
                      relvar_res = 5e-2, minvarfrac = 0) {
  d   <- d[seq_len(r)]
  fc2 <- rowSums(F_mat[seq_len(r), , drop = FALSE]^2)
  h_tol <- 0.2 / sqrt(max(n_dof, 2L))

  # Upper bound on h
  if (minvarfrac > 0) {
    varfrac <- cumsum(d) / sum(d)
    if (minvarfrac > min(varfrac)) {
      d_max <- approx(varfrac, d, xout = minvarfrac)$y
      h_max <- sqrt(max(d_max, .Machine$double.eps))
    } else {
      h_max <- sqrt(max(d)) / h_tol
    }
  } else {
    h_max <- sqrt(max(d)) / h_tol
  }

  # Lower bound on h
  if (trS0 >= trSmin) {
    h_min <- sqrt(.Machine$double.eps)
  } else if (r == 1L) {
    h_min <- sqrt(max(d[1L] / max(n_dof, 1L), .Machine$double.eps))
  } else {
    # Find TSVD truncation level closest to trSmin
    rtsvd    <- numeric(r)
    rtsvd[r] <- trS0
    for (j in (r - 1L):1L) rtsvd[j] <- rtsvd[j + 1L] + fc2[j + 1L]
    rmin  <- which.min(abs(rtsvd - trSmin))
    h_min <- sqrt(max(d[rmin], min(d) / max(n_dof, 1L), .Machine$double.eps))
  }

  if (h_min < h_max) {
    res <- tryCatch(
      optimize(
        function(h) .gcvfctn(h, d, fc2, trS0, max(n_dof - r, 1L)),
        interval = c(h_min, h_max),
        tol      = h_tol
      ),
      error = function(e) list(minimum = h_min)
    )
    res$minimum
  } else {
    h_min
  }
}

# ── Multiple ridge regression with GCV (mridge.m) ────────────────────────────
# Cxx : pa×pa covariance of observed variables
# Cyy : pm×pm covariance of missing variables
# Cxy : pa×pm cross-covariance (obs → miss)
# dof : degrees of freedom for covariance estimation (n-1)
#
# Returns list(B, S, h, peff):
#   B    : pa×pm regression coefficient matrix
#   S    : pm×pm residual covariance
#   h    : selected ridge parameter
#   peff : effective number of parameters
.mridge_r <- function(Cxx, Cyy, Cxy, dof, relvar_res = 5e-2, minvarfrac = 0) {
  px   <- nrow(Cxx); py <- ncol(Cxy)
  rmax <- min(dof, px)

  # Eigendecomposition of Cxx — keep positive eigenvalues only
  eig  <- eigen(Cxx, symmetric = TRUE)
  thr  <- .Machine$double.eps * max(abs(eig$values[1L]), 1) * px
  r    <- max(1L, min(sum(eig$values > thr), rmax))
  V    <- eig$vectors[, seq_len(r), drop = FALSE]         # pa×r
  d    <- pmax(eig$values[seq_len(r)], .Machine$double.eps)

  # Fourier coefficients F = diag(1/sqrt(d)) %*% t(V) %*% Cxy  (r×py)
  F_mat <- sweep(t(V) %*% Cxy, 1L, 1 / sqrt(d), `*`)

  # Generic (h-independent) part of residual covariance
  S0 <- if (dof > r) Cyy - t(F_mat) %*% F_mat else matrix(0, py, py)
  S0 <- (S0 + t(S0)) / 2                       # enforce symmetry
  trS0   <- max(sum(diag(S0)), 0)
  trSmin <- relvar_res * max(sum(diag(Cyy)), .Machine$double.eps)

  # Optimal ridge parameter via GCV
  h <- .gcvridge(F_mat, d, trS0, dof, r, trSmin, relvar_res, minvarfrac)

  # Regression coefficients: B = V %*% diag(sqrt(d)/(d+h^2)) %*% F  (pa×py)
  B <- V %*% sweep(F_mat, 1L, sqrt(d) / (d + h^2), `*`)

  # Residual covariance: S = S0 + F' %*% diag(h^4/(d+h^2)^2) %*% F  (py×py)
  S <- S0 + t(F_mat) %*% sweep(F_mat, 1L, h^4 / (d + h^2)^2, `*`)
  S <- (S + t(S)) / 2

  peff <- sum(d / (d + h^2))
  list(B = B, S = S, h = h, peff = peff)
}

# ── Main regem function (regem.m) ─────────────────────────────────────────────
#
# X        : n×p matrix with NAs at missing positions
# max_iter : max EM iterations (default 10, matches MATLAB)
# stagtol  : convergence: quit when relative change of imputed values ≤ stagtol
#            (default 5e-2 = 5%, matches MATLAB)
# inflation: inflate residual covariance (1 = no inflation, MATLAB default)
# relvar_res, minvarfrac: GCV tuning parameters (MATLAB defaults: 5e-2, 0)
#
# Returns: completed matrix (same dims as X, no NAs)
regem_r <- function(X, max_iter = 10L, stagtol = 5e-2,
                    inflation = 1, relvar_res = 5e-2, minvarfrac = 0) {
  n <- nrow(X); p <- ncol(X)
  if (n < 2L || p < 1L) return(X)
  dofC <- n - 1L

  miss_orig <- is.na(X)
  if (!any(miss_orig)) return(X)

  # Pre-compute per-row obs / miss indices
  obs_idx  <- lapply(seq_len(n), function(i) which(!miss_orig[i, ]))
  miss_idx <- lapply(seq_len(n), function(i) which( miss_orig[i, ]))

  # ── Initialise: fill missing with column means, then center ─────────────────
  col_means <- colMeans(X, na.rm = TRUE)
  col_means[is.nan(col_means)] <- 0

  Xc <- X
  for (j in seq_len(p)) {
    na_j <- which(miss_orig[, j])
    if (length(na_j)) Xc[na_j, j] <- col_means[j]
  }
  M  <- colMeans(Xc)               # accumulated mean (original scale)
  Xc <- sweep(Xc, 2L, M, `-`)     # center
  C  <- t(Xc) %*% Xc / dofC       # initial covariance

  # ── EM iterations ────────────────────────────────────────────────────────────
  for (it in seq_len(max_iter)) {

    # Scale X and C to unit variance (operate in correlation-matrix space)
    D   <- sqrt(diag(C))
    D[D < .Machine$double.eps] <- 1
    Xsc     <- sweep(Xc, 2L, D, `/`)                        # scaled X
    Csc     <- sweep(sweep(C, 1L, D, `/`), 2L, D, `/`)      # correlation C

    CovRes  <- matrix(0, p, p)   # accumulated residual covariance
    Xms_new <- Xsc               # will hold imputed values (scaled coords)

    for (j in seq_len(n)) {
      mi <- miss_idx[[j]]
      if (length(mi) == 0L) next       # fully observed row: skip
      oi <- obs_idx[[j]]
      if (length(oi) == 0L) next       # fully missing row: leave at 0 (=mean)

      reg <- tryCatch(
        .mridge_r(
          Csc[oi, oi, drop = FALSE],
          Csc[mi, mi, drop = FALSE],
          Csc[oi, mi, drop = FALSE],
          dofC, relvar_res, minvarfrac
        ),
        error = function(e) NULL
      )
      if (is.null(reg)) next

      # Impute missing values in scaled coordinates
      # X[j,obs] %*% B  → (1×pa) × (pa×pm) = 1×pm
      Xms_new[j, mi] <- as.vector(Xsc[j, oi, drop = FALSE] %*% reg$B)

      # Accumulate residual covariance (scaled)
      S <- inflation * reg$S
      CovRes[mi, mi] <- CovRes[mi, mi] + S
    }

    # Rescale back to Xc coordinate (undo unit-variance scaling)
    Xsc2   <- sweep(Xms_new, 2L, D, `*`)
    CovRes <- sweep(sweep(CovRes, 1L, D, `*`), 2L, D, `*`)

    # Convergence: relative RMS change in imputed values
    rdXmis <- {
      nv <- Xsc2[miss_orig]; ov <- Xc[miss_orig]
      dv <- sqrt(sum((nv - ov)^2))
      nm <- sqrt(sum(ov^2))
      if (nm < .Machine$double.eps) Inf else dv / nm
    }

    # Update Xc and re-center
    Xc  <- Xsc2
    Mup <- colMeans(Xc)
    Xc  <- sweep(Xc, 2L, Mup, `-`)
    M   <- M + Mup

    # Update covariance estimate
    C <- (t(Xc) %*% Xc + CovRes) / dofC

    if (rdXmis <= stagtol) break
  }

  # Return in original (non-centered) scale
  sweep(Xc, 2L, M, `+`)
}
