# experiment_runner.R
# Shared experiment infrastructure for all dataset scripts.
# Usage: source this file, then call run_experiment(cfg).
#
# cfg list fields (required):
#   $dataset_name   : character — "iris", "seeds", "wine", ...
#   $X_orig         : n×d numeric matrix (already scaled/centered as needed)
#   $labels         : integer vector length n (true cluster labels)
#   $k              : integer — number of clusters
#   $results_dir    : path to per-dataset results directory (will be created)
#   $paper_expected : named list / NULL
#       $acc   : named numeric, e.g. c(Proposed=84.4, Mean=61.3, ...)
#       $ratios: "10-70%" or "all"
#
# cfg list fields (optional, have defaults):
#   $missing_ratios : default c(0.1,0.2,0.3,0.4,0.5,0.6,0.7)
#   $paper_ratios   : ratios used when computing paper comparison (default = missing_ratios)
#   $n_inits        : default 50
#   $n_patterns     : default 20
#   $seed_base      : default 42L
#   $km_nstart      : default 3L
#   $use_parallel   : default TRUE
#   $n_cores        : default detectCores()-1
#   $quick_mode     : default FALSE (5 pat × 10 inits)
#   $secs_per_run   : default 0.05 (seconds per single GMM run, for ETA)

# ── Helpers ──────────────────────────────────────────────────────────────────

.fmt_elapsed <- function(sec) {
  if (sec < 60)    return(sprintf("%.1fs", sec))
  if (sec < 3600)  return(sprintf("%.1f min", sec / 60))
  sprintf("%.1f h", sec / 3600)
}

.fmt_eta <- function(elapsed_sec, done, total) {
  if (done == 0) return("--")
  .fmt_elapsed(elapsed_sec / done * (total - done))
}

.make_logger <- function(log_file) {
  function(..., level = "INFO", console = TRUE) {
    ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg <- sprintf("[%s] [%-5s] %s", ts, level, paste0(...))
    if (console) { cat(msg, "\n"); flush.console() }
    cat(msg, "\n", file = log_file, append = TRUE)
    invisible(msg)
  }
}

# ── Pattern management ───────────────────────────────────────────────────────

.load_or_gen_patterns <- function(patterns_file, missing_ratios, n_patterns,
                                   X_orig, seed_base, n, d) {
  if (file.exists(patterns_file)) {
    saved <- tryCatch(readRDS(patterns_file), error = function(e) NULL)
    if (!is.null(saved) &&
        isTRUE(saved$meta$N_PATTERNS == n_patterns) &&
        isTRUE(length(saved$meta$MISSING_RATIOS) == length(missing_ratios)) &&
        isTRUE(all(saved$meta$MISSING_RATIOS == missing_ratios)) &&
        isTRUE(saved$meta$n == n) &&
        isTRUE(saved$meta$d == d)) {
      return(list(patterns = saved$patterns, loaded = TRUE))
    }
  }
  pats <- lapply(missing_ratios, function(ratio) {
    lapply(seq_len(n_patterns), function(pat) {
      seed_pat <- seed_base + pat * 1000L + round(ratio * 100)
      X_miss   <- generate_missing(X_orig, ratio, seed = seed_pat)
      is.na(X_miss)
    })
  })
  names(pats) <- as.character(missing_ratios)
  saveRDS(list(
    meta     = list(N_PATTERNS    = n_patterns,
                    MISSING_RATIOS = missing_ratios,
                    n = n, d = d, SEED_BASE = seed_base),
    patterns = pats
  ), patterns_file)
  list(patterns = pats, loaded = FALSE)
}

# ── Per-pattern worker (dataset-agnostic) ────────────────────────────────────
# args fields: pat, ratio, X_orig, labels, k, n, d,
#              N_INITS, SEED_BASE, KM_NSTART, miss_mat (or NULL)

.run_one_pattern <- function(args) {
  pat       <- args$pat
  ratio     <- args$ratio
  X_orig    <- args$X_orig
  labels    <- args$labels
  k         <- args$k;       n <- args$n; d <- args$d
  N_INITS   <- args$N_INITS
  SEED_BASE <- args$SEED_BASE
  KM_NSTART <- args$KM_NSTART

  METHODS    <- c("Proposed", "Mean", "Zero", "EM", "DK_Mean", "DK_Zero", "DK_EM")
  METRICS    <- c("ACC", "NMI", "Fscore", "PUR")
  miss_empty <- matrix(FALSE, n, d)

  res_pat <- lapply(METHODS, function(m)
    matrix(NA_real_, N_INITS, 4L, dimnames = list(NULL, METRICS)))
  names(res_pat) <- METHODS

  if (!is.null(args$miss_mat)) {
    miss_mat <- args$miss_mat
    X_miss   <- X_orig
    X_miss[miss_mat] <- NA_real_
  } else {
    seed_pat <- SEED_BASE + pat * 1000L + round(ratio * 100)
    X_miss   <- generate_missing(X_orig, ratio, seed = seed_pat)
    miss_mat <- is.na(X_miss)
  }
  fills <- prepare_all_fillings(X_miss)

  for (init_i in seq_len(N_INITS)) {
    seed_i <- SEED_BASE + pat * 1000L + round(ratio * 100) * 100L + init_i

    # ── Proposed GMM (handles missing directly in E-step) ──────────────────
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_em, centers = k, nstart = KM_NSTART, iter.max = 100)$centers
      result <- gmm_incomplete(fills$data_em, k, miss_mat, km_c, 500, 1e-4)
      res_pat$Proposed[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # ── GMM + Mean imputation ───────────────────────────────────────────────
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_mean, centers = k, nstart = KM_NSTART, iter.max = 100)$centers
      result <- gmm_incomplete(fills$data_mean, k, miss_empty, km_c, 500, 1e-4)
      res_pat$Mean[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # ── GMM + Zero imputation ───────────────────────────────────────────────
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_zero, centers = k, nstart = KM_NSTART, iter.max = 100)$centers
      result <- gmm_incomplete(fills$data_zero, k, miss_empty, km_c, 500, 1e-4)
      res_pat$Zero[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # ── GMM + EM imputation (regem) ─────────────────────────────────────────
    tryCatch({
      set.seed(seed_i)
      km_c   <- kmeans(fills$data_em, centers = k, nstart = KM_NSTART, iter.max = 100)$centers
      result <- gmm_incomplete(fills$data_em, k, miss_empty, km_c, 500, 1e-4)
      res_pat$EM[init_i, ] <- compute_metrics(labels, result$labels)
    }, error = function(e) NULL)

    # ── DK + Mean (Dynamic K-means with mean-filled init) ──────────────────
    tryCatch({
      set.seed(seed_i)
      dk_lab <- dk_kmeans(fills$data_mean, k, miss_mat)
      res_pat$DK_Mean[init_i, ] <- compute_metrics(labels, dk_lab)
    }, error = function(e) NULL)

    # ── DK + Zero ───────────────────────────────────────────────────────────
    tryCatch({
      set.seed(seed_i)
      dk_lab <- dk_kmeans(fills$data_zero, k, miss_mat)
      res_pat$DK_Zero[init_i, ] <- compute_metrics(labels, dk_lab)
    }, error = function(e) NULL)

    # ── DK + EM ─────────────────────────────────────────────────────────────
    tryCatch({
      set.seed(seed_i)
      dk_lab <- dk_kmeans(fills$data_em, k, miss_mat)
      res_pat$DK_EM[init_i, ] <- compute_metrics(labels, dk_lab)
    }, error = function(e) NULL)
  }
  res_pat
}

# ── Two-level aggregation ────────────────────────────────────────────────────
# Bước 1: per-pattern mean over N_INITS → one row per pattern
# Bước 2: mean ± SD over N_PATTERNS (khớp cách paper tính SD)

.aggregate_ratio <- function(pat_results, methods, n_patterns, n_inits, .log) {
  lapply(methods, function(m) {
    pat_means <- do.call(rbind, lapply(pat_results, function(p) {
      rows <- p[[m]]
      rows <- rows[stats::complete.cases(rows), , drop = FALSE]
      if (nrow(rows) == 0) return(rep(NA_real_, 4L))
      colMeans(rows)
    }))
    colnames(pat_means) <- c("ACC", "NMI", "Fscore", "PUR")
    pat_means <- pat_means[stats::complete.cases(pat_means), , drop = FALSE]
    n_valid   <- nrow(pat_means)
    if (n_valid == 0) {
      if (!is.null(.log))
        .log(sprintf("  WARNING: method=%s — 0 valid patterns!", m), level = "WARN")
      return(rep(NA_real_, 9L))
    }
    c(ACC_mean = mean(pat_means[, 1]), ACC_sd = sd(pat_means[, 1]),
      NMI_mean = mean(pat_means[, 2]), NMI_sd = sd(pat_means[, 2]),
      F_mean   = mean(pat_means[, 3]), F_sd   = sd(pat_means[, 3]),
      PUR_mean = mean(pat_means[, 4]), PUR_sd = sd(pat_means[, 4]),
      n_valid  = n_valid)
  }) |> stats::setNames(methods)
}

.agg_across_ratios <- function(results_by_ratio, ratios, methods) {
  lapply(methods, function(m) {
    vals <- sapply(as.character(ratios), function(r) {
      v <- results_by_ratio[[r]][[m]]
      v[c("ACC_mean", "ACC_sd", "NMI_mean", "NMI_sd",
          "F_mean",   "F_sd",   "PUR_mean", "PUR_sd")]
    })
    rowMeans(vals, na.rm = TRUE)
  }) |> stats::setNames(methods)
}

# ── Main entry point ─────────────────────────────────────────────────────────

run_experiment <- function(cfg) {

  # ── Unpack config with defaults ──────────────────────────────────────────
  ds_name        <- cfg$dataset_name
  X_orig         <- cfg$X_orig
  labels         <- cfg$labels
  k              <- cfg$k
  results_dir    <- cfg$results_dir
  paper_expected <- cfg$paper_expected   # may be NULL

  missing_ratios <- cfg$missing_ratios %||% c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  paper_ratios   <- cfg$paper_ratios   %||% missing_ratios
  quick_mode     <- cfg$quick_mode     %||% FALSE
  n_inits        <- cfg$n_inits        %||% (if (quick_mode) 10L else 50L)
  n_patterns     <- cfg$n_patterns     %||% (if (quick_mode)  5L else 20L)
  seed_base      <- cfg$seed_base      %||% 42L
  km_nstart      <- cfg$km_nstart      %||% 3L
  use_parallel   <- cfg$use_parallel   %||% TRUE
  n_cores        <- cfg$n_cores        %||% max(1L, parallel::detectCores() - 1L)
  secs_per_run   <- cfg$secs_per_run   %||% 0.05

  n <- nrow(X_orig); d <- ncol(X_orig)
  METHODS <- c("Proposed", "Mean", "Zero", "EM", "DK_Mean", "DK_Zero", "DK_EM")

  # ── Dirs ────────────────────────────────────────────────────────────────
  log_dir  <- file.path(results_dir, "logs")
  for (dir_ in c(results_dir, log_dir))
    if (!dir.exists(dir_)) dir.create(dir_, recursive = TRUE)

  run_tag  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, sprintf("%s_%s.log", ds_name, run_tag))
  .log     <- .make_logger(log_file)

  # ── Fixed patterns ───────────────────────────────────────────────────────
  pat_file   <- file.path(results_dir, sprintf("%s_patterns.rds", ds_name))
  pat_result <- .load_or_gen_patterns(pat_file, missing_ratios, n_patterns,
                                      X_orig, seed_base, n, d)
  all_patterns <- pat_result$patterns

  # ── ETA estimate ─────────────────────────────────────────────────────────
  total_runs  <- length(missing_ratios) * n_patterns * n_inits
  n_workers   <- if (use_parallel) min(n_cores, n_patterns) else 1L
  est_seq_min <- total_runs * secs_per_run * length(METHODS) / 60
  est_par_min <- est_seq_min / n_workers

  # ── Header ───────────────────────────────────────────────────────────────
  .log(strrep("═", 58))
  .log(sprintf("  GMM Incomplete Data — %s Experiment", toupper(ds_name)))
  .log(strrep("═", 58))
  .log(sprintf("Dataset   : %s (n=%d, d=%d, k=%d)", toupper(ds_name), n, d, k))
  .log(sprintf("Mode      : %s | %d patterns × %d inits × %d ratios = %s runs",
    if (quick_mode) "QUICK" else "FULL",
    n_patterns, n_inits, length(missing_ratios),
    format(total_runs * length(METHODS), big.mark = ",")))
  .log(sprintf("Parallel  : %s (%d workers)", if (use_parallel) "YES" else "NO", n_workers))
  .log(sprintf("KM_NSTART : %d | EM fill: regem (Schneider 2001, GCV ridge)", km_nstart))
  .log(sprintf("Patterns  : %s (%d×%d)",
    if (pat_result$loaded) "loaded from file" else "generated + saved",
    n_patterns, length(missing_ratios)))
  .log(sprintf("Est. time : ~%.0f min (seq) / ~%.0f min (par)",
    ceiling(est_seq_min), ceiling(est_par_min)))
  .log(sprintf("Log file  : %s", log_file))
  .log(strrep("─", 58))

  # ── Self-test ────────────────────────────────────────────────────────────
  tryCatch({
    m <- compute_metrics(c(1,1,2,2,3,3), c(2,2,1,1,3,3))
    stopifnot(abs(m["ACC"] - 1.0) < 1e-9)
  }, error = function(e) stop("compute_metrics() lỗi: ", e$message))

  # ── Cluster setup ────────────────────────────────────────────────────────
  cl <- NULL
  R_DIR_local <- R_DIR   # captured from parent env where sources were loaded
  if (use_parallel && n_cores > 1L) {
    cl <- parallel::makeCluster(n_workers)
    parallel::clusterExport(cl, c("R_DIR_local", "all_patterns"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      source(file.path(R_DIR_local, "data_utils.R"))
      source(file.path(R_DIR_local, "gmm_incomplete.R"))
      source(file.path(R_DIR_local, "regem.R"))
      source(file.path(R_DIR_local, "imputation_baseline.R"))
      source(file.path(R_DIR_local, "dk_kmeans.R"))
      source(file.path(R_DIR_local, "evaluation.R"))
      NULL
    })
    parallel::clusterExport(cl, ".run_one_pattern", envir = environment())
    .log(sprintf("Cluster ready: %d workers", n_workers))
  }
  on.exit({ if (!is.null(cl)) parallel::stopCluster(cl) }, add = TRUE)

  # ── Main loop ────────────────────────────────────────────────────────────
  results_by_ratio <- list()
  t_total_start    <- proc.time()["elapsed"]
  ratio_elapsed    <- numeric(length(missing_ratios))

  for (ri in seq_along(missing_ratios)) {
    ratio    <- missing_ratios[ri]
    pct      <- round(ratio * 100)
    t_ratio0 <- proc.time()["elapsed"]

    elapsed_so_far <- t_ratio0 - t_total_start
    .log(sprintf("── Ratio %3d%% [%d/%d] | elapsed %s | ETA %s",
      pct, ri, length(missing_ratios),
      .fmt_elapsed(elapsed_so_far),
      .fmt_eta(elapsed_so_far, ri - 1L, length(missing_ratios))))

    ratio_str    <- as.character(ratio)
    pattern_args <- lapply(seq_len(n_patterns), function(pat) list(
      pat      = pat,    ratio    = ratio,
      X_orig   = X_orig, labels   = labels,
      k = k, n = n, d = d,
      N_INITS   = n_inits, SEED_BASE = seed_base, KM_NSTART = km_nstart,
      miss_mat  = if (!is.null(all_patterns[[ratio_str]])) all_patterns[[ratio_str]][[pat]] else NULL
    ))

    if (!is.null(cl)) {
      # ── Parallel ─────────────────────────────────────────────────────────
      pat_results <- parallel::parLapply(cl, pattern_args, .run_one_pattern)
      cat(sprintf("  [%s] Ratio %3d%% — %d patterns done (parallel)\n",
        format(Sys.time(), "%H:%M:%S"), pct, n_patterns))
      flush.console()
    } else {
      # ── Sequential with live progress ────────────────────────────────────
      pat_results <- vector("list", n_patterns)
      for (pat in seq_len(n_patterns)) {
        pat_results[[pat]] <- .run_one_pattern(pattern_args[[pat]])

        done_rows <- do.call(rbind, lapply(
          pat_results[seq_len(pat)][!sapply(pat_results[seq_len(pat)], is.null)],
          function(x) x$Proposed))
        done_rows <- done_rows[stats::complete.cases(done_rows), , drop = FALSE]
        run_acc   <- if (nrow(done_rows) > 0) mean(done_rows[, "ACC"]) * 100 else NA

        cat(sprintf("\r  Pattern %2d/%-2d | inits %d | Proposed ACC so far: %s%%   ",
          pat, n_patterns, n_inits,
          if (is.na(run_acc)) " -- " else sprintf("%5.1f", run_acc)))
        flush.console()
      }
      cat("\n")
    }

    # ── Aggregate ────────────────────────────────────────────────────────
    ratio_summary <- .aggregate_ratio(pat_results, METHODS, n_patterns, n_inits, .log)
    results_by_ratio[[ratio_str]] <- ratio_summary

    # ── Print ratio result ───────────────────────────────────────────────
    ratio_elapsed[ri] <- proc.time()["elapsed"] - t_ratio0
    v   <- ratio_summary$Proposed
    .log(sprintf(
      "  Proposed: ACC=%5.1f±%4.1f%% | NMI=%5.1f±%4.1f%% | F=%5.1f±%4.1f%% | PUR=%5.1f±%4.1f%% | n=%d | %s",
      v["ACC_mean"]*100, v["ACC_sd"]*100,
      v["NMI_mean"]*100, v["NMI_sd"]*100,
      v["F_mean"]*100,   v["F_sd"]*100,
      v["PUR_mean"]*100, v["PUR_sd"]*100,
      v["n_valid"], .fmt_elapsed(ratio_elapsed[ri])))

    # Log all methods (EM + DK_EM to console, others to file only)
    for (m in METHODS[-1]) {
      vv <- ratio_summary[[m]]
      .log(sprintf("  %-12s ACC=%5.1f±%4.1f%%  NMI=%5.1f±%4.1f%%",
        paste0(m, ":"), vv["ACC_mean"]*100, vv["ACC_sd"]*100,
        vv["NMI_mean"]*100, vv["NMI_sd"]*100),
        console = (m %in% c("EM", "DK_EM")))
    }
  }

  # ── Timing summary ───────────────────────────────────────────────────────
  total_elapsed <- proc.time()["elapsed"] - t_total_start
  .log(strrep("─", 58))
  .log(sprintf("Total elapsed: %s (avg %s/ratio)",
    .fmt_elapsed(total_elapsed), .fmt_elapsed(mean(ratio_elapsed))))

  # ── Per-ratio summary table ───────────────────────────────────────────────
  .log("\n══ PER-RATIO RESULTS (Proposed) ══")
  hdr <- sprintf("%-6s  %-14s  %-14s  %-14s  %-14s  %s",
    "Ratio%", "ACC (%)", "NMI (%)", "F-score (%)", "PUR (%)", "Time")
  .log(hdr); .log(strrep("-", nchar(hdr)))
  for (ri in seq_along(missing_ratios)) {
    r <- missing_ratios[ri]
    v <- results_by_ratio[[as.character(r)]]$Proposed
    .log(sprintf("%-6.0f  %5.1f ± %-5.1f  %5.1f ± %-5.1f  %5.1f ± %-5.1f  %5.1f ± %-5.1f  %s",
      r * 100,
      v["ACC_mean"]*100, v["ACC_sd"]*100,
      v["NMI_mean"]*100, v["NMI_sd"]*100,
      v["F_mean"]*100,   v["F_sd"]*100,
      v["PUR_mean"]*100, v["PUR_sd"]*100,
      .fmt_elapsed(ratio_elapsed[ri])))
  }

  # ── Table 2 comparison ───────────────────────────────────────────────────
  agg_paper <- .agg_across_ratios(results_by_ratio, paper_ratios, METHODS)

  .log(sprintf("\n══ SO SÁNH TABLE 2 — Trung bình %s ══",
    paste0(round(range(paper_ratios)*100), "%", collapse = "-")))
  .log(sprintf("%-12s  %13s  %13s  %13s  %13s",
    "Method", "ACC (%)", "NMI (%)", "F-score (%)", "PUR (%)"))
  .log(strrep("-", 64))
  for (m in METHODS) {
    v <- agg_paper[[m]]
    .log(sprintf("%-12s  %5.1f ± %-4.1f  %5.1f ± %-4.1f  %5.1f ± %-4.1f  %5.1f ± %-4.1f",
      m,
      v["ACC_mean"]*100, v["ACC_sd"]*100,
      v["NMI_mean"]*100, v["NMI_sd"]*100,
      v["F_mean"]*100,   v["F_sd"]*100,
      v["PUR_mean"]*100, v["PUR_sd"]*100))
  }

  # ── Paper comparison ─────────────────────────────────────────────────────
  if (!is.null(paper_expected) && !is.null(paper_expected$acc)) {
    exp_acc <- paper_expected$acc
    .log(sprintf("── Expected (Table 2, %s, ACC%%) ──", toupper(ds_name)))
    exp_str <- paste(names(exp_acc), sprintf("%.1f", exp_acc), sep = "=", collapse = "  ")
    .log(paste0("  ", exp_str))
    for (m in names(exp_acc)) {
      if (!is.null(agg_paper[[m]])) {
        diff_v <- abs(agg_paper[[m]]["ACC_mean"] * 100 - exp_acc[[m]])
        tol    <- if (m == "Proposed") 3.0 else 5.0
        status <- if (diff_v <= tol) sprintf("[OK, Δ=%.1f]", diff_v) else sprintf("[Δ=%.1f, check]", diff_v)
        .log(sprintf("  %s: got %.1f%%, expected %.1f%% %s",
          m, agg_paper[[m]]["ACC_mean"]*100, exp_acc[[m]], status))
      }
    }
  }

  # ── Ratio=0 baseline ─────────────────────────────────────────────────────
  if ("0" %in% names(results_by_ratio)) {
    .log("\n── Ratio=0% (complete data baseline) ──")
    for (m in METHODS) {
      v <- results_by_ratio[["0"]][[m]]
      .log(sprintf("  %-12s: ACC=%5.1f ± %4.1f%%", m, v["ACC_mean"]*100, v["ACC_sd"]*100))
    }
  }

  # ── Save RDS ─────────────────────────────────────────────────────────────
  rds_path <- file.path(results_dir, sprintf("%s_results.rds", ds_name))
  saveRDS(results_by_ratio, rds_path)

  # ── Save TSV (long format, per-ratio) ────────────────────────────────────
  rows <- list()
  for (r in missing_ratios) {
    for (m in METHODS) {
      v <- results_by_ratio[[as.character(r)]][[m]]
      rows[[length(rows)+1]] <- data.frame(
        dataset = ds_name, method = m, missing_pct = round(r * 100),
        ACC_mean = round(v["ACC_mean"]*100, 2), ACC_sd = round(v["ACC_sd"]*100, 2),
        NMI_mean = round(v["NMI_mean"]*100, 2), NMI_sd = round(v["NMI_sd"]*100, 2),
        F_mean   = round(v["F_mean"]*100, 2),   F_sd   = round(v["F_sd"]*100, 2),
        PUR_mean = round(v["PUR_mean"]*100, 2), PUR_sd = round(v["PUR_sd"]*100, 2),
        stringsAsFactors = FALSE)
    }
  }
  tsv_per_ratio <- file.path(results_dir, sprintf("%s_per_ratio.tsv", ds_name))
  write.table(do.call(rbind, rows), tsv_per_ratio,
    sep = "\t", row.names = FALSE, quote = FALSE)

  # ── Save TSV (aggregated Table 2 format) ─────────────────────────────────
  rows_agg <- lapply(METHODS, function(m) {
    v <- agg_paper[[m]]
    data.frame(dataset = ds_name, method = m,
      ratios   = paste0(round(range(paper_ratios)*100), "%", collapse = "-"),
      ACC_mean = round(v["ACC_mean"]*100, 2), ACC_sd = round(v["ACC_sd"]*100, 2),
      NMI_mean = round(v["NMI_mean"]*100, 2), NMI_sd = round(v["NMI_sd"]*100, 2),
      F_mean   = round(v["F_mean"]*100, 2),   F_sd   = round(v["F_sd"]*100, 2),
      PUR_mean = round(v["PUR_mean"]*100, 2), PUR_sd = round(v["PUR_sd"]*100, 2),
      stringsAsFactors = FALSE)
  })
  tsv_comparison_table <- file.path(results_dir, sprintf("%s_comparison_table.tsv", ds_name))
  write.table(do.call(rbind, rows_agg), tsv_comparison_table,
    sep = "\t", row.names = FALSE, quote = FALSE)

  # ── Final log ─────────────────────────────────────────────────────────────
  .log(strrep("─", 58))
  .log(sprintf("Saved RDS : %s", rds_path))
  .log(sprintf("Saved TSV : %s", tsv_per_ratio))
  .log(sprintf("Saved TSV : %s", tsv_comparison_table))
  .log(sprintf("Patterns  : %s", pat_file))
  .log(sprintf("Log       : %s", log_file))
  .log(sprintf("DONE. Total time: %s", .fmt_elapsed(total_elapsed)))

  invisible(results_by_ratio)
}
