# plot_results.R
# Visualize per-ratio line plots (ACC, NMI, F-score, PUR) cho 4 phương pháp
# Đọc từ results/iris_results.rds (hoặc tsv), lưu ra PNG + PDF
#
# Cách dùng (trong RStudio, setwd vào experiments/):
#   source("plot_results.R")
#
# Hoặc chỉ định dataset cụ thể:
#   DATASET <- "seeds"; source("plot_results.R")

# ── Config ────────────────────────────────────────────────────────────────────
if (!exists("DATASET"))   DATASET    <- "iris"
if (!exists("SAVE_PNG"))  SAVE_PNG   <- TRUE
if (!exists("SAVE_PDF"))  SAVE_PDF   <- TRUE
if (!exists("SHOW_PLOT")) SHOW_PLOT  <- TRUE   # FALSE khi chạy batch

# ── Paths ─────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
SCRIPT_DIR <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)
RESULTS_DIR <- file.path(SCRIPT_DIR, "..", "results")
PLOTS_DIR   <- file.path(RESULTS_DIR, "plots")
if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)

# ── Load data từ RDS ──────────────────────────────────────────────────────────
rds_file <- file.path(RESULTS_DIR, sprintf("%s_results.rds", DATASET))
tsv_file <- file.path(RESULTS_DIR, sprintf("%s_per_ratio.tsv", DATASET))

if (!file.exists(rds_file) && !file.exists(tsv_file)) {
  stop(sprintf("Không tìm thấy kết quả cho dataset '%s'.\n  Kiểm tra: %s\n       hoặc: %s",
               DATASET, rds_file, tsv_file))
}

# Ưu tiên TSV (đã aggregate, dễ dùng); fallback sang RDS
if (file.exists(tsv_file)) {
  cat(sprintf("Đọc từ TSV: %s\n", tsv_file))
  df_long <- read.table(tsv_file, header = TRUE, sep = "\t",
                        stringsAsFactors = FALSE)
} else {
  cat(sprintf("Đọc từ RDS: %s\n", rds_file))
  res <- readRDS(rds_file)
  methods <- c("Proposed", "Mean", "Zero", "EM")
  rows <- list()
  for (r_str in names(res)) {
    ratio_pct <- round(as.numeric(r_str) * 100)
    for (m in methods) {
      v <- res[[r_str]][[m]]
      if (is.null(v) || all(is.na(v))) next
      rows[[length(rows)+1]] <- data.frame(
        dataset  = DATASET,
        method   = m,
        missing_pct = ratio_pct,
        ACC_mean = v["ACC_mean"] * 100, ACC_sd = v["ACC_sd"] * 100,
        NMI_mean = v["NMI_mean"] * 100, NMI_sd = v["NMI_sd"] * 100,
        F_mean   = v["F_mean"]   * 100, F_sd   = v["F_sd"]   * 100,
        PUR_mean = v["PUR_mean"] * 100, PUR_sd = v["PUR_sd"] * 100,
        row.names = NULL, stringsAsFactors = FALSE
      )
    }
  }
  df_long <- do.call(rbind, rows)
}

# Đảm bảo kiểu dữ liệu đúng
df_long$missing_pct <- as.integer(df_long$missing_pct)
df_long$method      <- factor(df_long$method,
                               levels = c("Proposed", "EM", "Zero", "Mean"))

cat(sprintf("Dataset: %s | Methods: %s | Ratios: %s\n",
  DATASET,
  paste(levels(df_long$method), collapse=", "),
  paste(sort(unique(df_long$missing_pct)), collapse=", ")))

# ── Palette & style ───────────────────────────────────────────────────────────
METHOD_COLORS <- c(
  Proposed = "#E63946",   # đỏ đậm — phương pháp đề xuất
  EM       = "#457B9D",   # xanh dương
  Zero     = "#2A9D8F",   # xanh lá
  Mean     = "#E9C46A"    # vàng
)
METHOD_LTY <- c(Proposed = 1, EM = 2, Zero = 3, Mean = 4)
METHOD_PCH <- c(Proposed = 16, EM = 17, Zero = 15, Mean = 18)

# ── Hàm vẽ một metric ────────────────────────────────────────────────────────
.plot_metric <- function(df, metric, ylab_str, ylim_pad = 5) {
  mean_col <- paste0(metric, "_mean")
  sd_col   <- paste0(metric, "_sd")

  methods  <- levels(df$method)
  ratios   <- sort(unique(df$missing_pct))

  # Y range
  all_vals <- c(df[[mean_col]] - df[[sd_col]],
                df[[mean_col]] + df[[sd_col]])
  y_min    <- max(0,   floor(min(all_vals, na.rm=TRUE) / 5) * 5 - ylim_pad)
  y_max    <- min(100, ceiling(max(all_vals, na.rm=TRUE) / 5) * 5 + ylim_pad)

  # Empty plot
  plot(NULL, xlim = range(ratios), ylim = c(y_min, y_max),
       xlab = "Missing ratio (%)", ylab = paste0(ylab_str, " (%)"),
       xaxt = "n", las = 1, cex.axis = 0.9, cex.lab = 1.0,
       main = sprintf("%s — %s", toupper(DATASET), ylab_str))
  axis(1, at = ratios)
  grid(nx = NA, ny = NULL, lty = "dotted", col = "grey85")
  abline(v = ratios, col = "grey92", lty = "dotted")

  # Plot each method: ribbon (±SD) then line then points
  for (m in methods) {
    sub <- df[df$method == m, ]
    sub <- sub[order(sub$missing_pct), ]
    if (nrow(sub) == 0) next

    col_m <- METHOD_COLORS[m]
    lty_m <- METHOD_LTY[m]
    pch_m <- METHOD_PCH[m]
    x     <- sub$missing_pct
    y     <- sub[[mean_col]]
    yup   <- y + sub[[sd_col]]
    ylo   <- y - sub[[sd_col]]

    # SD ribbon
    polygon(c(x, rev(x)), c(yup, rev(ylo)),
            col = adjustcolor(col_m, alpha.f = 0.12),
            border = NA)
    # Line
    lines(x, y, col = col_m, lty = lty_m, lwd = 2)
    # Points
    points(x, y, col = col_m, pch = pch_m, cex = 1.1, bg = col_m)
    # SD error bars
    arrows(x, ylo, x, yup, angle = 90, code = 3,
           length = 0.04, col = adjustcolor(col_m, alpha.f = 0.6), lwd = 1)
  }

  # Legend
  legend("topright", legend = methods, col = METHOD_COLORS[methods],
         lty = METHOD_LTY[methods], pch = METHOD_PCH[methods],
         lwd = 2, pt.cex = 1.1, cex = 0.85, bty = "n",
         bg = adjustcolor("white", alpha.f = 0.8))
}

# ── Vẽ 4 metrics trong 1 figure (2×2) ────────────────────────────────────────
metrics <- list(
  list(col = "ACC", label = "Accuracy (ACC)"),
  list(col = "NMI", label = "NMI"),
  list(col = "F",   label = "F-score"),
  list(col = "PUR", label = "Purity (PUR)")
)

.draw_all <- function() {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  for (mt in metrics) {
    .plot_metric(df_long, mt$col, mt$label)
  }
  mtext(sprintf("GMM with Incomplete Data — %s", toupper(DATASET)),
        outer = TRUE, cex = 1.1, font = 2, line = 0.5)
}

# ── Lưu PNG ───────────────────────────────────────────────────────────────────
if (SAVE_PNG) {
  png_path <- file.path(PLOTS_DIR, sprintf("%s_per_ratio.png", DATASET))
  png(png_path, width = 1400, height = 1100, res = 130)
  .draw_all()
  dev.off()
  cat(sprintf("Saved PNG : %s\n", png_path))
}

# ── Lưu PDF ───────────────────────────────────────────────────────────────────
if (SAVE_PDF) {
  pdf_path <- file.path(PLOTS_DIR, sprintf("%s_per_ratio.pdf", DATASET))
  pdf(pdf_path, width = 11, height = 8.5)
  .draw_all()
  dev.off()
  cat(sprintf("Saved PDF : %s\n", pdf_path))
}

# ── Hiển thị trong RStudio ────────────────────────────────────────────────────
if (SHOW_PLOT) {
  if (exists("dev.list") && length(dev.list()) > 0 &&
      names(dev.cur()) %in% c("RStudioGD", "X11", "quartz", "windows")) {
    .draw_all()
  } else {
    tryCatch({ .draw_all() }, error = function(e) NULL)
  }
}

# ── In bảng tóm tắt ───────────────────────────────────────────────────────────
cat("\n── Per-ratio ACC (%) ──\n")
acc_wide <- reshape(
  df_long[, c("method", "missing_pct", "ACC_mean", "ACC_sd")],
  timevar   = "method",
  idvar     = "missing_pct",
  direction = "wide"
)
acc_wide <- acc_wide[order(acc_wide$missing_pct), ]
print(acc_wide, row.names = FALSE, digits = 3)

cat(sprintf("\nPlots saved to: %s/\n", PLOTS_DIR))
