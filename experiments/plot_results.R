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
if (!exists("DATASET"))   DATASET    <- "vehicle"
if (!exists("SAVE_PNG"))  SAVE_PNG   <- TRUE
if (!exists("SAVE_PDF"))  SAVE_PDF   <- TRUE
if (!exists("SHOW_PLOT")) SHOW_PLOT  <- TRUE   # FALSE khi chạy batch

# ── Paths ─────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
SCRIPT_DIR <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) getwd()
)
RESULTS_DIR <- file.path(SCRIPT_DIR, "..", "results", DATASET)
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

# Thứ tự vẽ: từ thấp → cao (đường tốt hơn vẽ sau, đè lên trên)
DRAW_ORDER <- c("Mean", "Zero", "EM", "DK_Mean", "DK_Zero", "DK_EM", "Proposed")
avail      <- unique(df_long$method)
draw_order <- DRAW_ORDER[DRAW_ORDER %in% avail]
# Các method có trong data nhưng không trong DRAW_ORDER thêm vào đầu
extra      <- avail[!avail %in% draw_order]
draw_order <- c(extra, draw_order)
df_long$method <- factor(df_long$method, levels = draw_order)

cat(sprintf("Dataset: %s | Methods: %s | Ratios: %s\n",
  DATASET,
  paste(draw_order, collapse=", "),
  paste(sort(unique(df_long$missing_pct)), collapse=", ")))

# ── Nhãn hiển thị — phân biệt rõ 3 nhóm thuật toán ──────────────────────────
# Nhóm 1: GMM xử lý missing trực tiếp  (phương pháp đề xuất trong bài báo)
# Nhóm 2: K-means + imputation (DK)
# Nhóm 3: GMM + imputation trước (baselines)
METHOD_LABELS <- c(
  Proposed = "Proposed",  # thuật toán chính của bài báo
  DK_EM    = "DK + EM-fill",               # K-means với EM imputation
  DK_Zero  = "DK + Zero-fill",
  DK_Mean  = "DK + Mean-fill",
  EM       = "GMM + EM-fill",              # standard GMM sau khi impute
  Zero     = "GMM + Zero-fill",
  Mean     = "GMM + Mean-fill"
)

# ── Palette & style (3 nhóm màu khác nhau) ───────────────────────────────────
METHOD_COLORS <- c(
  Proposed = "#C1121F",   # đỏ đậm    — GMM-Incomplete (nổi bật nhất)
  DK_EM    = "#023E8A",   # navy      — DK nhóm
  DK_Zero  = "#0077B6",   # xanh biển
  DK_Mean  = "#90E0EF",   # xanh nhạt
  EM       = "#606C38",   # xanh olive — GMM baseline nhóm
  Zero     = "#BC6C25",   # nâu cam
  Mean     = "#A8A8A8"    # xám
)
# lty: solid=1 (Proposed) | dashed=2 (DK) | dotdash=4 (GMM baselines)
METHOD_LTY <- c(Proposed=1, DK_EM=2, DK_Zero=2, DK_Mean=2, EM=4, Zero=4, Mean=4)
# pch: symbols lớn, dễ phân biệt
METHOD_PCH <- c(Proposed=16, DK_EM=17, DK_Zero=15, DK_Mean=23,
                EM=21, Zero=24, Mean=22)
# lwd: Proposed dày nhất, DK vừa, baselines mảnh hơn
METHOD_LWD <- c(Proposed=3.0, DK_EM=2.0, DK_Zero=2.0, DK_Mean=2.0,
                EM=1.5, Zero=1.5, Mean=1.5)
# alpha cho ribbon: tất cả hiển thị, Proposed đậm hơn
METHOD_ALPHA <- c(Proposed=0.18, DK_EM=0.12, DK_Zero=0.12, DK_Mean=0.10,
                  EM=0.10, Zero=0.10, Mean=0.10)

# ── Hàm vẽ một metric ────────────────────────────────────────────────────────
.plot_metric <- function(df, metric, ylab_str, ylim_pad = 5) {
  mean_col <- paste0(metric, "_mean")
  sd_col   <- paste0(metric, "_sd")

  methods <- levels(df$method)
  ratios  <- sort(unique(df$missing_pct))

  # Y range bao gồm tất cả ±SD
  all_vals <- c(df[[mean_col]] - df[[sd_col]],
                df[[mean_col]] + df[[sd_col]])
  y_min <- max(0,   floor(min(all_vals, na.rm=TRUE) / 5) * 5 - ylim_pad)
  y_max <- min(100, ceiling(max(all_vals, na.rm=TRUE) / 5) * 5 + ylim_pad)

  # Khung trống
  plot(NULL, xlim = range(ratios), ylim = c(y_min, y_max),
       xlab = "Missing ratio (%)", ylab = paste0(ylab_str, " (%)"),
       xaxt = "n", las = 1, cex.axis = 1.0, cex.lab = 1.1,
       main = sprintf("%s — %s", toupper(DATASET), ylab_str),
       cex.main = 1.15, font.main = 2)
  axis(1, at = ratios, cex.axis = 1.0)
  grid(nx = NA, ny = NULL, lty = "dotted", col = "grey82")
  abline(v = ratios, col = "grey90", lty = "dotted")

  # Vẽ ribbon (±SD) cho TẤT CẢ methods — từ thấp lên cao (draw_order)
  for (m in methods) {
    sub <- df[df$method == m, ]
    sub <- sub[order(sub$missing_pct), ]
    if (nrow(sub) == 0 || !m %in% names(METHOD_COLORS)) next
    x   <- sub$missing_pct
    y   <- sub[[mean_col]]
    yup <- pmin(y + sub[[sd_col]], 100)
    ylo <- pmax(y - sub[[sd_col]], 0)
    alpha_m <- if (!is.na(METHOD_ALPHA[m])) METHOD_ALPHA[m] else 0.10
    polygon(c(x, rev(x)), c(yup, rev(ylo)),
            col    = adjustcolor(METHOD_COLORS[m], alpha.f = alpha_m),
            border = NA)
  }

  # Vẽ đường + điểm (từ thấp lên cao — Proposed vẽ sau cùng, nổi nhất)
  for (m in methods) {
    sub <- df[df$method == m, ]
    sub <- sub[order(sub$missing_pct), ]
    if (nrow(sub) == 0 || !m %in% names(METHOD_COLORS)) next
    col_m   <- METHOD_COLORS[m]
    lty_m   <- METHOD_LTY[m]
    pch_m   <- METHOD_PCH[m]
    lwd_m   <- METHOD_LWD[m]
    x <- sub$missing_pct
    y <- sub[[mean_col]]
    # Đường
    lines(x, y, col = col_m, lty = lty_m, lwd = lwd_m)
    # Điểm — filled shapes dùng bg
    points(x, y, col = col_m, pch = pch_m, cex = 1.4,
           bg = adjustcolor(col_m, alpha.f = 0.85))
  }

  # Legend — hiện phía ngoài (topright), nhóm rõ ràng
  disp <- methods[methods %in% names(METHOD_LABELS)]
  legend("topright",
         legend = METHOD_LABELS[disp],
         col    = METHOD_COLORS[disp],
         lty    = METHOD_LTY[disp],
         pch    = METHOD_PCH[disp],
         lwd    = METHOD_LWD[disp],
         pt.bg  = METHOD_COLORS[disp],
         pt.cex = 1.3, cex = 0.82, bty = "n", y.intersp = 1.1,
         bg     = adjustcolor("white", alpha.f = 0.85))
}

# ── Layout 4 metrics (2×2) ────────────────────────────────────────────────────
metrics <- list(
  list(col = "ACC", label = "Accuracy (ACC)"),
  list(col = "NMI", label = "NMI"),
  list(col = "F",   label = "F-score"),
  list(col = "PUR", label = "Purity (PUR)")
)

.draw_all <- function() {
  par(mfrow = c(2, 2),
      mar   = c(4.5, 4.5, 3.5, 1.5),   # margins rộng hơn
      oma   = c(0, 0, 2.5, 0))
  for (mt in metrics) {
    .plot_metric(df_long, mt$col, mt$label)
  }
  mtext(
    sprintf("Clustering with Incomplete Data — %s dataset  (replication of Zhang et al., 2021)",
            toupper(DATASET)),
    outer = TRUE, cex = 1.05, font = 2, line = 1.0)
}

# ── Lưu PNG — kích thước lớn hơn cho dễ đọc ─────────────────────────────────
if (SAVE_PNG) {
  png_path <- file.path(PLOTS_DIR, sprintf("%s_per_ratio.png", DATASET))
  png(png_path, width = 2000, height = 1600, res = 150)
  .draw_all()
  dev.off()
  cat(sprintf("Saved PNG : %s\n", png_path))
}

# ── Lưu PDF ───────────────────────────────────────────────────────────────────
if (SAVE_PDF) {
  pdf_path <- file.path(PLOTS_DIR, sprintf("%s_per_ratio.pdf", DATASET))
  pdf(pdf_path, width = 14, height = 11)
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
