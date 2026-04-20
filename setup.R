# setup.R
# Run this once to verify and prepare your R environment.
# Usage: source("setup.R")  OR  Rscript setup.R

cat("══ GMM Incomplete Data — Environment Setup ══\n\n")

# ── R version check ───────────────────────────────────────────────────────────
r_ver <- as.numeric(paste0(R.version$major, ".", R.version$minor))
cat(sprintf("R version   : %s\n", R.version.string))
if (r_ver < 4.2) {
  warning("R >= 4.2.0 is required (native pipe |> is used). Please upgrade R.")
} else {
  cat("R version   : OK (>= 4.2.0 required)\n")
}

# ── Base packages (always available, no install needed) ───────────────────────
base_pkgs <- c("parallel", "stats", "utils", "datasets")
cat("\nBase packages (no install needed):\n")
for (p in base_pkgs) {
  v <- packageVersion(p)
  cat(sprintf("  %-12s %s\n", p, v))
}

# ── Optional packages ─────────────────────────────────────────────────────────
cat("\nOptional packages:\n")
if (requireNamespace("aricode", quietly = TRUE)) {
  cat(sprintf("  %-12s %s  [installed — faster NMI]\n",
              "aricode", packageVersion("aricode")))
} else {
  cat("  aricode      NOT installed — built-in NMI will be used (slower but correct)\n")
  ans <- if (interactive()) {
    readline("  Install aricode now? [y/N]: ")
  } else "n"
  if (tolower(trimws(ans)) == "y") {
    install.packages("aricode", repos = "https://cloud.r-project.org")
    cat("  aricode installed.\n")
  }
}

# ── Platform info ─────────────────────────────────────────────────────────────
cat(sprintf("\nPlatform    : %s\n", .Platform$OS.type))
cat(sprintf("OS          : %s\n", Sys.info()["sysname"]))
cat(sprintf("CPU cores   : %d\n", parallel::detectCores()))

cat("\n══ Setup complete. To run experiments: ══\n")
cat("  setwd(\"experiments\")\n")
cat("  source(\"run_iris.R\")   # quick verify (~5-10 min)\n")
cat("  source(\"run_glass.R\")  # glass dataset\n")
cat("  DATASET <- \"iris\"; source(\"plot_results.R\")  # visualize\n\n")
