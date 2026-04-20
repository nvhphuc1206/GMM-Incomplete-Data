# GMM with Incomplete Data — R Replication

R replication of **Zhang et al. (2021)**: *Gaussian Mixture Model Clustering with Incomplete Data*.

> Original MATLAB code: <https://github.com/Zhangyi1231/GMM-with-Incomplete-Data>

---

## Background

Standard Gaussian Mixture Models (GMM) require complete data. When observations have missing values (MCAR), naive approaches impute first and then cluster — introducing bias. Zhang et al. propose an EM algorithm that handles missing data **directly** in the E- and M-steps, marginalizing over missing dimensions without imputation.

Key implementation details (differ from the paper's description):
- `standardize_rms()` uses RMS normalization: `s = sqrt(mean(x²))`, not `sd()`
- `epsilon = 1e-4` convergence threshold (not 1e-6)
- Double E-step is applied every iteration
- GMM+Mean baseline uses **un-standardized** data

---

## Project Structure

```
gmm-incomplete-r/
├── R/
│   ├── gmm_incomplete.R       # Core Algorithm 1 — proposed method
│   ├── regem.R                # RegEM imputation (EM-fill baseline)
│   ├── imputation_baseline.R  # Mean / Zero filling baselines
│   ├── dk_kmeans.R            # Dynamic K-means with missing data
│   ├── evaluation.R           # ACC, NMI, F-score, PUR metrics
│   ├── data_utils.R           # Data loading, standardize_rms(), generate_missing()
│   └── experiment_runner.R    # Shared experiment infrastructure
│
├── experiments/
│   ├── run_iris.R             # Iris       150×4,  3 classes  (~5–10 min)
│   ├── run_seeds.R            # Seeds      210×7,  3 classes  (~10–20 min)
│   ├── run_wine.R             # Wine       178×13, 3 classes  (~15–25 min)
│   ├── run_glass.R            # Glass      214×9,  6 classes  (~1–2 min)
│   ├── run_vehicle.R          # Vehicle    846×18, 4 classes  (~2 min)
│   ├── run_alcoholqcm.R       # AlcoholQCM 125×10, 5 classes (~1 min)
│   ├── run_segment.R          # Segment    2310×18, 7 classes (~2–4 h)
│   ├── run_all.R              # Batch runner: iris + seeds + wine
│   └── plot_results.R         # 2×2 line-plot visualizer (ACC/NMI/F/PUR)
│
├── data/                      # Datasets (auto-downloaded on first run)
├── results/                   # Per-dataset output (RDS, TSV, plots, logs)
└── README.md
```

---

## Installation

This project runs on **base R only** — no required external packages.

```r
# Verify your environment (run once):
source("setup.R")
```

**R >= 4.2.0** is required (uses the native pipe `|>`).

Optional: install `aricode` for faster NMI computation (a pure-R fallback is used otherwise):
```r
install.packages("aricode")
```

---

## Datasets

| Dataset | n | d | k | Data file | Source |
|---|---|---|---|---|---|
| Iris | 150 | 4 | 3 | built-in R | `datasets` package |
| Seeds | 210 | 7 | 3 | `data/seeds_dataset.txt` | UCI — auto-downloaded |
| Wine | 178 | 13 | 3 | `data/wine.data` | UCI — auto-downloaded |
| Glass | 214 | 9 | 6 | `data/glass.data` | UCI — auto-downloaded |
| Vehicle | 846 | 18 | 4 | `data/bus.dat` + 3 others | UCI — manual download |
| AlcoholQCM | 125 | 10 | 5 | `data/AlcoholQCM.csv` | UCI — manual download |
| Segment | 2310 | 18 | 7 | `data/segment.dat` | manual download |

**Auto-downloaded datasets** (Seeds, Wine, Glass) are fetched from UCI on first run and cached to `data/` automatically.

**Manual download instructions:**

- **Vehicle** — download from [UCI Statlog Vehicle](https://archive.ics.uci.edu/dataset/149/statlog+vehicle+silhouettes),
  save the four class files (`bus.dat`, `opel.dat`, `saab.dat`, `van.dat`) to `data/`
- **AlcoholQCM** — download from [UCI AlcoholQCM](https://archive.ics.uci.edu/dataset/331/alcohol+qcm+sensor+dataset),
  save as `data/AlcoholQCM.csv`
- **Segment** — save as `data/segment.dat`

---

## Usage

All scripts are run from within `experiments/` in RStudio:

```r
setwd("experiments")
```

**Quick verify (Iris, ~5–10 min):**
```r
source("run_iris.R")
```

Expected results from Table 2 (ratio = 10–70%):
- ACC ≈ 84.4% | NMI ≈ 66.3% | F-score ≈ 84.8% | PUR ≈ 84.6%

**Run a specific dataset:**
```r
source("run_seeds.R")
source("run_glass.R")
source("run_wine.R")
```

**Batch run (iris + seeds + wine → Table 2):**
```r
source("run_all.R")
```

**Visualize results for any dataset:**
```r
DATASET <- "iris"        # set to: iris, seeds, wine, glass, vehicle, ...
source("plot_results.R")
```

Plots are saved to `results/{dataset}/plots/` as PNG and PDF.

---

## Methods Compared

| Method | Description |
|---|---|
| **Proposed** | GMM-Incomplete — handles missing data directly in EM |
| **GMM + Mean-fill** | Impute with column mean, then standard GMM |
| **GMM + Zero-fill** | Impute with zeros, then standard GMM |
| **GMM + EM-fill** | Impute with RegEM, then standard GMM |
| **DK + Mean/Zero/EM** | Dynamic K-means with corresponding imputation |

---

## Output Structure

Each dataset produces:
```
results/{dataset}/
├── {dataset}_results.rds           # Full raw results
├── {dataset}_per_ratio.tsv         # Aggregated per-ratio table
├── {dataset}_comparison_table.tsv  # Table 2 format comparison
├── plots/
│   ├── {dataset}_per_ratio.png
│   └── {dataset}_per_ratio.pdf
└── logs/
    └── {dataset}_YYYYMMDD_HHMMSS.log
```

---

## Reference

Zhang, Y., et al. (2021). *Gaussian Mixture Model Clustering with Incomplete Data*.
ACM Transactions on Multimedia Computing, Communications, and Applications (TOMM).
