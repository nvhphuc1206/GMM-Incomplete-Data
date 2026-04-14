# GMM with Incomplete Data — R Replication

R replication of Zhang et al. (2021): *Gaussian Mixture Model Clustering with Incomplete Data*.  
MATLAB original: https://github.com/Zhangyi1231/GMM-with-Incomplete-Data

## Cấu trúc

```
gmm-incomplete-r/
├── R/
│   ├── gmm_incomplete.R       # Core Algorithm 1 (Proposed method)
│   ├── imputation_baseline.R  # Mean / Zero / EM filling baselines
│   ├── evaluation.R           # ACC, NMI, F-score, PUR
│   └── data_utils.R           # Data loading, standardize_rms(), generate_missing()
├── experiments/
│   ├── run_iris.R             # Quick verify: Iris (150×4×3)
│   ├── run_seeds.R            # Quick verify: Seeds (210×7×3)
│   └── run_all.R              # Toàn bộ datasets → Table 2
├── results/                   # CSV/RDS output
├── EXPERIMENT_v2.md           # Ghi chú sai lệch MATLAB vs bài báo
└── README.md
```

## Cài đặt dependencies

```r
install.packages(c("clue", "aricode", "ggplot2", "dplyr", "tidyr"))
```

## Chạy nhanh (Iris — verify)

```r
setwd("experiments")
source("run_iris.R")
```

Kết quả kỳ vọng (Table 2, "Ours"):
- ACC ≈ 84.4% | NMI ≈ 66.3% | F-score ≈ 84.8% | PUR ≈ 84.6%

## Chạy đầy đủ (tất cả datasets)

```r
source("experiments/run_all.R")
```

Kết quả được lưu vào `results/table2_replication.csv`.

## Lưu ý quan trọng

Xem [EXPERIMENT_v2.md](../EXPERIMENT_v2.md) để biết các điểm khác biệt quan trọng  
giữa MATLAB gốc và mô tả trong bài báo, bao gồm:

1. `standardize_rms()` — dùng RMS (không phải `scale()`)
2. `epsilon = 1e-4` (không phải 1e-6)
3. Double E-step mỗi iteration
4. GMM+Mean dùng data chưa chuẩn hóa
