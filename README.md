# Formula One constructor ranking models

Code and data for *Modeling Formula One Constructors’ Championship Rankings Using Score-Driven Dynamics*, by Karolína Jindrová.

## Run the analysis

Download the repository and set R’s working directory to its main folder. Install the packages once, then run:

```r
install.packages(c("gasmodel", "ggplot2", "readr", "tidyr", "dplyr"))
source("scripts/03_fit_models.R")
```

This estimates the static and mean-reverting models and saves tables and figures in `results/`. The paper used R 4.6.1 and gasmodel 0.6.2; all recorded package versions are in [session_info.txt](session_info.txt). Use those versions to match the original environment.

## Data

[data/constructor_rankings_1995_2025.csv](data/constructor_rankings_1995_2025.csv) contains annual rankings for 17 constructor lineages over 1995–2025. Rows are seasons, columns are constructor lineages, and `Inf` indicates an unavailable rank. The source is the official Formula One standings; the lineage mapping is in `scripts/02_prepare_rankings.R`.

The included CSV is sufficient to run the analysis. To download and prepare the standings again:

```r
install.packages(c("rvest", "httr2", "purrr", "stringr"))
source("scripts/01_download_standings.R")
source("scripts/02_prepare_rankings.R")
```

These scripts write to `data/raw/` and `data/derived/`. To analyse rebuilt data, change `data_file` at the top of `scripts/03_fit_models.R` to the new CSV path.
