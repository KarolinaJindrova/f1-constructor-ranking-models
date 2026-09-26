# ============================================================
# Load raw constructor ranking matrix and restrict sample
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Raw file created by the scraping code
input_file <- file.path(
  "data", "raw",
  "f1_constructor_rank_matrix_1958_2025.csv"
)

out_dir <- file.path("data", "derived")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Load as character first, because the matrix contains both numbers and Inf
rank_matrix_raw <- read_csv(
  input_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# The first column contains seasons
names(rank_matrix_raw)[1] <- "season"

rank_matrix_raw <- rank_matrix_raw %>%
  mutate(season = as.integer(season))

# ------------------------------------------------------------
# Basic information on original raw dataset
# ------------------------------------------------------------

cat("\nOriginal dataset:\n")
cat("Number of seasons:", nrow(rank_matrix_raw), "\n")
cat("Number of constructor columns:", ncol(rank_matrix_raw) - 1, "\n")

# ------------------------------------------------------------
# Keep only seasons from 1995 onward and drop constructor columns
# with no finite numeric rank in the restricted sample
# ------------------------------------------------------------

has_finite_rank <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  any(is.finite(x_num))
}

rank_matrix_1995 <- rank_matrix_raw %>%
  filter(season >= 1995)

constructor_cols_keep <- names(rank_matrix_1995)[-1][
  sapply(rank_matrix_1995[-1], has_finite_rank)
]

rank_matrix_1995 <- rank_matrix_1995 %>%
  select(season, all_of(constructor_cols_keep))

cat("\nDataset from 1995 after dropping non-participating constructor columns:\n")
cat("Number of seasons:", nrow(rank_matrix_1995), "\n")
cat("Number of constructor columns:", ncol(rank_matrix_1995) - 1, "\n")

write_csv(
  rank_matrix_1995,
  file.path(
    out_dir,
    "f1_constructor_rank_matrix_1995_2025_raw_names.csv"
  )
)

# ============================================================
# Aggregate constructor names, 1995--2025
# ============================================================

# ------------------------------------------------------------
# 1. Data preparation
# ------------------------------------------------------------

out_dir <- file.path("data", "derived")

rank_matrix_1995_raw <- rank_matrix_1995

# ------------------------------------------------------------
# 2. Convert wide raw matrix to long format
# ------------------------------------------------------------

rank_long_raw <- rank_matrix_1995_raw %>%
  pivot_longer(
    cols = -season,
    names_to = "team_raw",
    values_to = "rank_raw"
  ) %>%
  mutate(
    rank_raw = str_squish(rank_raw),
    rank = case_when(
      str_to_lower(rank_raw) %in% c("inf", "infinity") ~ Inf,
      is.na(rank_raw) | rank_raw == "" ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(rank_raw))
    )
  ) %>%
  filter(is.finite(rank)) %>%
  mutate(
    # The official archive uses "Lotus Renault" for two different
    # historical situations. In 2011 it is treated as the Team Lotus /
    # Caterham line, while from 2012 to 2014 it is the Enstone Lotus F1 line.
    team_key = case_when(
      team_raw == "Lotus Renault" & season == 2011 ~
        "Lotus Renault (2011 Team Lotus)",
      team_raw == "Lotus Renault" & season >= 2012 ~
        "Lotus Renault (2012-2014 Lotus F1)",
      TRUE ~ team_raw
    )
  )

# ------------------------------------------------------------
# 3. Aggregation map
# ------------------------------------------------------------

team_map <- bind_rows(
  tibble(
    team_key = c(
      "Benetton Renault", "Benetton Playlife",
      "Renault",
      "Lotus Renault (2012-2014 Lotus F1)", "Lotus Mercedes",
      "Alpine Renault"
    ),
    team_final = "Alpine",
    aggregation_note =
      "Enstone lineage: Benetton/Renault/Lotus F1/Alpine. Aggregated because the team entry continued through rebranding and ownership changes."
  ),
  
  tibble(
    team_key = c(
      "Tyrrell Yamaha", "Tyrrell Ford",
      "BAR Honda", "Honda", "Brawn Mercedes"
    ),
    team_final = "Mercedes",
    aggregation_note =
      "Brackley lineage: Tyrrell/BAR/Honda/Brawn/Mercedes. Aggregated because the team entry continued through purchase and rebranding."
  ),
  
  tibble(
    team_key = c(
      "Jordan Peugeot", "Jordan Mugen Honda", "Jordan Honda",
      "Jordan Ford", "Jordan Toyota",
      "MF1 Toyota", "Spyker Ferrari",
      "Force India Ferrari", "Force India Mercedes",
      "Racing Point BWT Mercedes",
      "Aston Martin Mercedes", "Aston Martin Aramco Mercedes"
    ),
    team_final = "Aston Martin",
    aggregation_note =
      "Silverstone lineage: Jordan/Midland/Spyker/Force India/Racing Point/Aston Martin. Aggregated because the same team entry continued through sale and rebranding."
  ),
  
  tibble(
    team_key = c(
      "Stewart Ford", "Jaguar Cosworth",
      "RBR Cosworth", "RBR Ferrari",
      "Red Bull Renault", "RBR Renault",
      "Red Bull Racing Renault", "Red Bull Racing TAG Heuer",
      "Red Bull Racing Honda", "Red Bull Racing RBPT",
      "Red Bull Racing Honda RBPT"
    ),
    team_final = "Red Bull Racing",
    aggregation_note =
      "Milton Keynes lineage: Stewart/Jaguar/Red Bull Racing. Aggregated because Jaguar was acquired and rebranded as Red Bull Racing."
  ),
  
  tibble(
    team_key = c(
      "Minardi Ford", "Minardi Asiatech", "Minardi Cosworth",
      "STR Cosworth", "STR Ferrari", "STR Renault",
      "Toro Rosso Ferrari", "Toro Rosso",
      "Scuderia Toro Rosso Honda",
      "AlphaTauri Honda", "AlphaTauri RBPT",
      "AlphaTauri Honda RBPT",
      "RB Honda RBPT"
    ),
    team_final = "Racing Bulls",
    aggregation_note =
      "Faenza lineage: Minardi/Toro Rosso/AlphaTauri/RB/Racing Bulls. Aggregated because the same Faenza-based team entry continued through rebranding."
  ),
  
  tibble(
    team_key = c(
      "Sauber Ford", "Sauber Petronas", "Sauber BMW",
      "Sauber Ferrari",
      "Alfa Romeo Racing Ferrari", "Alfa Romeo Ferrari",
      "Kick Sauber Ferrari"
    ),
    team_final = "Kick Sauber",
    aggregation_note =
      "Hinwil lineage: Sauber/BMW Sauber/Sauber/Alfa Romeo-branded Sauber/Kick Sauber. Aggregated because the Sauber team entry continued under different commercial names."
  ),
  
  tibble(
    team_key = c("McLaren Mercedes", "McLaren Honda", "McLaren Renault"),
    team_final = "McLaren",
    aggregation_note =
      "Same constructor with different engine suppliers. Aggregated to the 2025 website name."
  ),
  
  tibble(
    team_key = c(
      "Williams Renault", "Williams Mecachrome", "Williams Supertec",
      "Williams BMW", "Williams Cosworth", "Williams Toyota",
      "Williams Mercedes"
    ),
    team_final = "Williams",
    aggregation_note =
      "Same constructor with different engine suppliers. Aggregated to the 2025 website name."
  ),
  
  tibble(
    team_key = c("Haas Ferrari"),
    team_final = "Haas F1 Team",
    aggregation_note =
      "Same constructor with engine-supplier designation removed. Aggregated to the 2025 website name."
  ),
  
  tibble(
    team_key = c(
      "Footwork Hart", "Arrows Yamaha", "Arrows",
      "Arrows Supertec", "Arrows Asiatech"
    ),
    team_final = "Arrows Cosworth",
    aggregation_note =
      "Arrows/Footwork lineage. Aggregated because Footwork was a sponsorship/name period of Arrows and the team later reverted to Arrows."
  ),
  
  tibble(
    team_key = c(
      "Ligier Mugen Honda",
      "Prost Mugen Honda", "Prost Peugeot"
    ),
    team_final = "Prost Acer",
    aggregation_note =
      "Ligier/Prost lineage. Aggregated because Prost acquired and renamed Ligier."
  ),
  
  tibble(
    team_key = c(
      "Lotus Cosworth",
      "Lotus Renault (2011 Team Lotus)"
    ),
    team_final = "Caterham Renault",
    aggregation_note =
      "Team Lotus/Caterham lineage. The 2011 Lotus Renault entry is treated as the Team Lotus/Caterham line, not the Enstone Lotus F1 line."
  ),
  
  tibble(
    team_key = c(
      "Virgin Cosworth",
      "Marussia Cosworth", "Marussia Ferrari"
    ),
    team_final = "MRT Mercedes",
    aggregation_note =
      "Virgin/Marussia/Manor lineage. Aggregated because the entry continued through rebranding."
  )
)

if (any(duplicated(team_map$team_key))) {
  stop("The aggregation map contains duplicate team_key values.")
}

# ------------------------------------------------------------
# 4. Apply aggregation
# ------------------------------------------------------------

rank_mapped <- rank_long_raw %>%
  left_join(team_map, by = "team_key") %>%
  mutate(
    team_final = if_else(is.na(team_final), team_raw, team_final),
    aggregation_note = if_else(
      is.na(aggregation_note),
      "Not aggregated; already final name or standalone constructor.",
      aggregation_note
    )
  )

# ------------------------------------------------------------
# 5. Check that no two teams were aggregated in the same season
# ------------------------------------------------------------

same_year_conflicts <- rank_mapped %>%
  count(season, team_final, name = "n") %>%
  filter(n > 1)

if (nrow(same_year_conflicts) > 0) {
  print(same_year_conflicts, n = Inf)
  stop("Invalid aggregation: at least one aggregated team appears more than once in the same season.")
}

# ------------------------------------------------------------
# 6. Create audit tables
# ------------------------------------------------------------

aggregation_audit <- rank_mapped %>%
  group_by(team_raw, team_key, team_final, aggregation_note) %>%
  summarise(
    first_season = min(season),
    last_season = max(season),
    n_seasons = n_distinct(season),
    .groups = "drop"
  ) %>%
  arrange(team_final, first_season, team_raw)

write_csv(
  aggregation_audit,
  file.path(out_dir, "f1_constructor_aggregation_audit_1995_2025.csv")
)

aggregation_summary <- aggregation_audit %>%
  group_by(team_final) %>%
  summarise(
    first_season = min(first_season),
    last_season = max(last_season),
    raw_names = paste(sort(unique(team_raw)), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(team_final)

write_csv(
  aggregation_summary,
  file.path(out_dir, "f1_constructor_aggregation_summary_1995_2025.csv")
)

# ------------------------------------------------------------
# 7. Create aggregated wide ranking matrix
# ------------------------------------------------------------

rank_matrix_aggregated <- rank_mapped %>%
  select(season, team_final, rank) %>%
  complete(
    season = sort(unique(rank_matrix_1995_raw$season)),
    team_final = sort(unique(team_final)),
    fill = list(rank = Inf)
  ) %>%
  pivot_wider(
    names_from = team_final,
    values_from = rank
  ) %>%
  arrange(season)

write_csv(
  rank_matrix_aggregated,
  file.path(out_dir, "f1_constructor_rank_matrix_1995_2025_aggregated.csv")
)

cat("\nAggregated 1995--2025 dataset:\n")
cat("Number of seasons:", nrow(rank_matrix_aggregated), "\n")
cat("Number of constructor columns:", ncol(rank_matrix_aggregated) - 1, "\n")

# ------------------------------------------------------------
# 8. Simple sanity check
# ------------------------------------------------------------

participating_by_year <- rank_matrix_aggregated %>%
  pivot_longer(
    cols = -season,
    names_to = "team",
    values_to = "rank"
  ) %>%
  mutate(rank = as.numeric(rank)) %>%
  filter(is.finite(rank)) %>%
  arrange(season, rank)

ranking_sanity_check <- participating_by_year %>%
  group_by(season) %>%
  summarise(
    n_constructors = n(),
    n_unique_teams = n_distinct(team),
    team_names_unique = n_unique_teams == n_constructors,
    n_unique_ranks = n_distinct(rank),
    ranks_unique = n_unique_ranks == n_constructors,
    min_rank = min(rank),
    max_rank = max(rank),
    max_rank_equals_n_constructors = max_rank == n_constructors,
    ordering_correct = identical(sort(as.integer(rank)), seq_len(n_constructors)),
    check_passed =
      team_names_unique &
      ranks_unique &
      min_rank == 1 &
      max_rank_equals_n_constructors &
      ordering_correct,
    .groups = "drop"
  )

print(ranking_sanity_check, n = Inf)

cat(
  "\nAll yearly ranking checks passed:",
  all(ranking_sanity_check$check_passed),
  "\n"
)

write_csv(
  ranking_sanity_check,
  file.path(out_dir, "f1_constructor_aggregation_sanity_check_1995_2025.csv")
)

# ------------------------------------------------------------
# 9. Create model-ready matrix
# ------------------------------------------------------------

y_constructor_1995 <- as.data.frame(rank_matrix_aggregated)

rownames(y_constructor_1995) <- y_constructor_1995$season
y_constructor_1995$season <- NULL

y_constructor_1995 <- as.matrix(y_constructor_1995)
storage.mode(y_constructor_1995) <- "numeric"

saveRDS(
  y_constructor_1995,
  file.path(out_dir, "f1_constructor_rank_matrix_1995_2025_aggregated.rds")
)
