# ============================================================
# Formula One constructor rankings: GAS model
# Dataset: annual Constructors' Championship rankings, 1995--2025
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gasmodel)

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

data_file <- file.path("data", "constructor_rankings_1995_2025.csv")

out_dir <- "results"
fig_dir <- file.path(out_dir, "figures")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE)
set.seed(42)

# ------------------------------------------------------------
# 2. Load ranking matrix
# ------------------------------------------------------------
#   rows    = seasons
#   columns = aggregated constructors
#   values  = annual ranks or Inf for non-participation

rank_data <- read.csv(
  data_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

rank_data$season <- as.integer(rank_data$season)

years <- rank_data$season

y <- rank_data[, -1]
y <- as.matrix(sapply(y, as.numeric))

rownames(y) <- years
colnames(y) <- names(rank_data)[-1]

# Drop possible all-Inf columns just in case
keep <- apply(y, 2, function(x) any(is.finite(x)))
y <- y[, keep, drop = FALSE]

cat("\nModel data:\n")
cat("Number of seasons:", nrow(y), "\n")
cat("Number of constructors:", ncol(y), "\n")

# ------------------------------------------------------------
# 3. Basic sanity check
# ------------------------------------------------------------
# For each season, finite ranks must be exactly 1, 2, ..., n.

rank_check <- tibble(
  season = as.integer(rownames(y)),
  n_constructors = apply(y, 1, function(x) sum(is.finite(x))),
  ordering_correct = apply(y, 1, function(x) {
    r <- sort(x[is.finite(x)])
    n <- length(r)
    n > 0 && all(r == seq_len(n))
  })
)

print(rank_check, n = Inf)

if (!all(rank_check$ordering_correct)) {
  stop("Some seasons do not have correct ranking order.")
}

write_csv(
  rank_check,
  file.path(out_dir, "rank_order_check.csv")
)

# ------------------------------------------------------------
# 4. Long-format data for plots
# ------------------------------------------------------------

rank_long <- as.data.frame(y) %>%
  mutate(season = as.integer(rownames(y))) %>%
  pivot_longer(
    cols = -season,
    names_to = "constructor",
    values_to = "rank"
  ) %>%
  mutate(
    participated = is.finite(rank)
  )

rank_long_finite <- rank_long %>%
  filter(participated) %>%
  arrange(season, rank)

write_csv(
  rank_long_finite,
  file.path(out_dir, "rankings_long.csv")
)

# ------------------------------------------------------------
# 5. Descriptive figure: participation over time
# ------------------------------------------------------------

p_participation <- ggplot(
  rank_long,
  aes(x = season, y = constructor, fill = participated)
) +
  geom_tile() +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  labs(
    x = "Season",
    y = NULL,
    fill = "Participated",
    title = "Constructor participation, 1995--2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8)
  )

ggsave(
  file.path(fig_dir, "figure_participation.png"),
  p_participation,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 6. Descriptive figure: observed rankings of selected teams
# ------------------------------------------------------------

selected_teams <- intersect(
  c("Ferrari", "McLaren", "Mercedes", "Red Bull Racing",
    "Williams", "Alpine", "Aston Martin"),
  colnames(y)
)

p_observed_ranks <- rank_long_finite %>%
  filter(constructor %in% selected_teams) %>%
  ggplot(aes(x = season, y = rank, group = constructor, colour = constructor)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_y_reverse(
    breaks = 1:max(rank_long_finite$rank),
    minor_breaks = NULL
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  labs(
    x = "Season",
    y = "Final constructor rank",
    colour = NULL,
    title = "Observed Constructors' Championship rankings"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(fig_dir, "figure_observed_rankings_selected.png"),
  p_observed_ranks,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 7. Estimate models
# ------------------------------------------------------------

# Static Plackett--Luce model
# No dynamics: p = 0, q = 0

est_static <- gas(
  y = y,
  distr = "pluce",
  p = 0,
  q = 0,
  par_link = T,
  coef_fix_special = c("zero_sum_intercept", "panel_structure"),
  print_progress = FALSE
)

# Mean-reverting GAS(1,1) model
# Starting values are based on the static model.

est_stnry <- gas(
  y = y,
  distr = "pluce",
  coef_fix_special = c("zero_sum_intercept", "panel_structure"),
  coef_start = as.vector(rbind(
    est_static$fit$par_unc / 2,
    0.5,
    0.5
  )),
  print_progress = FALSE
)



# ------------------------------------------------------------
# Check whether a fitted model is numerically usable
# ------------------------------------------------------------

model_is_ok <- function(model) {
  
  if (inherits(model, "try-error")) {
    return(FALSE)
  }
  
  ll <- tryCatch(
    as.numeric(logLik(model)),
    error = function(e) NA_real_
  )
  
  if (!is.finite(ll)) {
    return(FALSE)
  }
  
  if (any(!is.finite(model$fit$par_unc))) {
    return(FALSE)
  }
  
  return(TRUE)
}

static_ok <- model_is_ok(est_static)
stnry_ok  <- model_is_ok(est_stnry)

cat("\nModel usability:\n")
cat("Static model:", static_ok, "\n")
cat("Mean-reverting model:", stnry_ok, "\n")

# ------------------------------------------------------------
# 8. Model comparison
# ------------------------------------------------------------

models_for_aic <- list()

if (static_ok) models_for_aic$static <- est_static
if (stnry_ok)  models_for_aic$mean_reverting <- est_stnry

aic_table <- tibble(
  model = names(models_for_aic),
  logLik = sapply(models_for_aic, function(m) as.numeric(logLik(m))),
  AIC = sapply(models_for_aic, function(m) as.numeric(AIC(m))),
  BIC = sapply(models_for_aic, function(m) as.numeric(BIC(m)))
)

print(aic_table)

write.csv(
  as.data.frame(aic_table),
  file.path(out_dir, "model_comparison_aic.csv")
)

# ------------------------------------------------------------
# 9. Estimated long-run constructor strengths
# ------------------------------------------------------------
# Higher value = stronger constructor.

strength_table <- tibble(
  constructor = colnames(y),
  static_strength = as.numeric(est_static$fit$par_unc),
  gas_strength = as.numeric(est_stnry$fit$par_unc)
) %>%
  mutate(
    static_rank = rank(-static_strength, ties.method = "first"),
    gas_rank = rank(-gas_strength, ties.method = "first")
  ) %>%
  arrange(gas_rank)

print(strength_table, n = Inf)

write_csv(
  strength_table,
  file.path(out_dir, "estimated_long_run_strengths.csv")
)

p_strength <- strength_table %>%
  ggplot(aes(x = reorder(constructor, gas_strength), y = gas_strength)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimated strength",
    title = "Estimated long-run constructor strengths"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(fig_dir, "figure_long_run_strengths.png"),
  p_strength,
  width = 7,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 10. Time-varying worth parameters
# ------------------------------------------------------------
# gas_filter gives filtered time-varying parameters.
# These are the model-implied dynamic constructor strengths.

get_par_tv <- function(est, years, teams) {
  
  flt <- gas_filter(
    est,
    method = "given_coefs",
    coef_set = matrix(coef(est), nrow = 1)
  )
  
  z <- as.matrix(flt$filter$par_tv_mean)
  
  if (nrow(z) == length(teams) && ncol(z) == length(years)) {
    z <- t(z)
  }
  
  if (nrow(z) != length(years)) {
    stop("Could not identify the time dimension of par_tv.")
  }
  
  colnames(z) <- teams
  rownames(z) <- years
  
  z
}

par_tv <- get_par_tv(
  est = est_stnry,
  years = years,
  teams = colnames(y)
)

par_tv_long <- as.data.frame(par_tv) %>%
  mutate(season = years) %>%
  pivot_longer(
    cols = -season,
    names_to = "constructor",
    values_to = "strength"
  )

write_csv(
  par_tv_long,
  file.path(out_dir, "time_varying_strengths.csv")
)

p_tv_strength <- par_tv_long %>%
  filter(constructor %in% selected_teams) %>%
  ggplot(aes(x = season, y = strength, colour = constructor)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  labs(
    x = "Season",
    y = "Filtered strength",
    colour = NULL,
    title = "Filtered time-varying constructor strengths"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(fig_dir, "figure_time_varying_strengths_selected.png"),
  p_tv_strength,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 11. Model-implied yearly strength ranking
# ------------------------------------------------------------

strength_rank_long <- par_tv_long %>%
  group_by(season) %>%
  mutate(model_rank = rank(-strength, ties.method = "first")) %>%
  ungroup()

write_csv(
  strength_rank_long,
  file.path(out_dir, "model_implied_strength_ranks.csv")
)

p_model_ranks <- strength_rank_long %>%
  filter(constructor %in% selected_teams) %>%
  ggplot(aes(x = season, y = model_rank, colour = constructor)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_y_reverse(
    breaks = 1:ncol(y),
    minor_breaks = NULL
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  labs(
    x = "Season",
    y = "Model-implied strength rank",
    colour = NULL,
    title = "Model-implied constructor strength ranking"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(fig_dir, "figure_model_implied_ranks_selected.png"),
  p_model_ranks,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 12. Facet plots of estimated constructor strengths
# ------------------------------------------------------------
# This replaces the built-in gasmodel plots.
# Each panel highlights one constructor.
# Grey lines show all constructors in the background.

plot_years <- as.integer(rownames(y))
plot_teams <- colnames(y)

# ------------------------------------------------------------
# Helper: extract filtered time-varying strengths
# ------------------------------------------------------------

get_filtered_strength <- function(model, years, teams) {
  
  flt <- gas_filter(
    model,
    method = "given_coefs",
    coef_set = matrix(coef(model), nrow = 1)
  )
  
  z <- as.matrix(flt$filter$par_tv_mean)
  
  # Expected shape: years x constructors.
  # If the package returns constructors x years, transpose it.
  if (nrow(z) == length(teams) && ncol(z) == length(years)) {
    z <- t(z)
  }
  
  rownames(z) <- years
  colnames(z) <- teams
  
  z
}

# ------------------------------------------------------------
# Helper: make one facet plot
# ------------------------------------------------------------

make_strength_facet_plot <- function(strength_matrix, plot_title, order_by = "mean") {
  
  strength_long <- as.data.frame(strength_matrix) %>%
    mutate(season = as.integer(rownames(strength_matrix))) %>%
    pivot_longer(
      cols = -season,
      names_to = "constructor",
      values_to = "strength"
    )
  
  # Order panels by average strength unless specified otherwise
  if (order_by == "mean") {
    constructor_order <- strength_long %>%
      group_by(constructor) %>%
      summarise(mean_strength = mean(strength, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(mean_strength)) %>%
      pull(constructor)
  } else {
    constructor_order <- plot_teams
  }
  
  strength_long <- strength_long %>%
    mutate(constructor = factor(constructor, levels = constructor_order))
  
  # Background data: all constructors, repeated in every facet
  background_strengths <- strength_long %>%
    rename(background_constructor = constructor)
  
  ggplot() +
    geom_line(
      data = background_strengths,
      aes(
        x = season,
        y = strength,
        group = background_constructor
      ),
      colour = "grey78",
      linewidth = 0.35,
      alpha = 0.75
    ) +
    geom_line(
      data = strength_long,
      aes(
        x = season,
        y = strength,
        group = constructor
      ),
      colour = "#6B1F1F",
      linewidth = 0.9
    ) +
    facet_wrap(~ constructor, ncol = 4) +
    scale_x_continuous(
      breaks = seq(min(plot_years), max(plot_years), by = 10)
    ) +
    labs(
      x = "Year",
      y = "Strength",
      title = plot_title
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 12, face = "plain", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.25),
      strip.background = element_rect(fill = "grey90", colour = "grey35"),
      strip.text = element_text(size = 9, face = "plain"),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      panel.border = element_rect(colour = "grey35", fill = NA, linewidth = 0.45)
    )
}

# ------------------------------------------------------------
# 12.1 Static model
# ------------------------------------------------------------
# Static strengths do not vary over time.
# We repeat the estimated static strength for every season.

static_strength_matrix <- matrix(
  est_static$fit$par_unc,
  nrow = length(plot_years),
  ncol = length(plot_teams),
  byrow = TRUE
)

rownames(static_strength_matrix) <- plot_years
colnames(static_strength_matrix) <- plot_teams

p_static_all <- make_strength_facet_plot(
  static_strength_matrix,
  "Static model: estimated constructor strengths"
)

ggsave(
  file.path(fig_dir, "figure_static_strengths_all_facets.png"),
  p_static_all,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(fig_dir, "figure_static_strengths_all_facets.pdf"),
  p_static_all,
  width = 9,
  height = 7
)

# ------------------------------------------------------------
# 12.2 Mean-reverting GAS model
# ------------------------------------------------------------

stnry_strength_matrix <- get_filtered_strength(
  model = est_stnry,
  years = plot_years,
  teams = plot_teams
)

p_stnry_all <- make_strength_facet_plot(
  stnry_strength_matrix,
  "Mean-reverting GAS model: estimated constructor strengths"
)

ggsave(
  file.path(fig_dir, "figure_mean_reverting_strengths_all_facets.png"),
  p_stnry_all,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(fig_dir, "figure_mean_reverting_strengths_all_facets.pdf"),
  p_stnry_all,
  width = 9,
  height = 7
)

# ------------------------------------------------------------
# 13. One-step-ahead forecast
# ------------------------------------------------------------
# This gives forecasted strength for the next season.

fcst_stnry <- gas_forecast(
  est_stnry,
  t_ahead = 1
)

fcst_strength <- as.numeric(fcst_stnry$forecast$par_tv_ahead_mean[1, ])
names(fcst_strength) <- colnames(y)

forecast_table <- tibble(
  constructor = names(fcst_strength),
  forecast_strength = fcst_strength,
  forecast_win_probability =
    exp(forecast_strength) / sum(exp(forecast_strength))
) %>%
  mutate(
    forecast_rank = rank(-forecast_strength, ties.method = "first")
  ) %>%
  arrange(forecast_rank)

print(forecast_table, n = Inf)

write_csv(
  forecast_table,
  file.path(out_dir, "one_step_forecast.csv")
)

p_forecast <- forecast_table %>%
  ggplot(aes(
    x = reorder(constructor, forecast_win_probability),
    y = forecast_win_probability
  )) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Forecast probability of rank 1",
    title = "One-step-ahead forecast: probability of leading the ranking"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(fig_dir, "figure_one_step_forecast.png"),
  p_forecast,
  width = 7,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------
# 14. Save model objects
# ------------------------------------------------------------

saveRDS(
  list(
    y = y,
    rank_data = rank_data,
    rank_long = rank_long_finite,
    est_static = est_static,
    est_stnry = est_stnry,
    strength_table = strength_table,
    time_varying_strengths = par_tv_long,
    forecast_table = forecast_table
  ),
  file.path(out_dir, "f1_gas_model_results_complete.rds")
)

cat("\nDone. Results saved in:", out_dir, "\n")
cat("Figures saved in:", fig_dir, "\n")
