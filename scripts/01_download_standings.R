# ============================================================
# Scrape official Formula 1 constructor standings, 1958-2025
# Source: https://www.formula1.com/en/results/{year}/team
# ============================================================

# install.packages(c("rvest", "httr2", "dplyr", "purrr", "stringr", "readr", "tidyr"))

library(rvest)
library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyr)

years <- 1958:2025

out_dir <- file.path("data", "raw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

scrape_constructor_year <- function(year) {
  url <- sprintf("https://www.formula1.com/en/results/%s/team", year)
  
  message("Scraping ", year, " ...")
  
  html <- request(url) |>
    req_user_agent("Mozilla/5.0 academic research; contact: local") |>
    req_perform() |>
    resp_body_html()
  
  # ---- First try: HTML tables ----
  tables <- html |>
    html_elements("table") |>
    html_table(fill = TRUE)
  
  if (length(tables) > 0) {
    tab <- tables[[1]]
    
    names(tab) <- names(tab) |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]+", "_") |>
      str_replace_all("^_|_$", "")
    
    pos_col <- names(tab)[str_detect(names(tab), "^pos")]
    team_col <- names(tab)[str_detect(names(tab), "team|constructor")]
    pts_col <- names(tab)[str_detect(names(tab), "pts|points")]
    
    if (length(pos_col) > 0 && length(team_col) > 0 && length(pts_col) > 0) {
      return(
        tab |>
          transmute(
            season = year,
            position = as.integer(.data[[pos_col[1]]]),
            team = str_squish(as.character(.data[[team_col[1]]])),
            points = parse_number(as.character(.data[[pts_col[1]]])),
            source_url = url
          ) |>
          filter(!is.na(position), team != "")
      )
    }
  }
  
  # ---- Fallback: parse visible page text ----
  txt <- html |>
    html_text2()
  
  lines <- str_split(txt, "\n")[[1]] |>
    str_squish()
  
  start <- which(str_detect(lines, "^Pos\\.Team Pts\\.?$|^Pos\\. Team Pts\\.?$|^Pos Team Pts\\.?$"))
  
  if (length(start) == 0) {
    warning("Could not find standings table for ", year)
    return(tibble(
      season = integer(),
      position = integer(),
      team = character(),
      points = numeric(),
      source_url = character()
    ))
  }
  
  lines_after <- lines[(start[1] + 1):length(lines)]
  end <- which(str_detect(lines_after, "^OUR PARTNERS|^Download the Official F1 App"))
  
  if (length(end) > 0) {
    lines_after <- lines_after[1:(end[1] - 1)]
  }
  
  # Expected format, for example:
  # "1Cooper Climax 48"
  # "5Cooper Castellotti 3"
  parsed <- str_match(
    lines_after,
    "^([0-9]+)\\s*(.*?)\\s+([0-9]+(?:[\\.,][0-9]+)?)$"
  )
  
  tibble(
    season = year,
    position = as.integer(parsed[, 2]),
    team = str_squish(parsed[, 3]),
    points = parse_number(parsed[, 4]),
    source_url = url
  ) |>
    filter(!is.na(position), !is.na(points), team != "")
}

# Scrape all years
constructor_standings <- map_dfr(years, function(y) {
  Sys.sleep(0.5)   # be polite to the website
  scrape_constructor_year(y)
})

# Save long-format standings
write_csv(
  constructor_standings,
  file.path(out_dir, "f1_constructor_standings_1958_2025_official.csv")
)

# Create gasmodel-style ranking matrix:
# rows = seasons
# columns = team names exactly as scraped from the F1 website
# values = final constructor position, Inf if team absent
teams <- sort(unique(constructor_standings$team))

y <- matrix(
  Inf,
  nrow = length(years),
  ncol = length(teams),
  dimnames = list(as.character(years), teams)
)

idx <- cbind(
  match(constructor_standings$season, years),
  match(constructor_standings$team, teams)
)

y[idx] <- constructor_standings$position

# Save ranking matrix
saveRDS(
  y,
  file.path(out_dir, "f1_constructor_rank_matrix_1958_2025.rds")
)

write.csv(
  y,
  file.path(out_dir, "f1_constructor_rank_matrix_1958_2025.csv"),
  row.names = TRUE
)

# Save complete R object
f1_constructor_data <- list(
  standings_long = constructor_standings,
  rank_matrix = y,
  years = years,
  teams = teams
)

saveRDS(
  f1_constructor_data,
  file.path(out_dir, "f1_constructor_data_1958_2025_complete.rds")
)

# Quick checks
constructor_standings |> count(season) |> print(n = Inf)
head(constructor_standings, 20)
dim(y)
