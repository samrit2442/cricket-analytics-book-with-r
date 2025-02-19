library(pacman)
pacman::p_load(tidyverse)

# Read the data
match_sum <- readRDS(file = "./data/t20_raw_match_data09JAN2025_1219.rds")
t20 <- readRDS(file = "./data/t20_raw_data09JAN2025_1219.rds")
t20_bkp <- t20

# Check the data
t20[t20 == ""] <- NA

# Filling the NAs with 0
t20[is.na(t20)] <- 0

t20_1 <- t20 |>
  as_tibble() |> 
  select(-season, -other_wicket_type, -other_player_dismissed) |> 
  mutate(over = ceiling(ball),
         over_type = if_else(over >= 1 & over <= 6, "Powerplay",
                        if_else(over >= 7 & over <= 16, "Middle Over", "Death Over"))) |>
  mutate(isDot = if_else(runs_off_bat == 0, 1, 0),
                isOne = if_else(runs_off_bat == 1, 1, 0),
                isTwo = if_else(runs_off_bat == 2, 1, 0),
                isThree = if_else(runs_off_bat == 3, 1, 0),
                isFour = if_else(runs_off_bat == 4, 1, 0),
                isSix = if_else(runs_off_bat == 6, 1, 0),
                isOut = if_else(wicket_type != 0, 1, 0),
                team_runs = runs_off_bat + extras,
                start_date = as.Date(start_date)) |> 
  filter(innings == 1 | innings == 2)

# Selecting top 15 countries by their recent ICC T20I ranking
top_15_countries <- c("Pakistan", "England", "India", "Australia",
                      "South Africa", "New Zealand", "Afghanistan", 
                      "West Indies", "Bangladesh", "Sri Lanka", "Ireland", 
                      "Scotland", "Zimbabwe", "Netherlands", "USA")

# Mapping of Country names with flag emoji
country_flag <- tibble::tribble(~batting_team, ~batting_team_emoji,
                                "Pakistan", "🇵🇰",
                                "England", "🏴󠁧󠁢󠁥󠁮󠁧󠁿",
                                "India", "🇮🇳",
                                "Australia", "🇦🇺",
                                "South Africa", "🇿🇦",
                                "New Zealand", "🇳🇿",
                                "Afghanistan", "🇦🇫",
                                "West Indies", "🌴",
                                "Bangladesh", "🇧🇩",
                                "Sri Lanka", "🇱🇰",
                                "Ireland", "🇮🇪",
                                "Scotland", "🏴󠁧󠁢󠁳󠁣󠁴󠁿",
                                "Zimbabwe", "🇿🇼",
                                "Netherlands", "🇳🇱",
                                "USA", "🇺🇸")

# Filtering out top 15 Countries data
t20_2 <- t20_1 |>
  filter(batting_team %in% top_15_countries & bowling_team %in% top_15_countries)








