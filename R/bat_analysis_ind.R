library(pacman)
pacman::p_load(tidyverse)

plyr_name <- "Yuvraj Singh"

top_10_bat_stat <- t20_2 |>
  mutate(year = year(ymd(start_date))) |>
  group_by(striker, batting_team, year) |>
  summarise(runs = sum(runs_off_bat), balls = n(), 
            fours = sum(isFour), sixes = sum(isSix), 
            out = sum(isOut), dot_balls = sum(isDot)) |> 
  ungroup()


top_10_bat <- top_10_bat_stat |> 
  group_by(striker) |> 
  summarise(total_runs = sum(runs)) |> 
  arrange(desc(total_runs)) |>
  slice(1:10) |> 
  pull(striker)

# conflicted::conflict_prefer_all(winner = "dplyr")

top_10_bat_bar_df <- top_10_bat_stat |> 
  filter(striker %in% top_10_bat) |> 
  select(striker, year, runs) |> 
  mutate(year = as.character(year)) |> 
  arrange(striker, year) |>
  group_by(striker) |>
  mutate(total_runs = cumsum(runs)) |> 
  filter(year %in% as.character(2010:2024))
top_10_bat_bar_df


