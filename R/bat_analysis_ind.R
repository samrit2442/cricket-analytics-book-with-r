library(pacman)
pacman::p_load(tidyverse)

plyr_name <- "Yuvraj Singh"

top_10_bat_stat <- t20_2 |>
  dplyr::mutate(year = year(ymd(start_date))) |>
  dplyr::group_by(striker, batting_team, year) |>
  dplyr::summarise(runs = sum(runs_off_bat), balls = n(), 
            fours = sum(isFour), sixes = sum(isSix), 
            out = sum(isOut), dot_balls = sum(isDot)) |> 
  ungroup()


top_10_bat <- top_10_bat_stat |> 
  dplyr::group_by(striker) |> 
  dplyr::summarise(total_runs = sum(runs)) |> 
  dplyr::arrange(desc(total_runs)) |>
  slice(1:10) |> 
  pull(striker)

# conflicted::conflict_prefer_all(winner = "dplyr")

top_10_bat_bar_df <- top_10_bat_stat |> 
  dplyr::filter(striker %in% top_10_bat) |> 
  dplyr::select(striker, year, runs) |> 
  dplyr::mutate(year = as.character(year)) |> 
  dplyr::arrange(striker, year) |>
  dplyr::group_by(striker) |>
  dplyr::mutate(total_runs = cumsum(runs)) |> 
  dplyr::filter(year %in% as.character(2010:2024))
top_10_bat_bar_df


