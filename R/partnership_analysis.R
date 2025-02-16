library(pacman)
pacman::p_load(tidyverse, readxl, reader, plyr, gt, gtsummary)

source("./R/data_cleaning.R")

plyr_name <- "Yuvraj Singh"

partnership_df <- t20_2 |> 
  dplyr::filter(striker == plyr_name | non_striker == plyr_name) |> 
  dplyr::mutate(tmp_str = ifelse(non_striker == plyr_name, non_striker, striker),
                tmp_n_str = ifelse(non_striker == plyr_name, striker, non_striker)) |>
  dplyr::select(-striker, -non_striker) %>%
  dplyr::rename(striker = tmp_str, non_striker = tmp_n_str) |> 
  dplyr::group_by(match_id, striker, non_striker, bowling_team) |> 
  dplyr::summarise(runs = sum(runs_off_bat), balls = n(), 
            fours = sum(isFour), sixes = sum(isSix), 
            dot_balls = sum(isDot)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(match_id, striker, non_striker)
  # ungroup() |>
  


dplyr::group_by(match_id, non_striker, striker) |> 
  dplyr::summarise(runs = sum(runs), balls = sum(balls), 
            fours = sum(fours), sixes = sum(sixes), 
            dot_balls = sum(dot_balls))
  # dplyr::mutate(partnership = cumsum(runs)) |> 
  # dplyr::arrange(match_id, .by_group = TRUE)

partnership_df |> head(10)



