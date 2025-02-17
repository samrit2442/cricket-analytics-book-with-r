library(pacman)
pacman::p_load(tidyverse)

plyr_name <- "Yuvraj Singh"

# Total Number of Dismissals
diss <- t20_1 |> 
  dplyr::filter(player_dismissed == plyr_name) |> 
  nrow()

# Removing Wide Balls
plyr_data <- t20_1 |> 
  dplyr::filter(striker == plyr_name & wides == 0)

# Total Innings Played
tot_inng <- t20_1 |> 
  dplyr::filter(striker == plyr_name | non_striker == plyr_name) |> 
  dplyr::summarise(innings = n_distinct(match_id)) |> 
  as.numeric()




