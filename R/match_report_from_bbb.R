library(pacman)
pacman::p_load(tidyverse, readxl, reader, plyr)

# Select a random match_id for which we want to generate the match report
m_id <- t20_2$match_id |> 
  unique() |> 
  sample(1)

# Filter the data for the selected match_id
match_data <- t20_2 |> 
  filter(match_id == m_id)

# Extract the match details
bat_dtls <- match_data |> 
  dplyr::group_by(batting_team, striker) |> 
  dplyr::summarise(runs = sum(runs_off_bat), balls = n(), 
            fours = sum(isFour), sixes = sum(isSix))







