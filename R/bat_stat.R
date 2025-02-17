library(pacman)
pacman::p_load(tidyverse)

plyr_name <- "Yuvraj Singh"

# Total Number of Dismissals
diss <- t20_1 |> 
  dplyr::filter(player_dismissed == plyr_name) |> 
  dplyr::select(match_id, wicket_type, over_type, bowler)

# Removing Wide Balls
plyr_data <- t20_1 |> 
  dplyr::filter(striker == plyr_name & wides == 0)

# Total Innings Played
tot_inng <- t20_1 |> 
  dplyr::filter(striker == plyr_name | non_striker == plyr_name) |> 
  dplyr::summarise(innings = dplyr::n_distinct(match_id)) |> 
  as.numeric()

# Total Team Runs
team_runs <- t20_1 |> 
  dplyr::group_by(match_id, innings) |> 
  dplyr::summarise(team_runs = sum(team_runs))


# Innings-wise Batting Statistics
bat_stat_1 <- plyr_data |> 
  dplyr::filter(striker == plyr_name) |> 
  dplyr::group_by(start_date, match_id, innings) |> 
  dplyr::summarise(Runs = sum(runs_off_bat), Balls = length(runs_off_bat),
                   SR = round(Runs/Balls*100, 2), 
                   Fours = sum(isFour), Sixes = sum(isSix),
                   Dots = sum(isDot),
                   Opponent = first(bowling_team)) |> 
  left_join(diss) |> 
  dplyr::mutate(wicket_type = if_else(is.na(wicket_type), "not out", wicket_type),
                isNO = if_else(wicket_type == "not out", 1, 0),
                isThirty = if_else(Runs >= 30 & Runs < 50, 1, 0),
                isFifty = if_else(Runs >= 50 & Runs < 100, 1, 0),
                isHundred = if_else(Runs >= 100, 1, 0)) |> 
  left_join(team_runs) |> 
  dplyr::mutate(Contribution = round(Runs/team_runs*100, 2),
                Year = year(start_date))









