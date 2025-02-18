library(pacman)
pacman::p_load(tidyverse, gt, tictoc)

# Getting the raw data
tic()
source("./R/data_cleaning.R") # Approx. 6 seconds
toc()

plyr_name <- t20_2$striker |> 
  unique() |> 
  sample(1)
plyr_name
# plyr_name <- "RG Sharma"

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
  dplyr::summarise(innings = n_distinct(match_id)) |> 
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
                   Country = first(batting_team),
                   Opponent = first(bowling_team)) |> 
  left_join(diss) |> 
  dplyr::mutate(wicket_type = if_else(is.na(wicket_type), "not out", wicket_type),
                NO = if_else(wicket_type == "not out", 1, 0),
                `30s` = if_else(Runs >= 30 & Runs < 50, 1, 0),
                `50s` = if_else(Runs >= 50 & Runs < 100, 1, 0),
                `100s` = if_else(Runs >= 100, 1, 0)) |> 
  left_join(team_runs) |> 
  dplyr::mutate(Contribution = round(Runs/team_runs*100, 2),
                Year = year(start_date))


# Highest Score
hs <- bat_stat_1 |> 
  ungroup() |> 
  dplyr::arrange(desc(Runs), Balls) |> 
  slice(1) |> 
  dplyr::mutate(`Highest Score` = if_else(wicket_type == "not out",
                                          paste0(Runs, "* (", Balls, ") vs ", Opponent),
                                          paste0(Runs, " (", Balls, ") vs ", Opponent)))

# Computing Career Overview
career_overview <- bat_stat_1 |> 
  ungroup() |> 
  dplyr::summarise(across(
    .cols = c(Runs, Balls, Fours, Sixes, Dots, 
              NO, `30s`, `50s`, `100s`),
    .fns = sum
  )) |> 
  dplyr::mutate(Innings = tot_inng,
                `Batting Average` = round(Runs/(tot_inng - NO), 2),
                `Runs Per Innings` = round(Runs/Innings, 2),
                `Strike Rate` = round(Runs/Balls*100, 2)) |> 
  cbind(select(hs, `Highest Score`)) |> 
  dplyr::mutate(across(everything(), as.character)) |> 
  pivot_longer(cols = c(Innings, Runs, Balls, `Batting Average`,
                        `Runs Per Innings`, `Strike Rate`, NO, `Highest Score`,
                        Dots, Fours, Sixes, `30s`, `50s`, `100s`),
               names_to = "Stat", values_to = "Value")

# Career Overview gt Table
career_overview |> 
  gt() |> 
  tab_options(column_labels.hidden = TRUE) |>
  tab_header(title = md(paste0("**Batting Statistics🏏 in T20I <br>", plyr_name, "**")),
             subtitle = md(paste0("*", bat_stat_1$Country[1], "*"))) |> 
  opt_table_font(font = google_font("Outfit"), size = px(21)) |>
  tab_style(
    style = cell_fill(color = "#f4f4f4"),
    locations = cells_body(rows = seq(1, nrow(career_overview), by = 2))  # Alternating row colors
  ) |> 
  tab_footnote(
    footnote = "Afghanistan matches are excluded.",
    locations = cells_title(groups = "title")  # Adds footnote to the title
  )

