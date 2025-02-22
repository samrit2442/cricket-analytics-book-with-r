library(pacman)
pacman::p_load(tidyverse, readxl, reader, plyr, gt, gtsummary)

source("./R/data_cleaning.R")

# Bowler's wicket types
wkt_types <- c("bowled", "caught", "caught and bowled", "hit wicket", "lbw", "stumped")


# Select a random match_id for which we want to generate the match report
m_id <- t20_2$match_id |> 
  unique() |> 
  sample(1)

# m_id <- 1144161

# Filter the data for the selected match_id
match_data <- t20_2 |> 
  filter(match_id == m_id)

mat_date <- match_data$start_date |> first()
mat_date <- format(as.Date(mat_date), "%d %B, %Y")
mat_date

all_players <- match_data |>
  dplyr::select(batting_team, striker, innings) |>
  dplyr::distinct() |>
  dplyr::rename(player = striker) |>
  bind_rows(
    match_data |>
      dplyr::select(batting_team, non_striker, innings) |>
      dplyr::distinct() |>
      dplyr::rename(player = non_striker))|>
  dplyr::distinct()
all_players

# Extract the match details
bat_dtls_1 <- match_data |> 
  dplyr::group_by(innings, batting_team, striker) |> 
  dplyr::summarise(runs = sum(runs_off_bat), balls = n(), 
            fours = sum(isFour), sixes = sum(isSix))
bat_dtls_1

bat_dtls_2 <- match_data |> 
  dplyr::distinct(player_dismissed) |> 
  dplyr::mutate(out = 1) |>
  full_join(bat_dtls_1, by = c("player_dismissed" = "striker")) |> 
  full_join(all_players, by = c("player_dismissed" = "player", "batting_team", "innings")) |> 
  dplyr::filter(player_dismissed != "0") |> 
  dplyr::mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  dplyr::mutate(innings = ifelse(innings == 0, 
                                 max(innings[batting_team == batting_team]), 
                                 innings)) |> 
  dplyr::arrange(innings, desc(runs), balls, .by_group = TRUE)
bat_dtls_2

bowl_dtls_1 <- match_data |> 
  dplyr::mutate(isBowlWkt = if_else(wicket_type %in% wkt_types, 1, 0)) |>
  dplyr::filter(legbyes == 0 & byes == 0) |> 
  dplyr::group_by(bowling_team, bowler) |> 
  dplyr::summarise(runs = sum(team_runs),
                   wicket = sum(isBowlWkt))

bowl_dtls_2 <- match_data |> 
  dplyr::filter(wides == 0 & noballs == 0) |> 
  dplyr::group_by(innings, bowling_team, bowler) |> 
  dplyr::summarise(balls = n()) |> 
  left_join(bowl_dtls_1, by = c("bowling_team", "bowler")) |>
  dplyr::mutate(over = paste0(floor(balls/6), ".", balls%%6),
                econ = round(runs/balls*6, 2))

bat_stat <- bat_dtls_2 |> 
  dplyr::group_by(innings, batting_team) |> 
  dplyr::arrange(desc(runs), balls, .by_group = TRUE) |> 
  slice_head(n = 3)
bat_stat

bowl_stat <- bowl_dtls_2 |>
  dplyr::group_by(innings, bowling_team) |>
  dplyr::arrange(desc(innings), desc(wicket), runs, .by_group = F) |>
  dplyr::slice_head(n = 3)
bowl_stat

toss <- match_sum |> 
  dplyr::filter(match_id == m_id) |> 
  dplyr::pull(toss_winner) |> 
  unique()
toss

pom <- match_sum |> 
  dplyr::filter(match_id == m_id) |> 
  dplyr::pull(player_of_match) |> 
  unique()
pom

event <- match_sum |> 
  dplyr::filter(match_id == m_id) |> 
  dplyr::pull(event) |> 
  unique()
event

venue <- match_sum |> 
  dplyr::filter(match_id == m_id) |> 
  dplyr::pull(venue) |> 
  unique()

mat_results <- match_data |> 
  dplyr::group_by(innings, batting_team) |>
  dplyr::summarise(runs = sum(team_runs), 
                   wickets = sum(isOut), 
                   balls = sum(wides == 0 & noballs == 0)) |> 
  ungroup() |> 
  inner_join(country_flag) |>
  dplyr::mutate(summary = case_when(batting_team == toss ~ paste0(batting_team, 
                                                      "      ", 
                                                      runs, 
                                                      "/", 
                                                      wickets,
                                                      " (",
                                                      floor(balls/6), 
                                                      ".",
                                                      balls%%6," ovr.)", "  🪙"),
                                    .default = paste0(batting_team, 
                                                      "      ", 
                                                      runs, 
                                                      "/", 
                                                      wickets,
                                                      " (",
                                                      floor(balls/6), 
                                                      ".",
                                                      balls%%6," ovr.)")))

# mat_results$summary <- factor(mat_results$summary, levels = unique(mat_results$summary))


batter <- bat_stat |> 
  ungroup() |> 
  dplyr::transmute(bat = player_dismissed, 
                   runs = case_when(out == 1 ~ paste0(runs, " (", balls, ")"), 
                                    TRUE ~ paste0(runs, "*", " (", balls,")")))
batter

bowler <- bowl_stat |> 
  ungroup() |> 
  dplyr::transmute(bowl = bowler, 
                   wickets = paste0(wicket, "/", runs,
                                    " (", over, ")"))
bowler

mat_sum <- function() {
  if(mat_results$innings[1] == 1 & mat_results$runs[1] > mat_results$runs[2]){
    return(paste0(mat_results$batting_team[1], 
                  " won by ", 
                  mat_results$runs[1] - mat_results$runs[2],
                  " runs"))
  } else if(mat_results$innings[2] == 2 & mat_results$runs[2] > mat_results$runs[1]){
    return(paste0(mat_results$batting_team[2], 
                  " won by ", 
                  10 - mat_results$wickets[2], 
                  " wickets"))
  } else {
    return(paste0("Match tied"))
  }
}
mat_sum()

# Need to change the Coin emoji by adding if condition

mat_bat <- c(rep(mat_results$summary[1], 3), rep(mat_results$summary[2], 3))


mat_report <- tibble(
  cbind(batter, bowler)) |> 
  # dplyr::mutate(mat_bat) |>
  dplyr::mutate(blank1 = "    ", .before = 3) |> 
  dplyr::mutate(blank2 = "    ", .before = 3) |>
  dplyr::mutate(blank3 = "    ", .before = 3) |>
  # add_row(tibble(bat = paste(mat_results$summary[1], "🪙"), runs = " ", bowl = " ", wickets = " "), .before = 1) |>
  # add_row(tibble(bat = mat_results$summary[2], runs = " ", bowl = " ", wickets = " "), .before = 5) |> 
  # add_row(tibble(bat = " ", runs = mat_sum(), bowl = " ", wickets = " "), .after = 9) |> 
  # add_row(tibble(bat = paste("Match Date: ",mat_date), runs = " ", bowl = " ", wickets = " "), .before = 1) |> 
  mutate_all(~replace(., is.na(.), ""))

mat_report

mat_report |> 
  gt() |> 
  tab_header(title = md("🏏 **Cricket Match Report** 🏏"), 
             subtitle = md(paste0("**", mat_results$batting_team[1], 
                                  " vs. ", 
                                  mat_results$batting_team[2],
                                  "<br>", event,
                                  "<br>📅 ", mat_date,
                                  "<br>🏟 ", venue, "**"))) |>
  tab_row_group(label = mat_results$summary[1], rows = 1:3) |>
  tab_row_group(label = mat_results$summary[2], rows = 4:6) |> 
  row_group_order(groups = mat_results$summary) |>
  tab_options(column_labels.hidden = TRUE) |>
  tab_style(
    style = list(
      cell_fill(color = "#0a0740"),   # Yellow background for team totals
      cell_text(weight = "bold", color = "#ecf3b8", align = "center", size = "24px")
    ),
    locations = cells_row_groups() # Targeting team score rows
  ) |> 
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#2196F3"),  # Blue background for final match result
  #     cell_text(weight = "bold", color = "white", align = "center")
  #   ),
  #   locations = cells_body(rows = nrow(mat_report))  # Last row (match result)
  # ) |> 
  tab_style(
    style = list(
      cell_fill(color = "#faf7e1"),
      cell_text(weight = "bold", color = "#010729", align = "left")
    ),
    locations = cells_body(rows = c(1:6))  # Column headers
  ) |>
  opt_table_font(font = google_font("Outfit"), size = px(21)) |> 
  # Liter, Kanit, Lato, Nunito, Outfit, Oswald, PT Sans, Raleway, Roboto, Source Sans Pro, Ubuntu
  tab_options(table.align = "center") |>
  tab_source_note(source_note = md(paste0(
      "<div style='text-align: left; 
                 font-size: 22px; 
                 font-weight: bold;
                 font-style: italic;
                 color: black;  
                 width: 100%;'>Player of the Match: ", pom, "</div>"
  ))) |> 
  tab_source_note(
    source_note = md(paste0(
      "<div style='text-align: center; 
                 font-size: 24px; 
                 font-weight: bold; 
                 color: darkgreen; 
                 background-color: #f0f0f0;  
                 display: inline-block;  
                 width: 100%;'>",  
      mat_sum(), 
      "</div>"
    ))
  )










