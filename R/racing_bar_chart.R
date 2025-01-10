library(pacman)
pacman::p_load(tidyverse, ddplot)


filter(top_10_bat_bar_df, striker == "V Kohli")
filter(top_10_bat_bar_df, striker == "RG Sharma")

add_df <- data.frame(striker = c("RG Sharma", "V Kohli"), 
                     year = c("2023", "2023"), 
                     runs = c(0, 0), 
                     total_runs = c(3662, 3777))


top_10_bat_bar_df <- rbind(top_10_bat_bar_df, add_df)


race_bar_chart <- top_10_bat_bar_df |> 
  barChartRace(
    x = "total_runs",
    y = "striker",
    time = "year",
    ytitle = "Batter",
    xtitle = "Total Runs",
    title = "Bar chart race of T20I Runs",
    frameDur = 0,
    transitionDur = 2000,
    ease = "BackInOut")


htmlwidgets::saveWidget(race_bar_chart, "./plot/bar_chart_race.html")