library(pacman)
pacman::p_load(tidyverse, gt, tictoc)


# Getting the Batting Statistics data
tic()
source("./R/bat_stat_tab.R") # Approx. 6 seconds
toc()

bat_viz_1 <- bat_stat_1 |> 
  ungroup() |> 
  dplyr::group_by(Year) |> 
  dplyr::summarise(Innings = n(), Runs = sum(Runs), Balls = sum(Balls), 
                   NO = sum(NO), `Strike Rate` = round(Runs/Balls*100, 2),
                   `Batting Average` = round(Runs/(Innings - NO), 2),
                   `Runs Per Innings` = round(Runs/Innings, 2))

bat_viz_1

ggplot(bat_viz_1, aes(x = Year, y = Runs)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(size = 3, color = "red") +
  ggtitle("Total Runs Across Years") +
  theme_minimal()

bat_viz_1 %>%
  pivot_longer(cols = c(`Batting Average`, `Strike Rate`), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = factor(Year), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Batting Average & Strike Rate Comparison") +
  theme_minimal()

ggplot(bat_viz_1, aes(x = Year, y = `Runs Per Innings`)) +
  geom_line(color = "green", size = 1) +
  geom_point(size = 3, color = "orange") +
  ggtitle("Runs Per Innings Trend") +
  theme_minimal()

bat_viz_1$Year <- as.factor(bat_viz_1$Year)

# Plot with both bar and line chart
ggplot(bat_viz_1, aes(x = Year)) +
  # Bar Chart for Runs
  geom_bar(aes(y = Runs), stat = "identity", fill = "skyblue", alpha = 0.7) +
  
  # Line Chart for Batting Average
  geom_line(aes(y = `Batting Average`, group = 1), color = "red", size = 1) +
  geom_point(aes(y = `Batting Average`), color = "red", size = 3) +
  
  # Data Labels for Batting Average
  geom_text(aes(y = `Batting Average`, label = `Batting Average`), vjust = -1, color = "red") +
  
  # Data Labels for Runs
  geom_text(aes(y = Runs, label = Runs), vjust = -0.5, color = "blue") +
  
  # Fix Legend Mapping
  scale_fill_manual(name = "Metrics", values = c("Runs" = "skyblue")) +
  scale_color_manual(name = "Metrics", values = c("Batting Average" = "red")) +
  
  # Axis Labels and Title
  labs(title = "Total Runs and Batting Average Over Years",
       x = "Year",
       y = "Value",
       fill = "Metric") +
  
  # Theme for a clean look
  theme_minimal()





