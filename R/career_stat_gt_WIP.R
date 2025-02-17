library(dplyr)
library(tidyr)
library(gt)

# Sample data
df <- tibble(
  Runs = 4027,
  Balls = 2870,
  Fours = 364,
  Sixes = 194,
  Dots = 1098,
  NO = 13,
  `30s` = 16,
  `50s` = 31,
  `100s` = 4,
  Innings = 145,
  `Batting Average` = 30.5,
  `Runs Per Innings` = 27.8,
  `Strike Rate` = 140,
  `Highest Score` = '118 (43) vs Sri Lanka'
) |> 
  mutate(across(everything(), as.character))

# Transpose the data
df_long <- df |>
  pivot_longer(cols = everything(), names_to = "Stat", values_to = "Value")

# Create gt table
df_long |>
  gt() |>
  # cols_label(Stat = "Cricket Statistic", Value = "Value") %>%
  # Format integer values (except last 3 rows)
  fmt_number(
    columns = Value,
    rows = 1:(nrow(df_long) - 4),  # Apply to all except last 3 rows
    decimals = 0
  ) |>
  # Format last 3 rows with 1 decimal place
  fmt_number(
    columns = Value,
    rows = (nrow(df_long) - 2):nrow(df_long),
    decimals = 1
  ) |>
  tab_options(column_labels.hidden = TRUE) |>
  # Table Styling
  tab_header(title = md(paste0("**", plyr_name, " Batting Statistics🏏 in T20I**"))) |> 
  opt_table_font(font = google_font("Outfit"), size = px(21)) |> 
  tab_options(
    table.width = px(400),
    column_labels.font.weight = "bold",
    heading.title.font.size = px(18),
    table.font.size = px(14)
  ) |>
  tab_style(
    style = cell_text(weight = "bold", align = "left"),
    locations = cells_body(columns = Stat)
  ) |>
  tab_options(table.align = "center") |> 
  tab_style(
    style = cell_fill(color = "#f4f4f4"),
    locations = cells_body(rows = seq(1, nrow(df_long), by = 2))  # Alternating row colors
  ) |> 
  # Add footnote
  tab_footnote(
    footnote = "Afghanistan matches are excluded.",
    locations = cells_title(groups = "title")  # Adds footnote to the title
  )
