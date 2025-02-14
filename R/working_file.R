library(pacman)
pacman::p_load(tidyverse, readxl, reader, plyr)

test_mat_data <- read.csv(file.choose())

test_mat_data$shot |> unique()

date <- format(Sys.time(), c("%d","%b","%Y","_","%H","%M"))
dataset_name <- paste0(date, collapse = '') |> toupper()
dataset_name <- paste0("./data/DATASET_", dataset_name)


url <- "https://cricsheet.org/downloads/t20s_male_csv2.zip"
download.file(url, dest = "./data/bbb_dataset.zip", mode = "wb")
unzip("./data/bbb_dataset.zip", exdir = dataset_name)


readme <- list.files(path = dataset_name, pattern = "*.txt", full.names = TRUE)

# The last 3 matches data
n.readLines(readme, header = FALSE, n = 3,  skip = 121)

raw_data <- list.files(path = dataset_name, pattern = "*.csv", full.names = TRUE)

## Total number of matches
N <- length(raw_data)

t20_data = vector(mode = "list", length = N/2)

for (i in 1:(N/2))
{
  t20_data[[i]] = read.csv(file = raw_data[2*i - 1], header = T)
}

## Merging ball by ball data of each match altogether
t20 = rbind.fill(t20_data)

# t20$match_id |> unique() |> length()

date1 <- paste0(date, collapse = "") |> toupper()
saveRDS(t20, file = paste0("./data/t20_raw_data", date1, ".rds"))


# ------------------------------------------------------------------------------

# Cleaning Match by Match Data

t20_mat_data <- list.files(path = "./data/DATASET_09JAN2025_1219",
                             pattern = "*_info.csv",
                             full.names = T)

md <-  vector(mode = "list", length = length(t20_mat_data))
ms_full <- vector(mode = "list", length = length(t20_mat_data))

# test_df = read.csv2(file = t20_mat_data[1000], row.names = NULL, header = F)


match_summary <- function(df) {
  
  df_1 <- df |>
    separate_wider_delim(V1,
                         delim = ",",
                         names = c("C1", "C2", "C3", "C4", "C5"),
                         too_few = "align_start")
  
  teams <- df_1 |> filter(C2 == "team") |> pull(C3)
  st_date <- df_1 |> filter(C2 == "date") |> pull(C3) |> as.Date()
  city <- df_1 |> filter(C2 == "city") |> pull(C3)
  venue <- df_1 |> filter(C2 == "venue") |> pull(C3)
  toss_winner <- df_1 |> filter(C2 == "toss_winner") |> pull(C3)
  pom <- df_1 |> filter(C2 == "player_of_match") |> pull(C3)
  event <- df_1 |> filter(C2 == "event") |> pull(C3)
  team_win <- df_1 |> filter(C2 == "winner") |> pull(C3)
  out <- df_1 |> filter(C2 == "outcome") |> pull(C3)
  win_runs <- df_1 |> filter(C2 == "winner_runs") |> pull(C3)
  win_wkts <- df_1 |> filter(C2 == "winner_wickets") |> pull(C3)
  players <- df_1 |> filter(C2 == "player" | C2 == "players") |> pull(C4)
  

  if (identical(pom, character(0)))
    pom <- NA
  if (identical(win_runs, character(0)))
    win_runs <- NA
  if (identical(win_wkts, character(0)))
    win_wkts <- NA
  if (identical(out, character(0)))
    out <- NA
  if (identical(team_win, character(0)))
    team_win <- NA
  
  ddd <- tibble(team_1 = teams[1],
                team_2 = teams[2],
                start_date = st_date[1],
                venue = paste0(city, ", ", venue),
                event = event[1],
                toss_winner = toss_winner,
                winner = team_win,
                player_of_match = pom[1],
                outcome = out,
                winner_runs = win_runs,
                winner_wickets = win_wkts,
                players = players)
  
  return(ddd)
}


tictoc::tic()
for (i in 1:(length(t20_mat_data)))
{
  md[[i]] = read.csv2(file = t20_mat_data[i], row.names = NULL, header = F)
  ms_full[[i]] = match_summary(df = md[[i]])
  mid <- str_extract(t20_mat_data[i], "(\\d+)(?!.*\\d)")
  ms_full[[i]] = add_column(ms_full[[i]], match_id = mid, .after = 0)
}
tictoc::toc()

match_sum <- plyr::rbind.fill(ms_full)

saveRDS(match_sum, file = paste0("./data/t20_raw_match_data", "09JAN2025_1219", ".rds"))

# ------------------------------------------------------------------------------

