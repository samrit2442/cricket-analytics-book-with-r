library(pacman)
pacman::p_load(tidyverse, rvest, httr)

match_url <- "https://www.cricbuzz.com/live-cricket-scores/119015/inda-vs-enga-2nd-unofficial-test-india-a-tour-of-england-2025"



live_score <- function(match_url) {
  ss <- rvest::session(match_url, httr::user_agent("Mozilla/5.0"))
  pg <- ss$response |> read_html()
  
  score <- pg |> 
    html_elements(css = "div.cb-col-67.cb-col > div:nth-child(1)") |> 
    html_text2()
  
  raw_text <- score[1]
  
  ln <- str_split(raw_text, "\n")[[1]]
  ln
  
  match_info <- ln[1:3]
  match_info
  
  batter_header <- ln[4:9]
  batter_data <- matrix(ln[10:21], ncol = 6, byrow = TRUE) |> 
    as_tibble(.name_repair = "unique") |>
    set_names(batter_header)
  batter_data
  
  bowler_header <- ln[22:27]
  bowler_data <- matrix(ln[28:39], ncol = 6, byrow = TRUE) |>
    as_tibble(.name_repair = "unique") |>
    set_names(bowler_header)
  bowler_data
  
  key_stats_header <- ln[40]
  key_stats <- ln[41:44]
  
  
  # Step 3: Print output
  cat(glue::glue("{match_info[1]}\n{match_info[2]}\n{match_info[3]}\n\n"))
  
  cat("Batter\n")
  print(batter_data)
  
  cat("\nBowler\n")
  print(bowler_data)
  
  cat("\nKey Stats\n")
  cat(glue::glue("{key_stats[1]}\n{key_stats[2]}\n{key_stats[3]}\n{key_stats[4]}\n\n"))
  
}

live_score(match_url)

url2 <- "https://www.cricbuzz.com/live-cricket-scores/119954/pb-vs-rgr-6th-match-maharashtra-premier-league-2025"
live_score(url2)


ss1 <- rvest::session(url2, httr::user_agent("Mozilla/5.0"))
pg1 <- ss1$response |> read_html()

score1 <- pg1 |> 
  html_elements(css = "div.cb-col-67.cb-col > div:nth-child(1)") |> 
  html_text2()

raw_text1 <- score1[1]
raw_text1
ln1 <- str_split(raw_text1, "\n")[[1]]
ln1


url3 <- "https://www.cricbuzz.com/live-cricket-scores/119954/pb-vs-rgr-6th-match-maharashtra-premier-league-2025"

ss2 <- rvest::session(url3, httr::user_agent("Mozilla/5.0"))
pg2 <- ss2$response |> read_html()

score2 <- pg2 |> 
  html_elements(css = "div.cb-col-67.cb-col > div:nth-child(1)") |> 
  html_text2()

raw_text2 <- score2[1]
raw_text2
ln2 <- str_split(raw_text2, "\n")[[1]]
ln2




url4 <- "https://www.cricbuzz.com/live-cricket-scores/119384/ets-vs-smf-16th-match-t20-mumbai-2025"

ss3 <- rvest::session(url4, httr::user_agent("Mozilla/5.0"))
pg3 <- ss3$response |> read_html()

score3 <- pg3 |> 
  html_elements(css = "div.cb-col-67.cb-col > div:nth-child(1)") |> 
  html_text2()

raw_text3 <- score3[1]
raw_text3
ln3 <- str_split(raw_text3, "\n")[[1]]
ln3

url5 <- ""

ss4 <- rvest::session(url5, httr::user_agent("Mozilla/5.0"))
pg4 <- ss4$response |> read_html()

score4 <- pg4 |> 
  html_elements(css = "div.cb-col-67.cb-col > div:nth-child(1)") |> 
  html_text2()

raw_text4 <- score4[1]
raw_text4
ln4 <- str_split(raw_text4, "\n")[[1]]
ln4

if (length(ln) == 0) {
  cat("No live score data available.\n")
} else if (length(ln) > 0){
  cat("Live score data retrieved successfully.\n")
}

if (length(ln) == 45) {
  match_info <- ln[1:3]
  batter_header <- ln[4:9]
  bowler_header <- ln[22:27]
  
  batter <- matrix(ln[10:21], ncol = 6, byrow = TRUE) |> 
    as.data.frame() |> 
    set_names(batter_header)
  
  bowler <- matrix(ln[28:39], ncol = 6, byrow = TRUE) |>
    as.data.frame() |> 
    set_names(bowler_header)
  
  key_stats_header <- ln[40]
  key_stats <- ln[41:44]
}  else if (length(ln) == 44) {
  match_info <- ln[1:3]
  batter_header <- ln[4:9]
  bowler_header <- ln[17:22]
  
  batter <- matrix(ln[10:15], ncol = 6, byrow = TRUE) |> 
    as.data.frame() |> 
    set_names(batter_header)
  
  bowler <- matrix(ln[16:21], ncol = 6, byrow = TRUE) |>
    as.data.frame() |> 
    set_names(bowler_header)
  
  key_stats_header <- ln[22]
  key_stats <- ln[23:26]
} else if (length(ln) == 4) {
  cat("Unexpected data format.\n")
}


raw_text3
temp <- str_remove_all(raw_text3, "\n")
temp
cat(temp)
ln3





