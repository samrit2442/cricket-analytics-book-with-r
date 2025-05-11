library(pacman)
pacman::p_load(tidyverse, rvest, chromote)

match_url <- "https://www.cricbuzz.com/cricket-scores/114960/kkr-vs-rcb-1st-match-indian-premier-league-2025"

mt <- read_html_live(match_url)

str(mt)

mt$html_elements()


  




























