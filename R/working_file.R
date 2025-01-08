library(pacman)
pacman::p_load(tidyverse, readxl, readr)

test_mat_data <- read.csv(file.choose())

test_mat_data$shot |> unique()












