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






