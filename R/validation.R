library(pacman)
pacman::p_load(tidyverse, cricketr)

ys <- getPlayerDataTT(36084)

ys1 <- ys |> 
  dplyr::mutate(`Start Date` = dmy(`Start Date`)) |> 
  dplyr::arrange(`Start Date`) |> 
  dplyr::filter(Runs != 'DNB' & Runs != 'TDNB')











