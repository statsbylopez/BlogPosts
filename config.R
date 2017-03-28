# local configuration hack

if (Sys.info()["user"] == "Brian") {
  # For Brian
  constants <- "C:/Users/Brian/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/Brian/Dropbox/Sim/MJL_code/Outcomes/"
}

if (Sys.info()["user"] == "mlopez1") {
  # For Mike
  constants <- "~/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "~/Dropbox/Sim/MJL_code/Outcomes/"
}


library(knitr)
library(readxl)
library(readr)
library(profvis)
library(dplyr)
library(doParallel)
