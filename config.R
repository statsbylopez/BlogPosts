# local configuration hack

if (Sys.info()["user"] == "Brian") {
  # For Brian
  constants <- "C:/Users/Brian/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/Brian/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "C:/Users/Brian/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
}

if (Sys.info()["user"] == "mlopez1") {
  # For Mike
  constants <- "~/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "~/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
}


library(knitr)
library(readxl)
library(readr)
library(profvis)
library(dplyr)
library(doParallel)
