# local configuration hack

if (Sys.info()["user"] == "Brian") {
  # For Brian
  constants <- "C:/Users/Brian/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/Brian/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "C:/Users/Brian/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "C:/Users/Brian/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
}

if (Sys.info()["user"] == "mlopez1") {
  # For Mike
  constants <- "~/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "~/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
}


if (Sys.info()["user"] == "elopez") {
  # For Erin
  constants <- "C:/Users/elopez/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/elopez/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "C:/Users/elopez/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "C:/Users/elopez/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
}


library(knitr)
library(readxl)
library(readr)
library(profvis)
library(dplyr)
library(doParallel)
library(ggplot2)
