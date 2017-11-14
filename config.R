# local configuration hack

if (http://Sys.info ()["user"] == "doherbr") {
  # For Brian-JHK
  constants <- "C:/Users/doherbr/Desktop/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/doherbr/Desktop/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "C:/Users/doherbr/Desktop/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "C:/Users/doherbr/Desktop/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
  ModelLocation <- "C:/Users/doherbr/Desktop/Dropbox/Sim/MJL_code/12-16-2016/Model/"
  mljcode <- "C:/Users/doherbr/Desktop/Dropbox/Sim/MJL_code/"
}

if (http://Sys.info ()["user"] == "Brian") {
  # For Brian
  constants <- "C:/Users/Brian/Desktop/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/Brian/Desktop/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "C:/Users/Brian/Desktop/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "C:/Users/Brian/Desktop/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
  ModelLocation <- "C:/Users/Brian/Desktop/Dropbox/Sim/MJL_code/12-16-2016/Model/"
}
if (Sys.info()["user"] == "mlopez1") {
  # For Mike
  constants <- "~/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "~/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
  ModelLocation <- "~/Dropbox/Sim/MJL_code/12-16-2016/Model/"
}


if (Sys.info()["user"] == "elopez") {
  # For Erin
  constants <- "C:/Users/elopez/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "C:/Users/elopez/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "C:/Users/elopez/Dropbox/Sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "C:/Users/elopez/Dropbox/Sim/MJL_code/12-16-2016/Model/constants/"
  ModelLocation <- "C:/elopez/Dropbox/Sim/MJL_code/12-16-2016/Model/"
}

if (Sys.info()["user"] == "jenkins") {
  # For Apple Sim Box
  constants <- "~/Dropbox/Sim/MJL_code/MLconstants/"
  output <- "~/Dropbox/Sim/MJL_code/Outcomes/"
  gamefiles <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/gamedata/"
  gamedates <- "~/Dropbox/sim/MJL_code/12-16-2016/Model/constants/"
  ModelLocation <- "~/Dropbox/Sim/MJL_code/12-16-2016/Model/"
}


library(knitr)
library(readxl)
library(readr)
library(profvis)
library(dplyr)
library(doParallel)
library(ggplot2)
