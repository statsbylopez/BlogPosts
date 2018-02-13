# local configuration hack

if (Sys.info()["user"] == "Brian") {
  # For Brian
  constants <- "C:/Users/Brian/Dropbox/Constants/"
  output <- "C:/Users/Brian/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "C:/Users/Brian/Dropbox/sim/Model/gamedata/"
  gamedates <- "C:/Users/Brian/Dropbox/sim/Model/constants/"
  ModelLocation <- "C:/Users/Brian/Dropbox/Sim/Model/"
  mljcode <- "C:/Users/Brian/Dropbox/Sim/"
}

if (Sys.info()["user"] == "mlopez1") {
  # For Mike
  constants <- "~/Dropbox/Sim/Constants/"
  output <- "~/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "~/Dropbox/sim/Model/gamedata/"
  gamedates <- "~/Dropbox/sim/Model/constants/"
  ModelLocation <- "~/Dropbox/Sim/Model/"
  mljcode <- "~/Dropbox/Sim/"
}


if (Sys.info()["user"] == "elopez") {
  # For Erin
  constants <- "C:/Users/elopez/Constants/"
  output <- "C:/Users/elopez/Dropbox/2018 Outcomes/"
  gamefiles <- "C:/Users/elopez/Dropbox/Sim/Model/gamedata/"
  gamedates <- "C:/Users/elopez/Dropbox/Sim/Model/constants/"
  ModelLocation <- "C:/elopez/Dropbox/Sim/Model/"
  mljcode <- "C:/elopez/Dropbox/Sim/"
}

if (Sys.info()["user"] == "jenkins") {
  # For Apple Sim Box
  constants <- "~/Dropbox/Sim/Constants/"
  output <- "~/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "~/Dropbox/sim/Model/gamedata/"
  gamedates <- "~/Dropbox/sim/Model/constants/"
  ModelLocation <- "~/Dropbox/Sim/Model/"
  mljcode <- "~/Dropbox/Sim/"
}

if (Sys.info()["user"] == "doherbr") {
  # For Brian-JHK
  constants <- "C:/Users/doherbr/Dropbox/Constants/"
  output <- "C:/Users/doherbr/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "C:/Users/doherbr/Dropbox/sim/Model/gamedata/"
  gamedates <- "C:/Users/doherbr/Dropbox/sim/Model/constants/"
  ModelLocation <- "C:/Users/doherbr/Dropbox/Sim/Model/"
  mljcode <- "C:/Users/doherbr/Dropbox/Sim/"
}


library(knitr)
library(readxl)
library(readr)
library(profvis)
library(dplyr)
library(doParallel)
library(ggplot2)
