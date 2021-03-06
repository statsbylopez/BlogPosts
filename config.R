# local configuration hack

if (Sys.info ()["user"] == "Brian") {
  # For Brian
  constants <- "C:/Users/Brian/Dropbox/Sim/Constants/"
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

if (Sys.info ()["user"] == "jenkins") {
  # For Apple Sim Box
  constants <- "/Volumes/Mac_HD/Dropbox/Sim/Constants/"
  output <- "/Volumes/Mac_HD/Dropbox/Sim/Model/2018 Outcomes/"
  gamefiles <- "/Volumes/Mac_HD/Dropbox/Sim/Model/gamedata/"
  gamedates <- "/Volumes/Mac_HD/Dropbox/Sim/Model/constants/"
  ModelLocation <- "/Volumes/Mac_HD/Dropbox/Sim/Model"
  mljcode <- "/Volumes/Mac_HD/Dropbox/Sim"
}

if (Sys.info()["user"] == "doherbr") {
  # For doherbr
  constants <- "C:/Users/doherbr/desktop/Dropbox/Sim/Constants/"
  output <- "C:/Users/doherbr/desktop/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "C:/Users/doherbr/desktop/Dropbox/sim/Model/gamedata/"
  gamedates <- "C:/Users/doherbr/desktop/Dropbox/sim/Model/constants/"
  ModelLocation <- "C:/Users/doherbr/desktop/Dropbox/Sim/Model/"
  mljcode <- "C:/Users/doherbr/desktop/Dropbox/Sim/"
}



if (Sys.info ()["user"] == "btdoh"){
  # For brian laptop
  constants <- "C:/Users/btdoh/Dropbox/Sim/Constants/"
  output <- "C:/Users/btdoh/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "C:/Users/btdoh/Dropbox/sim/Model/gamedata/"
  gamedates <- "C:/Users/btdoh/Dropbox/sim/Model/constants/"
  ModelLocation <- "C:/Users/btdoh/Dropbox/Sim/Model/"
  mljcode <- "C:/Users/btdoh/Dropbox/Sim/"
}

if (Sys.info ()["user"] == "brian" & Sys.info()['sysname'] =="Windows") {
  # For Brian laptop
  constants <- "C:/Users/brian/Dropbox/Sim/Constants/"
  output <- "C:/Users/brian/Dropbox/Sim/2018 Outcomes/"
  gamefiles <- "C:/Users/brian/Dropbox/sim/Model/gamedata/"
  gamedates <- "C:/Users/brian/Dropbox/sim/Model/constants/"
  ModelLocation <- "C:/Users/brian/Dropbox/Sim/Model/"
  mljcode <- "C:/Users/brian/Dropbox/Sim/"
}


#if (Sys.info ()["user"] == "brian") {
#  # For Brian
#  constants <- "/home/brian/Dropbox/Sim/Constants/"
#  output <- "/home/brian/Dropbox/Sim/2018 Outcomes/"
#  gamefiles <- "/home/brian/Dropbox/sim/Model/gamedata/"
#  gamedates <- "/home/brian/Dropbox/sim/Model/constants/"
#  ModelLocation <- "/home/brian/Dropbox/Sim/Model/"
#  mljcode <- "/home/brian/Dropbox/Sim/"
#}

library(knitr)
library(readxl)
library(readr)
library(profvis)
library(dplyr)
library(doParallel)
library(ggplot2)
library(tidyr)
