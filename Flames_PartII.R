## Required packages
rm(list = ls())
library(MASS)
library(XML)
library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)

## Read in the data
load(url("http://data.war-on-ice.net/nhlscrapr-20152016.RData"))
grand.data.new <- grand.data
grand.data.new <- dplyr::select(grand.data.new, -c(etext, type2))
grand.data.new$seas <- "20152016"
grand.data <- grand.data.new



## Label a penalty as a minor, create penalty id (season, gcode, seconds)
nhl.data <- grand.data
nhl.data2 <- mutate(nhl.data, id = paste(season, gcode, seconds), 
                    minor = grepl("2 min", type)) 


## Identify times where there were multiple penalties
pens.freq <- nhl.data2 %>%
  filter(etype == "PENL") %>%
  group_by(id) %>%
  summarize(id.pen = length(id)) %>%
  filter(id.pen > 1)

## Drop penalty team names that don't make sense 
names(table(nhl.data2$ev.team))
nhl.data2 <- filter(nhl.data2, !id %in% pens.freq$id, 
                    !ev.team %in% c("","CHL", "CLO", "Ear",  
                                 "GOA", "HAN", "HIG",
                                 "HOM", "ICE", "ICI", 
                                 "Lea", "NET", "OFF", "Per", 
                                 "PLA", "PRE", "PUC", "OBJ",
                                 "REF", "RIN", "Sho", "SWI",
                                 "TV ", "VID","VIS", "Tea")) 

## Identify games after the Wideman penalty (done by game ID)
nhl.data2$post <- nhl.data2$gcode > 20742

## Summarize penalty rates
nhl.data3 <- nhl.data2 %>% 
  group_by(post, ev.team, gcode) %>% 
  summarise(total.count = sum(etype == "PENL" & minor)) %>% 
  ungroup() %>% 
  group_by(post, ev.team) %>% dplyr::summarise(ave.game = mean(total.count)) %>%
  mutate(type = ifelse(post, 1, 0))

## Store the rates for pre and post, done by team
nhl.data4 <- data.frame(ev.team = nhl.data3[1:30,2], rate = nhl.data3[31:60,3]/nhl.data3[1:30,3]-1)

## Barchart comparing pre and post Wideman penalty rates
ggplot(nhl.data4, aes(reorder(ev.team, ave.game), ave.game, fill = !ev.team == "CGY")) + 
  geom_bar(stat = "identity") + xlab("Team") + ylab("Penalty rate change") +
  labs(title = "Change in penalty rate post Wideman hit, 2015-16 season", 
       caption = "nhlscrapr package") + theme(legend.position="none") + ylim(c(-0.4, 0.4))

