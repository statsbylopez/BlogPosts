####First, read in the data (it's a large data set!)
rm(list = ls())
library(MASS)
library(stringr)
library(tidyverse)
library(patchwork)


## old data
pens <- read_csv("~/Dropbox/NHL code/allpens.csv")
pens <- pens %>% 
  mutate(gid = paste0(season, gcode)) %>% 
  filter(seconds <=3600) %>% 
  mutate(minutes = floor((seconds-1)/60) + 1,
         minutes = pmax(1, minutes), 
         ps = ifelse(gcode > 29999,  "Postseason",  "Regular season"), 
         period.cat = ifelse(period == 1, "1st Period", 
                             ifelse(period == 2, "2nd period", "3rd period")))

n.games <- length(unique(pens$gid))
n.psgames <- length(unique(pens[pens$gcode > 29999,]$gid))
n.reggames <- n.games - n.psgames


function.penalty <- function(ptype){
pen.type <- ptype
pens.type <- subset(pens, ptype == pen.type)


penalty.sum <-  pens.type %>% 
  group_by(ps, minutes, period.cat) %>%
  count() %>% 
  filter(minutes <= 60) %>% 
  mutate(penalties.rate = ifelse(ps == "Postseason", n/n.psgames, n/n.reggames)) %>% 
  ungroup()


rs <- penalty.sum %>% 
  filter(ps == "Regular season") %>% 
  ggplot(aes(minutes, penalties.rate)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~period.cat, scales = "free_x", nrow = 1) + 
  theme(panel.spacing = unit(0, "lines")) + 
  #scale_y_continuous(labels = scales::percent) + 
  theme_bw(14)+ ylab("") + xlab("Minute") + 
  labs(title = paste("Regular season", pen.type, "penalties per minute"))

ps <- penalty.sum %>% 
  filter(ps == "Postseason") %>% 
  ggplot(aes(minutes, penalties.rate)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~period.cat, scales = "free_x", nrow = 1) + 
  theme(panel.spacing = unit(0, "lines")) + 
  #scale_y_continuous(labels = scales::percent) + 
  theme_bw(14) + ylab("") + xlab("Minute") + 
  labs(title = paste("Postseason", pen.type, "penalties by minute"))

return(rs + ps)
}


## Less judgement
function.penalty("Holding")
function.penalty("Tripping")
function.penalty("Roughing")
function.penalty("Interference")

pens %>% group_by(ptype) %>% count() %>% arrange(-n) %>% head(20)

