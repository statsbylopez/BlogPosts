library(ggplot2)
library(dplyr)
library(broom)
library(randomForest)
library(tidyr)
library(nflscrapR)
library(RColorBrewer)
library(readr)
library(readxl)
library(GGally)



#############################################
##### Lock and Nettleton model: requires Armchair Analysis data
#############################################


###### Lock & Nettleton model (requires some wrangling)
setwd("~/Dropbox/nfl_00-15/csv")
forest = get(load("winprobmodrf.RData"))
forest$importance
             
play <- read_csv("PLAY.csv")
game <- read_csv("GAME.csv")
team <- read_csv("TEAM.csv")

play1 <- play %>%
  mutate(team.temp = off,
         off = ifelse(dwn == 4 & type == "PUNT", def, off),
         def = ifelse(dwn == 4 & type == "PUNT", team.temp, def)) %>%
  mutate(points.temp = ptso,
         ptso = ifelse(dwn == 4 & type == "PUNT", ptsd, ptso),
         ptsd = ifelse(dwn == 4 & type == "PUNT", points.temp, ptsd))

play2 <- play1 %>%
  mutate(ptsdiff = ptso-ptsd,
         seconds = 15*60*(4-qtr) + (60*min) + sec,
         score.lev = ptsdiff/sqrt(seconds + 1),
         totalpoints = ptso + ptsd) %>%
  filter(qtr <5) 

sample_n(play2, 5)

LockNett <- left_join(play2, game)
LockNett$Home <- ifelse(LockNett$off == LockNett$h, 1, 0)
LockNett$won.home <- ifelse(LockNett$ptsh > LockNett$ptsv, 1, 0)
LockNett$won.off <- ifelse(LockNett$Home == 1 & LockNett$won.home == 1 | LockNett$Home == 0 & LockNett$won.home == 0, 1, 0)
LockNett.test <- filter(LockNett, seas == 2016)

##Probability of winning
LockNett.test$winprob.off <- predict(forest, LockNett.test)

##Compare those win probability categories with the true mean outcomes within that group.
LockNett.test <- mutate(LockNett.test, won.off = as.numeric(as.character(won.off)), 
                      qtr.cat = ifelse(qtr == 1, "Quarter 1", ifelse(qtr == 2, "Quarter 2", 
                               ifelse(qtr == 3, "Quarter 3", "Quarter 4"))))


#############################################
##### Other models 
#############################################

## phdFootball
phd.wp <- read.csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/WinProb.17/phdFootball.csv")
phd.wp <- filter(phd.wp, quarter!="OT") %>%
  mutate(qtr = ifelse(quarter == "Q1", 1, 
                      ifelse(quarter == "Q2", 2, 
                             ifelse(quarter == "Q3", 3, 4)))) %>% 
  mutate(Pats.wp = ifelse(offense_team == "New England Patriots", wp/100, 1-wp/100), 
         seconds = seconds_left + 900*(4-qtr)) %>%
  arrange(-seconds) %>%
  mutate(Pats.delta = lead(Pats.wp, 1) - Pats.wp) 


## CMU model
cmu.wp <- game_play_by_play(2017020500)
cmu.wp <- filter(cmu.wp, !is.na(Away.WP.pre), as.numeric(as.character(qtr)) < 5) %>%
  mutate(Pats.wp = Away.WP.pre, 
         Pats.delta = lead(Pats.wp, 1) - Pats.wp, 
         seconds = TimeSecs) %>% 
  slice(-n())


## PFR model
pfr.wp <- read.csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/WinProb.17/pfr.csv")
pfr.wp <- filter(pfr.wp, !is.na(Win.),  Quarter!="OT") %>% 
  mutate(Pats.wp = lag(1 - Win./100, 0), qtr = (as.numeric(as.character(Quarter)))) %>%
  select(-Detail) %>%
  separate(Time, c("min", "sec"), sep = ":", remove=FALSE) %>% 
  mutate(min = as.numeric(as.character(min)), sec = as.numeric(as.character(sec)), 
         seconds = 15*60*(4-qtr) + (60*min) + sec, 
         Pats.delta = Pats.wp - lag(Pats.wp, 1))


## Lock-Nettleton model in the Super Bowl
LockNett.Pats <- LockNett.test %>%
  filter(gid == 4523) %>%
  mutate(Pats.wp = ifelse(off == "NE", winprob.off, 1 - winprob.off), 
         Pats.pts = ifelse(off == "NE", ptso, ptsd), 
         Falcons.pts = ifelse(off == "NE", ptsd, ptso), 
         Pats.delta = lead(Pats.wp, 1) - Pats.wp) %>%
  group_by(qtr) %>%
  mutate(play.time = 1:n()/n()*0.25 + (as.numeric(as.character(qtr))-1)/4)


## Gambletron
Gambletron.wp <- read.csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/WinProb.17/Gambletron.csv")
## Halftime is 8:03 to 8:35, so drop those, Overtime starts at 10:14, so drop those
Gambletron.wp <- mutate(Gambletron.wp, Pats.wp = ne_win_prob, row.index = 1:n()) %>% 
  mutate(quarter = ifelse(row.index < 192, 1, 
                          ifelse(row.index < 341, 2, 
                                 ifelse(row.index < 700, 3, 4)))) %>%
  filter(row.index > 78, row.index < 923) %>% 
  filter(row.index < 341 | row.index > 526) %>% 
  group_by(quarter) %>% 
  mutate(seconds = (900 - 1:n()/n()*900) + 900*(4-quarter)) %>% ungroup()


## ESPN accessed remotely
espn.wp <- read.csv("~/Dropbox/BlogPosts/BlogPosts/WinProb.17/espn.csv")
espn.wp <- filter(espn.wp,  QUARTER!="OVERTIME") %>% 
  mutate(Pats.wp = ifelse(TEAM == "New England Patriots", TEAM_WP_START, 1 - TEAM_WP_START), 
         QUARTER = as.numeric(as.character(QUARTER))) %>%
  separate(CLOCK_TIME, c("min", "sec"), sep = ":", remove=FALSE) %>% 
  mutate(min = as.numeric(as.character(min)), sec = as.numeric(as.character(sec)), 
         seconds = 15*60*(4-QUARTER) + (60*min) + sec, 
         Pats.delta = lead(Pats.wp, 1) - Pats.wp)


ann_text <- data.frame(x = c(450, 450, 450, 450, 450, 450), 
                       y1 = c(0.95, 0.90, 0.85, 0.8, 0.75, 0.7), 
                       y2 = c(950, 900, 850, 800, 750, 700),
                       lab = c("Pro Football Reference", "ESPN", "phdFootball", "nflscrapr", 
                               "Lock & Nettleton", "Gambletron"), 
                       int = c(min(pfr.wp$Pats.wp), min(espn.wp$Pats.wp), min(phd.wp$Pats.wp), 
                               min(cmu.wp$Pats.wp), min(LockNett.Pats$Pats.wp), min(Gambletron.wp$Pats.wp)), 
                       seconds = c(pfr.wp$seconds[which.min(pfr.wp$Pats.wp)], 
                                   espn.wp$seconds[which.min(espn.wp$Pats.wp)], 
                                   phd.wp$seconds[which.min(phd.wp$Pats.wp)], 
                                   cmu.wp$seconds[which.min(cmu.wp$Pats.wp)], 
                                   LockNett.Pats$seconds[which.min(LockNett.Pats$Pats.wp)], 
                                   Gambletron.wp$seconds[which.min(Gambletron.wp$Pats.wp)]))
cols <- c(brewer.pal(5,"Set1"), "#A6611A")

cols <- brewer.pal(6, "Set1")

ggplot(LockNett.Pats, aes(3600-seconds, Pats.wp)) + geom_path(colour = cols[5]) + 
  scale_x_continuous("",labels = c("Q1", "Q2", "Q3", "Q4", "End of Reg"), 
                     breaks = c(0, 900, 1800, 2700, 3600)) + 
  scale_y_continuous(lim = c(0, 1), "", labels=scales::percent) + 
  geom_text(data = ann_text,aes(x = x, y = y1, label = lab), 
            colour = cols) +
  geom_path(data = pfr.wp, colour = cols[1]) + 
  geom_path(data = espn.wp, colour = cols[2]) +
  geom_path(data = phd.wp, colour = cols[3]) +  
  geom_path(data = cmu.wp, colour = cols[4]) + 
  geom_path(data = Gambletron.wp, colour = cols[6]) + 
  labs(title = "Super Bowl win probability") + 
  theme_grey(14)


p <- ggplot(LockNett.Pats, aes(3600-seconds, 1/(Pats.wp/(1-Pats.wp)))) + 
  geom_path(colour = cols[5]) + 
  scale_x_continuous("",labels = c("Q2", "Q3", "Q4", "End of Reg"), 
                     breaks = c(900, 1800, 2700, 3600), lim = c(1800, 3600)) + 
  scale_y_continuous("", breaks = c(20, 100, 200, 500, 1000), 
                     labels = c("20:1", "100:1", "200:1", "500:1", "1000:1")) + 
  geom_path(data = cmu.wp, colour = cols[2]) + 
  geom_text(data = ann_text, aes(x = x + 1500, y = y2, label = lab), 
            colour = cols) +
  geom_path(data = pfr.wp, colour = cols[1]) + 
  geom_path(data = espn.wp, colour = cols[2]) +
  geom_path(data = phd.wp, colour = cols[3]) +  
  geom_path(data = cmu.wp, colour = cols[4]) + 
  geom_path(data = Gambletron.wp, colour = cols[6]) + 
  labs(title = "Super Bowl win odds (New England)")


ints <- 1:nrow(ann_text)
for (i in ints){
  p <- p + geom_segment(x = rep(900, nrow(ann_text))[i], y = 1/(ann_text$int/(1-ann_text$int))[i], 
                        xend = 3600 - ann_text$seconds[i], yend = 1/(ann_text$int/(1-ann_text$int))[i],
                        colour = cols[i], lty = 2)   
}
p + theme_grey(14)



#############################################
##### Compare differences in WPA
#############################################

### first filter into plays that they have in common

el1 <- LockNett.Pats %>% filter(type == "RUSH"|type == "PASS", Pats.delta < 0.3) %>% group_by(seconds) %>% count() %>% filter(n == 1) 
el2 <- pfr.wp %>% group_by(seconds) %>% count() %>% filter(n == 1) 
el3 <- cmu.wp %>% group_by(seconds) %>% count() %>% filter(n == 1) 
el4 <- phd.wp %>% group_by(seconds) %>% count() %>% filter(n == 1) 
el5 <- espn.wp %>% group_by(seconds) %>% count() %>% filter(n == 1) 

el.all <- Reduce(intersect, list(el1$seconds, el2$seconds, el3$seconds, el4$seconds, el5$seconds))


winprob.Pats.all <- data.frame(seconds = rev(el.all), 
                               `Lock_Nettleton` = LockNett.Pats$Pats.delta[LockNett.Pats$seconds %in% el.all], 
                               `Pro_Football_Reference` = pfr.wp$Pats.delta[pfr.wp$seconds %in% el.all], 
                               `nflscrapR` = cmu.wp$Pats.delta[cmu.wp$seconds %in% el.all], 
                               `PhDfootball` = phd.wp$Pats.delta[phd.wp$seconds %in% el.all], 
                               `ESPN` = espn.wp$Pats.delta[espn.wp$seconds %in% el.all])

winprob.Pats.all %>% mutate(agree.all = (Lock_Nettleton > 0) & (Pro_Football_Reference) > 0 & (nflscrapR > 0) & (PhDfootball > 0) & (ESPN > 0)| 
                              (Lock_Nettleton < 0) & (Pro_Football_Reference < 0) & (nflscrapR < 0) & (PhDfootball < 0) & (ESPN < 0) ) %>%
  count(agree.all)



library(GGally)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_abline(intercept = 0, slope = 1, lty = 2) + 
    scale_x_continuous(lim = c(-.2, .2)) + 
    scale_y_continuous(lim = c(-.2, .2))
  p
}

my_fn2 <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_histogram(fill = "black") + scale_x_continuous(lim = c(-0.2, 0.2))
  p
}

ggpairs(winprob.Pats.all[,2:ncol(winprob.Pats.all)], lower = list(continuous = my_fn), diag = list(continuous = my_fn2)) + 
  labs(title = "New England's win probability added per play in Super Bowl 51", 
       subtitle = "Rushes and passes only (either team on offense)")  + theme_grey(14)




#############################################
##### Check the Lock Nettleton model
#############################################

## Sample 5 plays per quarter per game
set.seed(1)
LockNett.test.samp <- LockNett.test %>%
  group_by(gid, qtr.cat) %>%
  sample_n(5) %>% 
  ungroup()

## Binned points - every 0.05
LockNett.binned <- LockNett.test.samp %>%
  mutate(p_home_bin = round(winprob.off/0.05)*0.05) %>%
  group_by(qtr.cat, p_home_bin) %>%
  summarize(N = n(), home_win_bin_pct = mean(won.off))

ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25), 
                       lab = c("More wins\nthan expected", "Less wins\nthan expected"),
                       qtr.cat = factor("Quarter 1"))

rf_plot <- ggplot(data = LockNett.test.samp, aes(x = winprob.off, y = as.numeric(won.off), 
                           group = qtr.cat)) + 
  geom_point(data = LockNett.binned,  
             aes(x = p_home_bin, y = home_win_bin_pct, size = N), alpha = 0.5) + 
  geom_smooth(method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() + 
  scale_x_continuous("Estimated offensive team win probability", labels = scales::percent, limits = c(0,1)) + 
  scale_y_continuous("Observed offensive team win probability",  labels = scales::percent, limits = c(0,1)) + 
  scale_color_brewer(palette = "Spectral", name = NULL, guide = FALSE) +
  geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  facet_wrap(~ qtr.cat)
rf_plot + 
  labs(title = "Win probability model: sample of 2016 plays", 
        subtitle = "Lock and Nettleton's random forest model (JQAS, 2014)") +theme_gray(16)









