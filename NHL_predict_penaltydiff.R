####First, read in the data (it's a large data set!)
rm(list = ls())
library(MASS)
library(stringr)
library(tidyverse)



## old data
all.data <- read_csv("~/Dropbox/NHL code/allNHLevents.csv")
all.data1 <- all.data %>% 
  mutate(gid = paste0(seas, gcode)) 

### Penalty difference at end of first period
pens.1 <- all.data1 %>% 
  filter(period == 1, etype == "PENL") %>% 
  mutate(pen.team = case_when(ev.team == hometeam ~ "H", 
                              ev.team == awayteam ~ "A"), 
         major = ifelse(grepl("5 min", type)|grepl("10 min", type), "TRUE", "FALSE")) 

pens.summary <- pens.1 %>% 
  filter(major == "FALSE") %>% 
  group_by(gid) %>% 
  summarise(pens.home = sum(pen.team == "H"), 
            pens.away = sum(pen.team == "A")) %>% 
  mutate(pendiff.home = pens.home - pens.away)

### Score difference at end of first period
first.event.2 <- all.data1 %>% 
  filter(period == 2) %>% 
  group_by(gid) %>% 
  filter(row_number() == 1) %>% 
  mutate(scorediff.home = home.score - away.score) %>% 
  select(gid, scorediff.home)


## Game summary
alldata.summary <- all.data1 %>% 
  select(seas, gcode, hometeam, awayteam, gid, refdate) %>% 
  group_by(gid) %>% 
  filter(row_number() == 1)


## Combine data
pens.combine <- first.event.2 %>% 
  left_join(pens.summary) %>% 
  replace_na(list(pens.home = 0, pens.away = 0, pendiff.home = 0)) %>% 
  inner_join(alldata.summary)


rdate<-"2002-01-01"
pens.combine$date<-as.Date(rdate)+pens.combine$refdate

pens.combine <- pens.combine %>% select(-refdate)

### Overall penalty information

all.data1 %>% 
  filter(etype == "PENL") %>% 
  mutate(pen.team = case_when(ev.team == hometeam ~ "H", 
                              ev.team == awayteam ~ "A"), 
         major = ifelse(grepl("5 min", type)|grepl("10 min", type), "TRUE", "FALSE")) %>% 
  filter(major == "FALSE") %>% 
  group_by(gid) %>% 
  summarise(pens.home = sum(pen.team == "H"), 
            pens.away = sum(pen.team == "A"), 
            total.pens = pens.home + pens.away) %>%
  summarise(mean.pens = mean(total.pens, na.rm = TRUE))
  #filter(total.pens == 5) %>% 
  group_by(total.pens) %>% count()

### Interlude: remainder of game penalty differential

### Penalty difference at end of first period
pens.23 <- all.data1 %>% 
  filter(period > 1, etype == "PENL") %>% 
  mutate(pen.team = case_when(ev.team == hometeam ~ "H", 
                              ev.team == awayteam ~ "A"), 
         major = ifelse(grepl("5 min", type)|grepl("10 min", type), "TRUE", "FALSE")) %>% 
  filter(major == "FALSE") %>% 
  group_by(gid) %>% 
  summarise(pens.home = sum(pen.team == "H"), 
            pens.away = sum(pen.team == "A")) %>% 
  mutate(pendiff.home.future = pens.home - pens.away) %>% 
  select(gid, pendiff.home.future)

pens.combine <- pens.combine %>% 
  left_join(pens.23) %>% 
  replace_na(list(pendiff.home.future = 0)) 


#### Load money line data, available at https://github.com/bigfour/competitiveness/blob/master/data/bigfour_public.rda 
load("/Users/mlopez1/Downloads/bigfour_public.rda")

nhl.outcomes <- bigfour_public %>% 
  filter(sport == "nhl")

nhl.outcomes$date <- as.Date(substr(nhl.outcomes$gameDate, 1, 10))
nhl.outcomes <- nhl.outcomes %>% select(-gameDate) %>% arrange(date)

scrapr.names <- pens.combine %>%  group_by(hometeam) %>% count() %>% 
  filter(!hometeam %in% c("PHX", "ATL"))
bigfour.names <- nhl.outcomes %>% group_by(home_team) %>% count()

data.combine <- data.frame(scrapr.name = scrapr.names$hometeam, bigfour.name = bigfour.names$home_team)
data.combine[5, 1] <- "CGY"
data.combine[6, 1] <- "CAR"
data.combine[7, 1] <- "CHI"
data.combine[8, 1] <- "COL"
data.combine[9, 1] <- "CBJ"
data.combine[17, 1] <- "NSH"
data.combine[18, 1] <- "N.J"
data.combine[29, 1] <- "WSH"
data.combine[30, 1] <- "WPG"


### Merge

nhl.outcomes <- left_join(nhl.outcomes, data.combine, by = c("home_team" = "bigfour.name")) %>% 
  rename(hometeam = scrapr.name) %>% 
  left_join(data.combine, by = c("visitor_team" = "bigfour.name")) %>% 
  rename(awayteam = scrapr.name)
 

pens.combine <- pens.combine %>% 
  mutate(hometeam = ifelse(hometeam == "ATL", "WPG", hometeam), 
         hometeam = ifelse(hometeam == "PHX", "ARI", hometeam),
         awayteam = ifelse(awayteam == "ATL", "WPG", awayteam), 
         awayteam = ifelse(awayteam == "PHX", "ARI", awayteam), 
         playoffs = gcode > 29999) %>%
  filter(!playoffs)

pens.combine.merge <- pens.combine %>% 
  left_join(nhl.outcomes, by = c("date" = "date", "hometeam" = "hometeam", "awayteam" = "awayteam")) %>% 
  ungroup() 


pens.combine.clean <- pens.combine.merge %>% filter(!is.na(visitor_team))
pens.combine.missing <- pens.combine.merge %>% filter(is.na(visitor_team)) %>% 
  mutate(date = date - 1) %>% 
  select(gid:playoffs) %>% 
  left_join(nhl.outcomes, by = c("date" = "date", "hometeam" = "hometeam", "awayteam" = "awayteam")) 


pens.final <- bind_rows(pens.combine.clean, pens.combine.missing) %>% arrange(date, hometeam) %>% filter(!is.na(visitor_team))



pens.final %>% 
  mutate(home.win = home_score > visitor_score) %>% 
  filter(scorediff.home == 0, !is.na(home.win)) %>% 
  mutate(pendiff.cat = case_when(pendiff.home < -1 ~ "a: -2 or less", 
                                 pendiff.home == -1 ~ "b: -1", 
                                 pendiff.home == 0 ~ "c: 0", 
                                 pendiff.home == 1 ~ "d: 1", 
                                 pendiff.home > 1 ~ "e: 2 or more"),
         gameprob.cat = case_when(p_home < 0.45 ~ "0.underdog", 
                                  p_home >=0.45 & p_home < 0.55 ~ "1.tossup", 
                                  p_home >= 0.55 ~ "2.favored")) %>% 
  group_by(pendiff.cat) %>% 
  summarise(ave.homewin = mean(home.win), n.games = n(), 
            ave.pdiff = mean(pendiff.home.future), 
            sd.pdiff = sd(pendiff.home.future), 
            se.pdiff = sd.pdiff/sqrt(n.games))



pens.final <- pens.final %>% 
  mutate(home.win = home_score > visitor_score) %>% 
  filter(scorediff.home == 0, !is.na(home.win)) %>% 
  mutate(pendiff.cat = case_when(pendiff.home < -1 ~ "a: -2 or less", 
                                 pendiff.home == -1 ~ "b: -1", 
                                 pendiff.home == 0 ~ "c: 0", 
                                 pendiff.home == 1 ~ "d: 1", 
                                 pendiff.home > 1 ~ "e: 2 or more"),
         gameprob.cat = case_when(p_home < 0.49 ~ "0.underdog", 
                                  p_home >=0.45 & p_home < 0.55 ~ "1.slight.fave", 
                                  p_home >=0.55 & p_home < 0.65 ~ "2.moderate.fave", 
                                  p_home >= 0.60 ~ "3.bigfave")) 

pens.final %>% 
  group_by(gameprob.cat, pendiff.cat) %>% 
  summarise(ave.homewin = mean(home.win), n.games = n())

library(splines)
summary(glm(home.win ~ pendiff.home + p_home, data = pens.final, family = "binomial"))
summary(glm(home.win ~ pendiff.home + p_home, data = pens.final))

library(mgcv)
m1 <- bam(home.win ~ s(p_home, k = 50, by = pendiff.home), 
          data = pens.final, method = "fREML", 
          discrete = TRUE, family = binomial(link='logit'))
AIC(m1)

#m2 <- bam(home.win ~ s(p_home) + (pendiff.home), 
#          data = pens.final, method = "fREML", 
#          discrete = TRUE, family = binomial(link='logit'))
#AIC(m2)
#

pre <- expand.grid(pendiff.home = seq(-3, 3, 1), p_home = seq(.35, .7, .01))
pre$predict <- predict.gam(m1, pre, type = "response")

p <- ggplot(pre, aes(x=p_home, y=pendiff.home)) + 
  geom_tile(aes(fill = predict-0.5)) + 
  scale_fill_gradient2("Home win rate", low = "#af8dc3", mid = "white", high = "#7fbf7b",
                       lim = (c(0.45, 0.6)-0.5), 
                       breaks = (c(0.45, 0.5, 0.55, 0.60)-0.5), 
                       labels = c("45%", "50%", "55%", "60%"))+
  xlab("pre-game win probability, home team") + ylab("Period 1 penalty differential") + 
  labs(title = "Win probability, end of first period", 
       subtitle = "2006-2016 regular season tied games") + theme_bw(14) 
p

ggsave(p, file = "~/Dropbox/mlopez/mlopez/static/img/makeupF2.png")


### Future penalties

library(mgcv)
m1 <- bam(pendiff.home.future ~ s(p_home, k = 50, by = pendiff.home), 
          data = pens.final, method = "fREML", 
          discrete = TRUE)
AIC(m1)



pre <- expand.grid(pendiff.home = seq(-3, 3, 1), p_home = seq(.35, .7, .01))
pre$predict <- predict.gam(m1, pre)

p <- ggplot(pre, aes(x=p_home, y=pendiff.home)) + 
  geom_tile(aes(fill = predict)) + 
  scale_fill_gradient2("Period 2/3 penalty differential", low = "#af8dc3", mid = "white", high = "#7fbf7b")+
  xlab("pre-game win probability, home team") + ylab("Period 1 penalty differential") + 
  labs(title = "Future penalty differential", 
       subtitle = "2006-2016 regular season tied games") + 
  theme_bw(14) 
p
ggsave(p, file = "~/Dropbox/mlopez/mlopez/static/img/makeupF1.png")


pens.final %>% 
  group_by(pendiff.home) %>% 
  summarise(future.diff = mean(pendiff.home.future))

