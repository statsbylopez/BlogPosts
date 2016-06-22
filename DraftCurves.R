rm(list = ls())
library(XML)
library(plyr)
library(dplyr)
library(ggplot2)


year <- 1990:2015


## NFL
nfl.all <- NULL
for (i in year){
  url <- paste0("http://www.pro-football-reference.com/years/", i, "/draft.htm")
  tables <- readHTMLTable(url)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  nfl <- tables[[which.max(n.rows)]]
  nfl <- select(nfl[,1:12], Rnd, Pick, Tm, Pos, CarAV)
  nfl$year <- i
  nfl.all <- rbind(nfl.all, nfl)
  print(i)
}
  
nfl.all1 <- nfl.all
nfl.all1 <- filter(nfl.all1, Pos!="Pos", Tm!="")
nfl.all1[nfl.all1$CarAV=="",]$CarAV <- 0
nfl.all1 <- nfl.all1 %>%
  mutate(Rnd = as.numeric(as.character(Rnd)), 
           CarAV = as.numeric(as.character(CarAV)), 
           year = as.numeric(as.character(year)))
nfl.all1 <- nfl.all1 %>%
  group_by(year) %>%
  mutate(pick.number = 1:n())

p <- ggplot(filter(nfl.all1, year <=2010, Rnd <=7), aes(pick.number, CarAV))
p1 <- p + geom_jitter() + geom_smooth(method = "loess", span = 0.5)  
p1 + ggtitle("NFL draft: career value by pick number, 1990-2010") + 
  scale_x_continuous("Pick number") + scale_y_continuous("Career approximate value")+
  theme_bw()


### NBA 

nba.all <- NULL
for (i in year){
  url <- paste0("http://www.basketball-reference.com/draft/NBA_", i, ".htm")
  tables <- readHTMLTable(url)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  nba <- tables[[which.max(n.rows)]]
  nba <- nba[,c("Rk", "Pk", "Tm", "Player", "WS")]
  nba$year <- i
  nba.all <- rbind(nba.all, nba)
  print(i)
}

nba.all1 <- filter(nba.all, Rk!="", Rk!="Rk")
nba.all1 <- nba.all1 %>%
  mutate(WS = as.numeric(as.character(WS)), 
         year = as.numeric(as.character(year)),
         Pk = as.numeric(as.character(Pk)),
         WS = ifelse(is.na(WS), 0, WS))
p <- ggplot(filter(nba.all1, year <=2010), aes(Pk, WS))
p1 <- p + geom_jitter() + geom_smooth(method = "loess", span = 0.4)  
p1 + ggtitle("NBA draft: win shares by pick number, 1990-2010") + 
  scale_x_continuous("Pick number") + scale_y_continuous("Win Shares")+
  theme_bw()





### NHL 

nhl.all <- NULL
for (i in year){
  url <- paste0("http://www.hockey-reference.com/draft/NHL_", i, "_entry.html")
  tables <- readHTMLTable(url)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  nhl <- tables[[which.max(n.rows)]]
  nhl <- nhl[,c("Overall", "Team", "Player", "GP", "PTS")]
  nhl$year <- i
  nhl.all <- rbind(nhl.all, nhl)
  print(i)
}

nhl.all1 <- filter(nhl.all, Team!="", Overall!="Overall")
nhl.all1 <- nhl.all1 %>%
  mutate(PTS = as.numeric(as.character(PTS)), 
         year = as.numeric(as.character(year)),
         Pk = as.numeric(as.character(Overall)),
         GP = as.numeric(as.character(GP)),
         PTS = ifelse(is.na(PTS), 0, PTS),
         GP = ifelse(is.na(GP), 0, GP), 
         Decade = ifelse(year >=2000, "2000s", "1990s"))

p <- ggplot(filter(nhl.all1, year <=2010), aes(Pk, GP))
p1 <- p + geom_point() + geom_smooth(method = "loess", span = 0.4)  
p1 + ggtitle("NHL draft: games played by pick number, 1990-2010") + 
  scale_x_continuous("Pick number") + scale_y_continuous("Games played")+
  theme_bw() 

p <- ggplot(filter(nhl.all1, year <=2005, Pk <=100), aes(Pk, GP))
p1 <- p + geom_point() + geom_smooth(method = "loess", span = 0.4)  
p1 + ggtitle("NHL draft: games played by pick number, 1990-2005") + 
  scale_x_continuous("Pick number") + scale_y_continuous("Games played")+
  theme_bw() 





### MLB 

mlb.all <- NULL
for (i in year){
  for (j in 1:10){
  url <- paste0("http://www.baseball-reference.com/draft/?query_type=year_round&year_ID=",i,"&draft_round=",j,"&draft_type=junreg&")
  tables <- readHTMLTable(url)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  mlb <- tables[[which.max(n.rows)]]
  mlb <- mlb[,c("Rnd", "OvPck", "Tm", "Name","Pos","WAR", "Type")]
  mlb$year <- i
  mlb.all <- rbind(mlb.all, mlb)
  print(i)
}
}

## This code will allow you to compare pitchers and non-pitchers, or HS vs. college players

pitchers <- c("LHP", "RHP", "P", "lHP", "RHp")
mlb.all1 <- mlb.all %>%
  mutate(WAR = as.numeric(as.character(WAR)), 
         year = as.numeric(as.character(year)),
         Pk = as.numeric(as.character(OvPck)),
         WAR = ifelse(is.na(WAR), 0, WAR), 
         Position = ifelse(Pos %in% pitchers, "Pitcher", "non-Pitcher"), 
         Year.cat = cut(year, c(1989, 1995, 2000, 2005, 2010)))


## lots of players drafted twice
mlb.all2 <- mlb.all1 %>% 
  arrange(Name, year) %>%
  group_by(Name) %>% 
  summarise(max.year = max(year)) %>%
  right_join(mlb.all1) %>%
  mutate(WAR = ifelse(year==max.year, WAR, 0))

#mlb.all2 <- filter(mlb.all2, year <=2010, Type!="", Type!="JC")
#gg_animate(p1, "mlbdraft.gif")

p <- ggplot(mlb.all2, aes(Pk, WAR, frame = year))
p1 <- p  + geom_smooth(aes(group = year), method = "loess", span = 0.4) + 
  geom_point() + 
  ggtitle("MLB draft: WAR by pick number, ") + 
  scale_x_continuous("Pick number") + scale_y_continuous("WAR")+
  theme_bw() + facet_grid(~Position)




### Merge & graph together

nba.all2 <- nba.all1 %>%
  rename(Outcome = WS) %>%
  select(Pk, Outcome, year) %>%
  mutate(Sport = "NBA")

nfl.all2 <- nfl.all1 %>%
  rename(Outcome = CarAV, Pk = Pick) %>%
  select(Pk, Outcome, year)%>%
  mutate(Sport = "NFL") %>%
  filter(as.numeric(as.character(Pk)) < 224)

nhl.all2 <- nhl.all1 %>%
  rename(Outcome = GP) %>%
  select(Pk, Outcome, year)%>%
  mutate(Sport = "NHL") %>%
  filter(as.numeric(as.character(Pk)) < 211)


mlb.all2 <- mlb.all2 %>%
  rename(Outcome = WAR) %>%
  select(Pk, Outcome, year)%>%
  mutate(Sport = "MLB") %>%
  filter(Pk < 105)

############################################################
######### First chart: only using rounds one and two
############################################################


all.sports <- rbind(mlb.all2, nhl.all2, nfl.all2, nba.all2)
all.sports$Pk <- as.numeric(all.sports$Pk)

all.sports <- filter(all.sports, Pk <=60)
std <- all.sports %>%  
  filter(Pk >= 55) %>%
  group_by(Sport) %>%
  summarise(Baseline = mean(Outcome))

all.sports <- left_join(all.sports, std)  %>%
  mutate(Outcome.std = Outcome/Baseline)

p <- ggplot(filter(all.sports, year <=2010), 
            aes(Pk, Outcome.std, group = Sport, colour = Sport, fill = Sport))
p1 <- p + geom_smooth(method = "loess", span = 0.4)  
p1 + ggtitle("Relative value of the top 60 picks") + 
  scale_x_continuous("Pick number", breaks = c(1, 15, 30, 45, 60)) + ylab("") +
  theme_bw(12) 



############################################################
######### Second curve: only using pick percentile
############################################################


all.sports <- rbind(mlb.all2, nhl.all2, nfl.all2, nba.all2)
all.sports$Pk <- as.numeric(all.sports$Pk)

limit.year <- all.sports %>%
  group_by(Sport, year) %>%
  summarise(min.year = max(Pk))

all.sports <- left_join(all.sports, limit.year)

std <- all.sports  %>%
  filter(Pk >= (min.year - 5) ) %>%
  group_by(Sport) %>%
  summarise(Baseline = mean(Outcome)) %>%
  mutate(Baseline = ifelse(Sport=="MLB", 1, Baseline))


all.sports <- left_join(all.sports, std)  %>%
  mutate(Outcome.std = Outcome/Baseline, Pk.Percent = Pk/min.year*100) %>%
  filter(!Sport=="MLB")

p <- ggplot(filter(all.sports, year <= 2005), 
            aes(Pk.Percent, Outcome.std, group = Sport, colour = Sport, fill = Sport))
p1 <- p + geom_smooth(method = "loess", span = 0.4)  
p1 + ggtitle("Relative value by pick percentile") + 
  scale_x_continuous("Pick percentile") + ylab("") +
  theme_bw(14) 



