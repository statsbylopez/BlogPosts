rm(list = ls())
library(XML)
library(dplyr)
library(ggplot2)
library(forcats)


#### First part: collect the data

year <- 1968:2016


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
         year = as.numeric(as.character(year)), 
         Pos1 = fct_recode(Pos, "Oline" = "C", "Dline" = "DE", "Dline" = "DT", "WR" = "E",
                           "RB" = "FB", "WR" = "FL", "Oline" = "G", "RB"= "HB", 
                           "K/P" = "K", "K/P" = "P", "Oline" = "T", "RB" = "WB", 
                           "Dline" = "NT", "K/P" = "KR", "Dline" = "DL","Oline" = "OL",
                           "DB" = "CB", "DB" = "FS", "LB" = "ILB", "LB" = "OLB", "DB" = "S",
                           "DB" = "SS","K/P" = "LS"))

nfl.all1 <- nfl.all1 %>%
  group_by(year) %>%
  mutate(pick.number = 1:n())

nfl.all2 <- nfl.all1[nfl.all1$Pos1 != "K/P",]

rank.cor <- nfl.all2 %>% 
  filter(Rnd <= 6) %>%
  group_by(year, Rnd) %>%
  summarise(n.players = n(), efficiency = cor(CarAV, -pick.number, method = "spearman"))
p <- ggplot(rank.cor, aes(year, efficiency))
p + geom_point() + geom_smooth(method = "loess") +theme_bw(12)+
  labs(title = "NFL draft efficiency by round", subtitle = "Rank correlation between draft position and performance") +  scale_x_continuous("Year") + 
  scale_y_continuous("Efficiency") + facet_wrap(~Rnd, scales = "free_x", nrow = 2)

rank.cor <- nfl.all2 %>% 
  group_by(year, Pos1) %>%
  summarise(n.players = n(), efficiency = cor(CarAV, -pick.number, method = "spearman"))
p <- ggplot(rank.cor, aes(year, efficiency))
p + geom_point() + geom_smooth(method = "loess") +theme_bw(12)+
  labs(title = "NFL draft efficiency by position", subtitle = "Rank correlation between draft position and performance") +  scale_x_continuous("Year") + 
  scale_y_continuous("Efficiency") + facet_wrap(~Pos1, scales = "free_x", nrow = 2)



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


### NHL 

nhl.all <- NULL
for (i in 1979:2016){
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
         GP = ifelse(is.na(GP), 0, GP))


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
         Position = ifelse(Pos %in% pitchers, "Pitcher", "non-Pitcher"))


## lots of players drafted twice
mlb.all2 <- mlb.all1 %>% 
  arrange(Name, year) %>%
  group_by(Name) %>% 
  summarise(max.year = max(year)) %>%
  right_join(mlb.all1) %>%
  mutate(WAR = ifelse(year==max.year, WAR, 0))



### Merge & graph together
nba.teams <- nba.all1 %>%
  group_by(year) %>%
  summarise(n.teams = length(unique(Tm)))

nhl.teams <- nhl.all1 %>%
  group_by(year) %>%
  summarise(n.teams = length(unique(Team)))

nba.all2 <- nba.all1 %>%
  left_join(nba.teams) %>%
  mutate(Rnd = floor(Pk/n.teams) + 1) %>%
  rename(Outcome = WS) %>%
  select(Pk, Rnd, Outcome, year) %>%
  mutate(Sport = "NBA")

nfl.all2 <- nfl.all1 %>%
  rename(Outcome = CarAV, Pk = Pick) %>%
  select(Pk, Rnd, Outcome, year)%>%
  mutate(Sport = "NFL") %>%
  filter(as.numeric(as.character(Pk)) < 224) %>%
  mutate(Pk = as.numeric(as.character(Pk)))

nhl.all2 <- nhl.all1 %>%
  left_join(nhl.teams) %>%
  mutate(Rnd = floor(Pk/n.teams) + 1) %>%
  rename(Outcome = GP) %>%
  select(Pk, Rnd, Outcome, year)%>%
  mutate(Sport = "NHL") %>%
  filter(as.numeric(as.character(Pk)) < 211)


mlb.all2 <- mlb.all2 %>%
  rename(Outcome = WAR) %>%
  select(Pk, Rnd, Outcome, year, Rnd)%>%
  mutate(Sport = "MLB") %>%
  mutate(Rnd = as.numeric(as.character(Rnd)))

############################################################
######### Compare efficiency
############################################################


all.sports <- bind_rows(mlb.all2, nhl.all2, nfl.all2, nba.all2)
#write.csv(all.sports, "allsports_draft_422.csv", row.names = FALSE)

all.sports <- read.csv("allsports_draft_422.csv")
## Nba draft: 2 rounds beginning in 1989
## NHL roughly the same beginning 1980
## NFL draft: same from 1994 onwards: included 8 or 9 rounds in years prior

rank.cor <- all.sports %>% 
  filter((Pk <= 60 & Sport!="NFL") | Pk <= 64) %>%
  group_by(year, Sport) %>%
  summarise(efficiency = cor(Outcome, -Pk, method = "spearman"))

p <- ggplot(rank.cor, aes(year, efficiency))
p + geom_point() + geom_smooth(method = "loess") + facet_grid(~Sport)+theme_bw(12)+
  labs(title = "Draft efficiency by league, picks 1-60", subtitle = "Rank correlation between draft position and performance") +  scale_x_continuous("Year") + 
  scale_y_continuous("Efficiency") 

### Note: similar chart for round 1, although rounds may not be accurate for NHL and NBA given varying teams over time.

rank.cor <- all.sports %>% 
  filter(Pk <=60, Rnd <=2) %>%
  group_by(year, Sport, Rnd) %>%
  summarise(efficiency = cor(Outcome, -Pk, method = "spearman"))

p <- ggplot(rank.cor, aes(year, efficiency))
p + geom_point() + geom_smooth(method = "loess") + facet_grid(~Sport + Rnd)+theme_bw(12)+
  ggtitle("Draft efficiency by year, picks 1-60") +  scale_x_continuous("Year")  
  scale_y_continuous("Efficiency") 





######################## 
### Future ideas: adjust value to account for draft odds
### Identify the lottery odds that would allow the NBA to match the NFLs curve
########################

all.sports1 <- all.sports %>% filter(Pk >= 55, Pk <= 60) %>% group_by(Sport) %>% summarise(baseline = mean(Outcome))
all.sports <- all.sports %>% left_join(all.sports1) %>%
  mutate(Outcome.rel = Outcome/baseline)
  
  
span <- 0.3
nba.fit <- loess(Outcome.rel ~ Pk, data = filter(all.sports, Pk <= 60, Sport == "NBA"), span = span)
nba.predict <- data.frame(Pk = 1:60)
nba.predict$val.hat <- predict(nba.fit, nba.predict$Pk)
nba.predict$SE <- predict(nba.fit, nba.predict$Pk, se = TRUE)$se.fit

nba.probs <- matrix(nrow = 14, ncol = 14, 0)
nba.probs[1,1:4] <- c(.250,	.215,	.178,	.357)										
nba.probs[2,1:5] <- c(.199,	.188,	.171,	.319,	.123)									
nba.probs[3,1:6] <- c(.156,	.157,	.156,	.226,	.265,	.040)								
nba.probs[4,1:7] <- c(.119,	.126,	.133,	.099,	.351,	.160,	.012)							
nba.probs[5,1:8] <- c(.088,	.097,	.107,	0, .261,	.360,	.084,	.004)						
nba.probs[6,1:9] <- c(.063,	.071,	.081,	0, 0, .439,	.305,	.040,	.001)					
nba.probs[7,1:10] <- c(.043, .049, .058,	0, 0, 0, .599, .232, .018, .000)				
nba.probs[8,1:11] <- c(.028, .033, .039,	0, 0, 0, 0,  .724, .168, .008, .000)			
nba.probs[9,1:12] <- c(.017, .020, .024,	0, 0, 0, 0, 0, .813,	.122,	.004,	.000)		
nba.probs[10,1:13] <- c(.011, .013, .016,	0, 0, 0, 0, 0, 0,  .870,	.089,	.002,	.000)	
nba.probs[11,1:14] <- c(.008, .009, .012,	0, 0, 0, 0, 0, 0, 0,  .907,	.063,	.001,	.000)
nba.probs[12,1:14] <- c(.007, .008, .010,	0, 0, 0, 0, 0, 0, 0, 0, .935,	.039,	.000)
nba.probs[13,1:14] <- c(.006,	.007,	.009,	0,0, 0, 0, 0, 0, 0, 0, 0, .960,	.018)
nba.probs[14,1:14] <- c(.005,	.006,	.007,	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .982)

exp.val <- matrix(nba.predict$val.hat[1:14])
se.exp.val <- matrix(nba.predict$SE[1:14])
nba.predict$val.hat.adj <- ifelse(nba.predict$Pk < 15, nba.probs %*% exp.val, nba.predict$val.hat)


nfl.fit <- loess(Outcome.rel ~ Pk, data = filter(all.sports, Pk <= 60, Sport == "NFL"), span = span)
nba.predict$val.hat.nfl <- predict(nfl.fit, nba.predict$Pk)



p <- ggplot(filter(all.sports, Sport == "NFL", year > 1990), aes(Pk, Outcome.rel))
p +   geom_point() + 
  geom_smooth(method = "loess", span = span) + xlim(0, 60) + 
scale_y_continuous("Efficiency") 

p <- ggplot(nba.predict, aes(Pk, val.hat)) + 
  geom_line() + 
  geom_point()+ 
  geom_line(data = nba.predict, aes(Pk, val.hat.adj), colour = "red", lty = 2) + 
  geom_point(data = nba.predict, aes(Pk, val.hat.adj), colour = "red") + 
  #geom_line(data = nba.predict, aes(Pk, val.hat.nfl), colour = "blue", lty = 2) + 
  #geom_point(data = nba.predict, aes(Pk, val.hat.nfl), colour = "blue") + 
  xlim(0, 22) + 
  annotate("text", x = 1.5, y = 14, label = "Lottery Adjusted", colour = "red") + 
  annotate("text", x = 1.5, y = 20.5, label = "Original", colour = "black") + 
  xlab ("Finishing record") + ylab("Value") + labs(title = "Finishing spot value, original versus lottery adjusted")
p



b <- nba.predict$val.hat.nfl[1:14]
A <- diag(nrow = 14, ncol = 14, nba.predict$val.hat[1:14])
solve(A, b)




