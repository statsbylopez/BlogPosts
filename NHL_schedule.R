library(XML)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(teamcolors)
library(mosaic)
set.seed(100)
nsim <- 1000
### Note: I used more sim's for part II

########################
### Joy Division plot
########################

#### Initial wrangling - team colors
#### Divisional facet
div <- c("Metro", "Atlantic", "Central", "Pacific")
div.teams.NHL <- c(div[4], div[4], div[2], div[2], div[4], 
                   div[1], div[3], div[3], div[1], div[3],
                   div[2], div[4], div[2], div[4], div[3], 
                   div[2], div[3], div[1], div[1], div[1],
                   div[2], div[1], div[1], div[4], div[3], 
                   div[2], div[2], div[4], div[1], div[3])
divisions_nhl <- data_frame(sport = "nhl", division = div.teams.NHL)

divisions_nhl$team <- teamcolors %>%
  filter(sport == "nhl") %>%
  select(name) %>%
  unlist()

teamcolors1 <- teamcolors %>%
  filter(sport == "nhl") %>%
  left_join(divisions_nhl, by = c("sport" = "sport", "name" = "team")) %>%
  mutate(name = ifelse(name == "St Louis Blues", "St  Louis Blues", name)) 
teamcolors1 %>% head()


nhl<-NULL
urls<- NULL
for (i in c(2017)){
  url<-paste("http://www.hockey-reference.com/leagues/NHL_",i,"_games.html",sep="")
  urls <- c(urls, url)
}

for (i in 1:length(urls)){
  tables <- readHTMLTable(urls[i])
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  temp<-tables[[which.max(n.rows)]]
  temp<- temp[,1:6]
  temp$season<-i+2004
  nhl<-rbind(nhl,temp)
  print(i)
}

names(nhl)<-c("Date","Visitor","VisGoals","Home","HomeGoals","OTCat","Year")
nhl[nhl$Year>2004,]$Year <- nhl[nhl$Year>2004,]$Year + 1
table(nhl$OTCat)
nhl<-nhl[nhl$HomeGoals!="",]
nhl$OT<-nhl$OTCat=="OT"|nhl$OTCat=="SO"

nhl$home.win <- as.numeric(as.character(nhl$HomeGoals)) > as.numeric(as.character(nhl$VisGoals))
nhl$vis.win <- as.numeric(as.character(nhl$VisGoals)) > as.numeric(as.character(nhl$HomeGoals))
nhl <- nhl %>%
  mutate(result =  ifelse(as.numeric(as.character(HomeGoals)) > 
                            as.numeric(as.character(VisGoals)), 1, 0), 
         ptsH = ifelse(home.win == 1, 2, ifelse(OT, 1, 0)), 
         ptsV = ifelse(vis.win == 1, 2, ifelse(OT, 1, 0))) 


library(BradleyTerry2)
homeBT <- BTm(result,
              data.frame(team = Home, home.adv = 1),
              data.frame(team = Visitor, home.adv = 0),
              ~ team + home.adv,
              id = "team", data = nhl)


#### Model matrix for predictions
betas <- matrix(c(0,msummary(homeBT)$coeff[,1]))
Teams <- sort(as.character(unique(c(as.character(nhl$Home),
                                    as.character(nhl$Visitor)))))
x <- matrix(0, nrow = nrow(nhl), ncol = length(Teams) + 1)
for (i in 1:nrow(nhl)) {
  x[i, which(as.character(nhl[i,"Home"]) == Teams)] <- (1)
  x[i, which(as.character(nhl[i,"Visitor"]) == Teams)] <- (-1)
} 
x[,31] <- 1

nhl$predict.home <- exp(x %*% betas)/(1 + exp(x %*% betas))
nhl$predict.vis <- 1 - nhl$predict.home
pts <- NULL
for (i in 1:nsim){
  nhl$sim.win <- rbinom(nrow(nhl), 1, nhl$predict.home)
  nhl$sim.ot <- rbinom(nrow(nhl), 1, rep(0.25, nrow(nhl)))
  nhl$sim.ptsH <- ifelse(nhl$sim.win == 1, 2, ifelse(nhl$sim.ot == 1, 1, 0))
  nhl$sim.ptsV <- ifelse(nhl$sim.win == 0, 2, ifelse(nhl$sim.ot == 1, 1, 0))
  home.sim <- nhl %>% select(Home, sim.ptsH) %>% rename(Team = Home, Pts = sim.ptsH)
  vis.sim <- nhl %>% select(Visitor, sim.ptsV) %>% rename(Team = Visitor, Pts = sim.ptsV)
  all.sim <- bind_rows(home.sim, vis.sim)
  pts.sim <- all.sim %>% group_by(Team) %>% summarise(n.games = n(), 
                                                      sim.pts = sum(Pts)*82/n.games)
  pts <- rbind(pts, t(pts.sim$sim.pts))
}

colnames(pts) <- Teams
pts <- data.frame(pts)
all.teams <- gather(pts, Team, wins, Anaheim.Ducks:Winnipeg.Jets, factor_key=FALSE)%>%
  mutate(Schedule = "Unbalanced/current")


df <- all.teams %>%
  mutate(GroupNum = rev(as.numeric(as.factor(Team)))) %>% #rev() means the ordering will be from top to bottom
  group_by(Team, GroupNum) %>% 
  do(tidy(density(.$wins, bw = diff(range(.$wins))/20))) %>% #The original has quite a large bandwidth
  group_by() %>% 
  filter(x > 45, x < 130) %>% 
  mutate(ymin = GroupNum * (max(y) / 1.5), #This constant controls how much overlap between groups there is
         ymax = y + ymin,
         ylabel = ymin + min(ymin)/2,
         xlabel = -15) #This constant controls how far to the left the labels are

#Get quartiles

labels <- all.teams %>% 
  mutate(GroupNum = rev(as.numeric(as.factor(Team)))) %>% 
  group_by(Team, GroupNum) %>% 
  mutate(q1 = quantile(wins)[2],
         median = quantile(wins)[3],
         q3 = quantile(wins)[4]) %>%
  filter(row_number() == 1) %>% 
  select(-wins) %>% 
  left_join(df) %>% 
  mutate(xmed = x[which.min(abs(x - median))],
         yminmed = ymin[which.min(abs(x - median))],
         ymaxmed = ymax[which.min(abs(x - median))]) %>% 
  filter(row_number() == 1)

labels$Team <- gsub("\\.", " ", labels$Team)

p <- ggplot(df, aes(x, ymin = ymin, ymax = ymax)) + 
  geom_text(data = labels, aes(xlabel, ylabel, label = Team), hjust = 0) +
  geom_vline(xintercept = c(25, 50, 75, 100, 125, 150), 
             size = 0.75, alpha = 0.5, colour = "#626262") + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#F0F0F0"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        title = element_text(size=15), 
        axis.text.x =element_text(size=12))


for (i in unique(df$GroupNum)) {
  p <- p + geom_ribbon(data = df[df$GroupNum == i,], aes(group = GroupNum), colour = "#F0F0F0", fill = teamcolors1$primary[31-i]) +
    geom_segment(data = labels[labels$GroupNum == i,], aes(x = xmed, xend = xmed, y = yminmed, yend = ymaxmed), colour = "#F0F0F0", linetype = "dashed") +
    geom_segment(data = labels[labels$GroupNum == i,], x = min(df$x), xend = max(df$x), aes(y = ymin, yend = ymin), size = 1.5, lineend = "round") 
}
p <- p + geom_text(data = labels[labels$Team == "Anaheim Ducks",], 
                   aes(xmed - xlabel/50, ylabel), 
                   label = "Median", colour = "#F0F0F0", 
                   hjust = 0, fontface = "italic", size = 4)  
p <- p + labs(title = "Replaying the 2016-17 NHL season", 
         subtitle = "Simulated number of points in 10,000 seasons") + 
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150))
p

ggsave("~/Dropbox/BlogPosts/BlogPosts/figure/NHLjoydivision.pdf", p, height = 8, width = 8)



########################
### Schedule differences
########################



nhl.balance <- nhl
pts <- NULL
for (j in 1:nsim){
  
x <- matrix(0, nrow = nrow(nhl.balance), ncol = length(Teams) + 1)
for (i in 1:nrow(nhl.balance)) {
  x[i, which(as.character(nhl.balance[i,"Home"]) == Teams)] <- (1)
  nhl.balance[i,"Visitor"] <- sample(Teams[Teams!=Teams[which(as.character(nhl.balance[i, "Home"]) == Teams)]], 1)
  x[i, which(as.character(nhl.balance[i,"Visitor"]) == Teams)] <- -(1)
} 
x[,31] <- 1

nhl.balance$predict.home <- exp(x %*% betas)/(1 + exp(x %*% betas))
nhl.balance$predict.vis <- 1 - nhl.balance$predict.home

nhl.balance$sim.win <- rbinom(nrow(nhl.balance), 1, nhl.balance$predict.home)
nhl.balance$sim.ot <- rbinom(nrow(nhl.balance), 1, rep(0.25, nrow(nhl.balance)))
nhl.balance$sim.ptsH <- ifelse(nhl.balance$sim.win == 1, 2, ifelse(nhl.balance$sim.ot == 1, 1, 0))
nhl.balance$sim.ptsV <- ifelse(nhl.balance$sim.win == 0, 2, ifelse(nhl.balance$sim.ot == 1, 1, 0))
home.sim <- nhl.balance %>% select(Home, sim.ptsH) %>% rename(Team = Home, Pts = sim.ptsH)
vis.sim <- nhl.balance %>% select(Visitor, sim.ptsV) %>% rename(Team = Visitor, Pts = sim.ptsV)
all.sim <- bind_rows(home.sim, vis.sim)
pts.sim <- all.sim %>% group_by(Team) %>% summarise(n.games = n(), sim.pts = sum(Pts)*82/n.games)
pts <- rbind(pts, t(pts.sim$sim.pts))
print(j)
}

colnames(pts) <- Teams
pts <- data.frame(pts)
all.teams1 <- gather(pts, Team, wins, Anaheim.Ducks:Winnipeg.Jets, factor_key=FALSE) %>%
  mutate(Schedule = "Balanced")
all.teams$Team <- gsub("\\.", " ", all.teams$Team)
all.teams1$Team <- gsub("\\.", " ", all.teams1$Team)

home.real <- nhl %>% select(Home, ptsH) %>% rename(Team = Home, Pts = ptsH)
vis.real <- nhl %>% select(Visitor, ptsV) %>% rename(Team = Visitor, Pts = ptsV)
all.real <- bind_rows(home.real, vis.real)
pts.real <- all.sim %>% group_by(Team) %>% summarise(n.games = n(), 
                                                    wins = sum(Pts)*82/n.games) %>%
  mutate(Schedule = "Observed") %>% select()
                                                    


all.both <- bind_rows(all.teams, all.teams1)
all.both.sum <- all.both %>% group_by(Team, Schedule) %>% 
  summarise(mean.wins = mean(wins))

### Add in team colors

all.both.sum <- left_join(all.both.sum, teamcolors1, by = c("Team" = "name"))

benefit <- all.both.sum %>% group_by(Team) %>% mutate(sched.benefit = mean.wins - lag(mean.wins))  %>%
  select(Team, Schedule, sched.benefit, division, sport, primary) %>% na.omit()

p2 <- ggplot(benefit, aes(x = reorder(Team, sched.benefit), sched.benefit, fill = Team)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  labs(title = "Points added due to the unbalanced schedule, 2016-17 season", 
       subtitle = "Differences shown relative to a balanced schedule") + 
  xlab("") + ylab("Net benefit/loss in points")  + 
  scale_fill_manual(name = NULL, values = benefit$primary) + 
  facet_wrap(~division, scales = "free_x") +
  guides(color = FALSE, fill = FALSE) 
p2

ggsave("~/Dropbox/BlogPosts/BlogPosts/figure/NHL_diff3.pdf", p2, height = 10, width = 8)

## Calgary - 5 times against Vancouver
## LA - 5 times against Coyotes
## Devils: Rangers and Penguins five times
## 
temp <- all.both
all.both1 <- all.both %>% 
  left_join(teamcolors1, by = c("Team" = "name")) %>%
  mutate(Eastern = (division == "Atlantic" | division == "Metro")) %>%
  group_by(Schedule, Team) %>%
  mutate(iteration = 1:n()) %>% 
  ungroup() 
all.both2 <- all.both1 %>%
  group_by(iteration, Schedule, Eastern) %>% 
  arrange(iteration, Schedule, Eastern, -wins) %>% 
  mutate(rank = 1:n(), playoffs = rank <= 8)

all.both3 <- all.both2 %>%
  ungroup() %>%
  group_by(Team, Schedule) %>%
  summarise(mean.playoffs = sum(playoffs == TRUE)/n())

all.both4 <- spread(all.both3, Schedule, mean.playoffs) %>%
  rename(Current = `Unbalanced/current`)

all.both4




