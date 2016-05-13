library(devtools)
library(engsoccerdata)
library(mosaic)
library(BradleyTerry2)
data(package="engsoccerdata") 


epl.df <- engsoccerdata2 %>%
  filter(division==1)%>% 
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result) 

epl.df2 <- read.csv("http://www.football-data.co.uk/mmz4281/1516/E0.csv")
epl.df2 <- epl.df2 %>%
  mutate(result =  ifelse(FTHG > FTAG, 1, ifelse(FTHG < FTAG, 0, 0.5))) %>%
  select(HomeTeam, AwayTeam, result)  %>%
  mutate(Season = 2015)
colnames(epl.df2)[1:2]<-c("home", "visitor")

epl.data<-rbind(epl.df, epl.df2)

BTL.estimate <-NULL
BTL.se <- NULL
year.prop.win <- NULL
year.prop.draw <- NULL
year.prop.loss <- NULL
loop <- max(epl.data$Season) - min(epl.data$Season)

for (i in 1:(loop+1)){
temp.epl <- filter(epl.data, Season==i+min(epl.data$Season)-1)  
year.prop.win[i] <- tally(~ result, data= temp.epl)[3]/nrow(temp.epl)
year.prop.draw[i] <- tally(~ result, data= temp.epl)[2]/nrow(temp.epl)
year.prop.loss[i] <- tally(~ result, data= temp.epl)[1]/nrow(temp.epl)
if(nrow(temp.epl)>0)
  {homeBT <- BTm(result,
              data.frame(team = home, home.adv = 1),
              data.frame(team = visitor, home.adv = 0),
              ~ team + home.adv,
              id = "team", data = temp.epl)
  coeff <- msummary(homeBT)$coeff
BTL.estimate[i] <- coeff[nrow(coeff),1]
BTL.se[i] <- coeff[nrow(coeff),2]}
else{BTL.estimate[i] <- NA; BTL.se[i]<-NA }
print(i)
}


df <- data.frame(BTL.estimate, BTL.se, year.prop.win, year.prop.draw, year.prop.loss, Season = 
                   min(epl.data$Season):max(epl.data$Season))

p <- ggplot(df, aes(x = Season, y = year.prop.win)) + 
  geom_point(colour = 1) + 
  geom_smooth(colour = 1, span = 0.5, alpha = 0.2) +
  theme_bw() + #geom_vline(xintercept = 1981)+ 
  geom_point(aes(x = Season, y = year.prop.draw), colour = 2) + 
  geom_smooth(aes(x = Season, y = year.prop.draw), colour = 2) +
  geom_point(aes(x = Season, y = year.prop.loss), colour = 3) + 
  geom_smooth(aes(x = Season, y = year.prop.loss), colour = 3) +
  scale_y_continuous("",breaks = c(0, 0.25, 0.5, 0.75), lim = c(0, 0.75), 
                     labels = c("0%", "25%", "50%", "75%"))+
  ggtitle("EPL results by year")+
  annotate(x = 1960, y = 0.65, "text", label = "home win %", size = 4, colour = 1)+
  annotate(x = 1960, y = 0.35, "text", label = "draw %", size = 4, colour = 2)+
  annotate(x = 1960, y = 0.15, "text", label = "away win %", size = 4, colour = 3)
p




ggplot(df, aes(x = Season, y = exp(BTL.estimate))) + 
  geom_point(data = filter(df, Season<2015),  aes(x = Season, y = exp(BTL.estimate))) + 
  geom_smooth(colour = 1, span = 0.5, alpha = 0.2) +
  theme_bw()  +
  scale_y_continuous("", lim = c(exp(0), exp(1.1)), 
                     breaks = c(1, 1.5, 2, 2.5, 3), 
                     labels = c("Even", "50% higher", 
                      "100% higher", "150% higher", "200% higher")) +
  ggtitle("Relative odds of a home win in the EPL")+
  geom_point(data = filter(df, Season==2015), col = "red") + 
  annotate(x = 2015, y = 1.16, "text", label = "2015", size = 3, colour = "red")

df.EPL <- df
df.EPL$League <- "EPL"



#### Get information from Bundesliga
### Bundesliga
####

epl.df <- bundesliga %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result) 
epl.data<-epl.df

BTL.estimate <-NULL
BTL.se <- NULL
year.prop.win <- NULL
year.prop.draw <- NULL
year.prop.loss <- NULL

loop <- max(epl.data$Season) - min(epl.data$Season)
for (i in 1:(loop+1)){
  temp.epl <- filter(epl.data, Season==i+min(epl.data$Season)-1)    
  year.prop.win[i] <- tally(~ result, data= temp.epl)[3]/nrow(temp.epl)
  year.prop.draw[i] <- tally(~ result, data= temp.epl)[2]/nrow(temp.epl)
  year.prop.loss[i] <- tally(~ result, data= temp.epl)[1]/nrow(temp.epl)
  if(nrow(temp.epl)>0)
  {homeBT <- BTm(result,
                 data.frame(team = home, home.adv = 1),
                 data.frame(team = visitor, home.adv = 0),
                 ~ team + home.adv,
                 id = "team", data = temp.epl)
  coeff <- msummary(homeBT)$coeff
  BTL.estimate[i] <- coeff[nrow(coeff),1]
  BTL.se[i] <- coeff[nrow(coeff),2]}
  else{BTL.estimate[i] <- NA; BTL.se[i]<-NA }
  print(i)
}
df.Bundesliga<- data.frame(BTL.estimate, BTL.se, year.prop.win, year.prop.draw, year.prop.loss, 
                Season = min(epl.data$Season):max(epl.data$Season), League = "Bundesliga")





#### Get information from Italy
### italycalcio
####

epl.df <- italycalcio %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result) 
epl.data<-epl.df

BTL.estimate <-NULL
BTL.se <- NULL
year.prop.win <- NULL
year.prop.draw <- NULL
year.prop.loss <- NULL

loop <- max(epl.data$Season) - min(epl.data$Season)
for (i in 1:(loop+1)){
  temp.epl <- filter(epl.data, Season==i+min(epl.data$Season)-1)    
  year.prop.win[i] <- tally(~ result, data= temp.epl)[3]/nrow(temp.epl)
  year.prop.draw[i] <- tally(~ result, data= temp.epl)[2]/nrow(temp.epl)
  year.prop.loss[i] <- tally(~ result, data= temp.epl)[1]/nrow(temp.epl)
  if(nrow(temp.epl)>0&i!=19)
  {homeBT <- BTm(result,
                 data.frame(team = home, home.adv = 1),
                 data.frame(team = visitor, home.adv = 0),
                 ~ team + home.adv,
                 id = "team", data = temp.epl)
  coeff <- msummary(homeBT)$coeff
  BTL.estimate[i] <- coeff[nrow(coeff),1]
  BTL.se[i] <- coeff[nrow(coeff),2]}
  else{BTL.estimate[i] <- NA; BTL.se[i]<-NA }
  print(i)
}
df.Italy<- data.frame(BTL.estimate, BTL.se, year.prop.win, year.prop.draw, year.prop.loss, 
                           Season = min(epl.data$Season):max(epl.data$Season), League = "Serie A")






#### Get information from Spain
### spainliga
####

epl.df <- spainliga %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)))%>%
  select(home, visitor, Season, result) 
epl.data<-epl.df

BTL.estimate <-NULL
BTL.se <- NULL
year.prop.win <- NULL
year.prop.draw <- NULL
year.prop.loss <- NULL

loop <- max(epl.data$Season) - min(epl.data$Season)
for (i in 1:(loop+1)){
  temp.epl <- filter(epl.data, Season==i+min(epl.data$Season)-1)    
  year.prop.win[i] <- tally(~ result, data= temp.epl)[3]/nrow(temp.epl)
  year.prop.draw[i] <- tally(~ result, data= temp.epl)[2]/nrow(temp.epl)
  year.prop.loss[i] <- tally(~ result, data= temp.epl)[1]/nrow(temp.epl)
  if(nrow(temp.epl)>0)
  {homeBT <- BTm(result,
                 data.frame(team = home, home.adv = 1),
                 data.frame(team = visitor, home.adv = 0),
                 ~ team + home.adv,
                 id = "team", data = temp.epl)
  coeff <- msummary(homeBT)$coeff
  BTL.estimate[i] <- coeff[nrow(coeff),1]
  BTL.se[i] <- coeff[nrow(coeff),2]}
  else{BTL.estimate[i] <- NA; BTL.se[i]<-NA }
  print(i)
}
df.Spain<- data.frame(BTL.estimate, BTL.se, year.prop.win, year.prop.draw, year.prop.loss, 
                      Season = min(epl.data$Season):max(epl.data$Season), League = "Liga")





df.all <- rbind(df.Bundesliga, df.EPL, df.Italy, df.Spain)

ggplot(df.all, aes(x = Season, y = year.prop.win, colour = League))+ 
  geom_point() + geom_smooth(span = 0.5, alpha = 0.2)+
  theme_bw() +
  scale_y_continuous("",breaks = c(0.4, 0.5, 0.6, 0.7), lim = c(0.4, 0.72), 
                     labels = c("40%", "50%", "60%", "70%"))+
  ggtitle("Home team win percentage by year") +scale_x_continuous(lim = c(1930, 2015))



ggplot(df.all, aes(x = Season, y = exp(BTL.estimate), colour = League))+ 
  geom_point() + geom_smooth(span = 0.5, alpha = 0.2) +
  theme_bw()  +
  scale_y_continuous("", lim = c(exp(0), exp(1.1)), 
                     breaks = c(1, 1.5, 2, 2.5, 3), 
                     labels = c("Even", "50% higher", 
                                "100% higher", "150% higher", "200% higher")) +
  ggtitle("Relative odds of a home win")+scale_x_continuous(lim = c(1930, 2015))



