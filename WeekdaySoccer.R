## packages to load
library(devtools)
library(engsoccerdata)
library(mosaic)
library(BradleyTerry2)
library(lubridate)
data(package="engsoccerdata") 


bundes <- bundesliga %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)), 
         dow = wday(date, label = TRUE), weekend = (dow=="Sat"|dow=="Sun"|dow=="Fri"))%>%
  select(date, home, visitor, Season, result, dow, weekend) 

weekend <- bundes %>% 
  filter(weekend, Season!=2009) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

weekday <- bundes %>% 
  filter(!weekend) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

diff.pts <- (weekend$h.pt.ave - weekday$h.pt.ave) - 
  (weekend$a.pt.ave - weekday$a.pt.ave) 

diff.pts <- data.frame(seas = c(1963:2008, 2010:2013), diff = diff.pts)


ggplot(diff.pts, aes(seas, diff)) + 
  geom_point() + geom_smooth() +theme_bw() + xlab("Season") + ylab("Difference") +
  geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) + 
  ggtitle("Home weekend advantage in the Bundesliga")



## Check the EPL

epl <- engsoccerdata2 %>%
  filter(division==1) %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)), 
         dow = wday(Date, label = TRUE), weekend = (dow=="Sat"|dow=="Sun"|dow=="Fri")) %>%
  select(Date, home, visitor, Season, result, dow, weekend) 


weekend <- epl %>% 
  filter(weekend, Season >=1960) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

weekday <- epl %>% 
  filter(!weekend, Season >=1960) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

diff.pts <- (weekend$h.pt.ave - weekday$h.pt.ave) - 
  (weekend$a.pt.ave - weekday$a.pt.ave) 

diff.pts <- data.frame(seas = 1960:2014, diff = diff.pts)

ggplot(diff.pts, aes(seas, diff)) + 
  geom_point() + geom_smooth() +theme_bw() + xlab("Season") + ylab("Difference") +
  geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) + 
  ggtitle("Home weekend advantage in the EPL")







italy <- italycalcio %>%
  filter(tier==1) %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)), 
         dow = wday(date, label = TRUE), weekend = (dow=="Sat"|dow=="Sun"|dow=="Fri")) %>%
  select(date, home, visitor, Season, result, dow, weekend) 

weekend <- italy %>% 
  filter(weekend, Season >=1993) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

weekday <- italy %>% 
  filter(!weekend, Season >=1993) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

diff.pts <- (weekend$h.pt.ave - weekday$h.pt.ave) - 
  (weekend$a.pt.ave - weekday$a.pt.ave) 

diff.pts <- data.frame(seas = 1993:2013, diff = diff.pts)

ggplot(diff.pts, aes(seas, diff)) + 
  geom_point() + geom_smooth() +theme_bw() + xlab("Season") + ylab("Difference") +
  geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) + 
  ggtitle("Home weekend advantage in Italy")







spain <- spainliga %>%
  filter(tier==1) %>%
  mutate(result =  ifelse(hgoal > vgoal, 1, ifelse(hgoal < vgoal, 0, 0.5)), 
         dow = wday(date, label = TRUE), weekend = (dow=="Sat"|dow=="Sun"|dow=="Fri")) %>%
  select(date, home, visitor, Season, result, dow, weekend) 


weekend <- spain %>% 
  filter(weekend, Season >=1980) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

weekday <- spain %>% 
  filter(!weekend, Season >=1980) %>%
  group_by(Season) %>%
  summarise(n.games = n(), n.hw = sum(result==1), n.aw = sum(result==0), 
            n.dr = n.games - n.hw - n.aw, h.pt.ave = (n.hw*3 + n.dr)/n.games, 
            a.pt.ave = (n.aw*3 + n.dr)/n.games)

diff.pts <- (weekend$h.pt.ave - weekday$h.pt.ave) - 
  (weekend$a.pt.ave - weekday$a.pt.ave) 

diff.pts <- data.frame(seas = 1980:2013, diff = diff.pts)

ggplot(diff.pts, aes(seas, diff)) + 
  geom_point() + geom_smooth() +theme_bw() + xlab("Season") + ylab("Difference") +
  geom_abline(intercept = 0, slope = 0, col = "red", lty = 2) + 
  ggtitle("Home weekend advantage in Spain")







