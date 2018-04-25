library(XML)
library(rvest)
library(dplyr)
library(ggplot2)


year <- 1990:2017


## NFL
nfl.all <- NULL
for (i in year){
  url <- paste0("http://www.pro-football-reference.com/years/", i, "/draft.htm")
  url.i <- read_html(url)
  table <- url.i %>% html_table(fill = TRUE) 
  table <- table [[1]]  %>% 
    as.data.frame() 
  table <- table[,1:11]
  table <- table[2:nrow(table),]
  names(table) <- c("Rnd", "Pick", "Tm", "Name", "Pos", "Age", "Career", "x1", "x2", "x3", "CarAV")
  nfl <- table
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

year.rates <- nfl.all1 %>% group_by(year) %>% summarise(total.av = sum(CarAV)) %>% mutate(ratio.year = ifelse(year > 2008, 4200/total.av, 1))
nfl.all1 <- nfl.all1 %>% left_join(year.rates) %>% mutate(CarAV.projected = CarAV*ratio.year)

Teams <- sort(unique(nfl.all1$Tm))
Teams[Teams == "SDG"] <- "LAC"
Teams[Teams == "RAM"] <- "LAR"
Teams[Teams == "STL"] <- "LAR"
Teams[Teams == "RAI"] <- "OAK"
Teams[Teams == "PHO"] <- "ARI"
sort(unique(Teams))

nfl.all1$Tm[nfl.all1$Tm == "SDG"] <- "LAC"
nfl.all1$Tm[nfl.all1$Tm == "RAM"] <- "LAR"
nfl.all1$Tm[nfl.all1$Tm == "STL"] <- "LAR"
nfl.all1$Tm[nfl.all1$Tm == "RAI"] <- "OAK"
nfl.all1$Tm[nfl.all1$Tm == "PHO"] <- "ARI"

division <- data.frame(Tm = sort(unique(Teams)), Division = c("NFC West", "NFC South", "AFC North", "AFC East", "NFC South", "NFC North", 
                                                              "AFC North", "AFC North", "NFC East", "AFC West", "NFC North", "NFC North", 
                                                              "AFC South", "AFC South", "AFC South", "AFC West", "AFC West", "NFC West", 
                                                              "AFC East", "NFC North", "NFC South", "AFC East", "NFC East", "AFC East", 
                                                              "AFC West", "NFC East", "AFC North", "NFC West", "NFC West", "NFC South", "AFC South", "NFC East"))

nfl.all1 <- nfl.all1 %>% left_join(division)



ggplot(filter(nfl.all1, year > 1999, Rnd <=7, Division == "AFC North"), aes(pick.number, CarAV.projected)) + 
  geom_jitter(alpha = 0.05) + geom_smooth(method = "loess", span = 0.5, colour = "black", se= FALSE)  + 
  geom_smooth(method = "loess", span = 0.5, se = FALSE, aes(colour = Tm))  + 
  labs(title = "AFC North draft picks since 2000 versus the league average", subtitle = "Using Career Approximate Value via PFR (or projections for recent draftees)") + 
  scale_x_continuous("Pick number") + scale_y_continuous("Career approximate value")+
  theme_bw(15)




library(ggrepel)
nfl.all1 <- nfl.all1 %>% mutate(pats = (Tm == "NWE")) %>% ungroup()
p1 <- ggplot(filter(nfl.all1, year > 1999, Rnd <=7, !pats), aes(pick.number, CarAV.projected)) + 
   geom_jitter(alpha = 0.05) + geom_smooth(method = "loess", span = 0.5, colour = "black", se= FALSE)  + 
  geom_point(data = filter(nfl.all1, year > 1999, Rnd <=7, pats), aes(pick.number, CarAV.projected), colour = "red") + 
  geom_smooth(data = filter(nfl.all1, year > 1999, Rnd <=7, pats), aes(pick.number, CarAV.projected), 
              colour = "red", method = "loess", span = 0.5, fill = "red", se = FALSE) + 
  geom_label_repel(data = filter(nfl.all1,  year > 1999,  Rnd <=7, pats, 
                                 CarAV.projected > 65| (CarAV.projected > 40 & pick.number > 100)), 
                   aes(pick.number, CarAV.projected, label = Name), colour = "red") + 
  geom_label_repel(data = filter(nfl.all1,  year > 1999,  Rnd <=7, pats, 
                                 CarAV.projected < 15, pick.number < 40), 
                   aes(pick.number, CarAV.projected, label = Name), colour = "red") + 
  labs(title = "Patriots draft picks since 2000 versus the league average", subtitle = "Using Career Approximate Value via PFR (or projections for recent draftees)") + 
  scale_x_continuous("Pick number") + scale_y_continuous("Career approximate value")+
  theme_bw(15)
p1


fit.draft <- loess(CarAV.projected ~ pick.number, data = filter(nfl.all1, year > 1999, Rnd <=7))
nfl.all1$yhat <- predict(fit.draft, nfl.all1)

draft.sum <- nfl.all1 %>% filter(year > 1999) %>% group_by(Tm, year) %>% 
  summarise(exp = sum(yhat), 
            obs = sum(CarAV.projected), 
            diff = obs-exp) %>% 
  arrange(diff) %>% ungroup()

draft.sum %>% ggplot(aes(exp, obs)) + geom_point() + 
  geom_label_repel(data = filter(draft.sum,  diff > 150), 
                   aes(exp, obs, label = paste(Tm,year))) +  
  geom_point(data = filter(draft.sum,  diff > 150), 
                   aes(exp, obs)) +
  geom_label_repel(data = filter(draft.sum,  diff < -100), 
                   aes(exp, obs, label = paste(Tm,year))) +  
  geom_point(data = filter(draft.sum,  diff <  -100), 
             aes(exp, obs)) +
  annotate("text", x = 100, y = 350, label = "Better than expected", size = 5, colour = "red")+
  annotate("text", x = 210, y = 20, label = "Worse than expected", size = 5, colour = "red") + 
  xlab("Expected value") + ylab("Observed value") + ggtitle("NFL drafts versus expectations") + 
  geom_abline(aes(intercept = 0, slope = 1), lty = 2) + 
  theme_bw(14)


draft.sum %>% ggplot(aes(exp, obs)) + geom_point(alpha = 0.05) + 
  geom_label_repel(data = filter(draft.sum,  Tm == "NYG"), 
                   aes(exp, obs, label = paste(Tm,year)), colour = "navy blue") +  
  geom_point(data = filter(draft.sum,  Tm == "NYG"), 
             aes(exp, obs), colour = "navy blue") +
  annotate("text", x = 100, y = 350, label = "Better than expected", size = 5, colour = "red")+
  annotate("text", x = 210, y = 20, label = "Worse than expected", size = 5, colour = "red") + 
  xlab("Expected value") + ylab("Observed value") + ggtitle("Giants drafts versus expectations") + 
  geom_abline(aes(intercept = 0, slope = 1), lty = 2) + 
  theme_bw(14)

filter(nfl.all1,  Tm == "NWE", year == 2017)

draft.sum %>% group_by(Tm, year) %>% summarise(total.exp = sum(exp), n.picks = n()) %>% arrange(total.exp) %>% print.data.frame()
library(forcats)
draft.sum %>% group_by(Tm) %>% summarise(total.diff = sum(obs) - sum(exp), n.picks = n()) %>% arrange(total.diff) %>% 
  ggplot(aes(x = fct_reorder(Tm, total.diff), total.diff)) + geom_bar(stat = "identity") + 
  labs(title = "Observed NFL draft value versus expectation, by team", subtitle = "Comparing PFR's career approximate vaue")  + 
  ylab("Difference in performance") + xlab("") + 
  theme_bw(14) + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

\
