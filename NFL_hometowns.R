
library(rvest)
library(XML)

statemap <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", "Connecticut", 
              "District of Columbia", "Delaware","Florida", "Georgia", "Hawaii", "Iowa", "Idaho",
              "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", "Maryland",
              "Maine", "Michigan","Minnesota", "Missouri", "Mississippi", "Montana", "North Carolina",
              "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", "New York",
              "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
              "Tennessee", "Texas", "Utah", "Virginia", "Vermont","Washington", "Wisconsin", "West Virginia", "Wyoming")

states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC","DE",
            "FL", "GA", "HI", "IA", "ID","IL", "IN", "KS", 
            "KY", "LA", "MA", "MD", "ME", "MI", "MN",
            "MO", "MS", "MT", "NC", "ND", "NE","NH", "NJ", "NM",
            "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
            "TN", "TX", "UT", "VA", "VT","WA", "WI", "WV", "WY")

nfl.all <- NULL
for (i in states){
  url <- paste0("http://www.pro-football-reference.com/friv/birthplaces.cgi?country=USA&state=", i)
  tables <- readHTMLTable(url)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  nfl <- tables[[which.max(n.rows)]]
  colnames(nfl)[2] <- "Player"
  nfl <- nfl[,c(2:4)]
  nfl$state <- i
  nfl.all <- rbind(nfl.all, nfl)
  print(i)
}

nfl.all1 <- nfl.all

city.map <- data.frame(STNAME = statemap, states = states)

cities <- read.csv("http://www.census.gov/popest/data/cities/totals/2014/files/SUB-EST2014_ALL.csv", header = TRUE)
cities <- select(cities, NAME, STNAME, POPESTIMATE2014)
cities1 <- left_join(cities, city.map)
cities1$NAME<-gsub(" city", "", cities1$NAME)
cities1$NAME<-gsub(" town", "", cities1$NAME)
cities1$NAME<-gsub("(pt.)", "", cities1$NAME)
cities1$NAME<-gsub(" village", "", cities1$NAME)
cities1$NAME<-gsub(" township", "", cities1$NAME)
cities1$NAME<-gsub(" borough", "", cities1$NAME)
cities1$NAME<-gsub("()", "", cities1$NAME)


cities1$ID <- paste(cities1$states, cities1$NAME)
sample_n(cities1, 5)



nfl.tab <- nfl.all %>%
  group_by(state, City) %>%
  summarise(total.players = n())
colnames(nfl.tab)[1:2] <- c("states", "NAME")
nfl.tab$ID <- paste(nfl.tab$states, nfl.tab$NAME)



cities2 <- left_join(cities1, nfl.tab)
cities2[is.na(cities2$total.players),]$total.players <- 0
cities2 <- filter(cities2, POPESTIMATE2014>0)
cities21 <- unique(cities2)

temp <- cities21 %>%
  group_by(ID) %>%
  arrange(POPESTIMATE2014) %>%
  summarise(max.pop = max(POPESTIMATE2014))

cities22 <- inner_join(cities21, temp)
cities23 <- filter(cities22, POPESTIMATE2014 == max.pop)

cities3 <- cities23 %>%
  mutate(ratio = total.players/POPESTIMATE2014*1000) %>%
  arrange(-ratio) %>%
  filter(POPESTIMATE2014 > 50, total.players > 1) 

new <- select(cities3, NAME, STNAME, POPESTIMATE2014, total.players, ratio)
colnames(new)<- c("Town", "State", "Population", "Players", "Ratio_1000")
new[1:20,]


qplot(total.players, data=filter(cities23, POPESTIMATE2014 <= 1200 & POPESTIMATE2014 >= 800, total.players > 0), geom="histogram", binwidth = 0.5) + theme_bw() +
  scale_x_continuous("Players") + ggtitle("Number of NFL players from small towns (between 800 and 1200 residents)") + 
  geom_segment(aes(x = 5.75, y = 20, xend = 5.25, yend = 3), arrow = arrow(length = unit(0.5, "cm")), col = "red")+ 
  annotate("text", x = 5.75, y = 23, label = "Lamar, SC", col = "red", cex = 5) +
  scale_y_continuous("Count")
