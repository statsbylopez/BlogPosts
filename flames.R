library(dplyr); library(ggplot2)
flames <- read.csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/flames.csv")
names(flames) <- c("gid", "taken", "drawn")
flames$post <- flames$gid > 2015020742
flames$seas <- as.numeric(substr(flames$gid, 1, 4))

flames <- flames %>%
  group_by(seas) %>%
  mutate(game.seas = 1:n()/82, 
         game.seas.num = seas + game.seas, 
         net = taken - drawn)

flames %>% 
  group_by(post) %>%
  summarise(cor.pair = cor(taken, drawn), 
            mean.taken = mean(taken), 
            mean.drawn = mean(drawn), 
            prop.0 = mean(taken == 0), 
            sum.taken = sum(taken),
            n = n())

flames <- ungroup(flames)

ggplot(flames, aes(game.seas.num, taken, group = post)) + 
  geom_point(pch=21, fill="red", colour = "red", alpha = 0.2) +
  geom_smooth(colour="red", fill = "red", span = 1.3) + 
  geom_point(data = flames, aes(game.seas.num, drawn, group = post), 
              fill="grey40", colour = "grey40", alpha = 0.2) + 
  geom_smooth(data = flames, aes(game.seas.num, drawn), colour="grey40", 
              fill = "grey40", span = 1.3)  + 
  geom_vline(xintercept = 2015.585, colour = "black", lty = 2) + 
  ylab("Penalties") + xlab("Season") +
  annotate("text", x = c(2014.75, 2016.2), y = c(8, 8), label = c("Pre-hit", "Post-hit"), 
           colour = "black", size = 6) + 
  annotate("text", x = c(2014.5, 2014.5), y = c(1, 4), label = c("Taken", "Drawn"), 
           colour = c("red", "grey40"), size = 6) + 
  labs(title = "Calgary Flames penalties", 
       subtitle = "Pre and post Dennis Wideman hit on official", 
       caption = "Data via @IneffectiveMath") + 
  theme_bw()


### Poisson testing and related analysis
flames.count <- flames %>% 
  group_by(post, taken) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n), type = "observed")
flames.count


flames.exp <- flames.count[,1:3]
flames.exp$freq <- ifelse(flames.exp$post, dpois(flames.exp$taken, 3.34), 
                              dpois(flames.exp$taken, 2.13))
flames.exp$type <- "expected"

flames.all <- rbind(flames.count, flames.exp)

poisson.test(c(129,94),c(275, 314))

## Compare to Poisson random variables
ggplot(flames.all, aes(taken, freq, group = type, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~post)


## Poisson regression
fit <- glm(taken ~ post, data = flames, family = poisson)
with(fit, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## Pre and post histograms
ggplot(flames, aes(taken, group = post, colour = post, fill = post)) + 
  geom_histogram(aes(y = ..density..), position = "dodge")


  



  
  
  