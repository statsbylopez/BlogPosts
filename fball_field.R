
theme.football <- function(){
  theme(panel.grid.minor = element_blank(), 
        axis.text.y = element_text(angle=270, hjust=0.5), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black"), 
        axis.ticks = element_blank()) 
}

xlim <- (160/3)/2
hash.width <- 3.3
hash.x <- (xlim + hash.width)/2

df.hash <- expand.grid(x = c(-1*xlim, -1*hash.width, hash.width, xlim), y = (0:100))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
base.football <- ggplot() + xlab("") + ylab("") + 
  theme_minimal() +
  annotate("segment", x = c(-1*xlim, -1*xlim, xlim, xlim), 
           y = c(-10, 110, 110, -10), 
           xend = c(-1*xlim, xlim, xlim, -1*xlim), 
           yend = c(110, 110, -10, -10), colour = "black") + 
  #geom_point(data = df.hash, aes(x, y), pch = 1) + 
  annotate("text", x = df.hash$x[df.hash$x < 0], y = df.hash$y[df.hash$x < 0], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 0], y = df.hash$y[df.hash$x > 0], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = rep(-1*xlim, 21), 
           y = seq(0, 100, by = 5), 
           xend =  rep(xlim, 21), 
           yend = seq(0, 100, by = 5)) +
  annotate("text", x = rep(-1*hash.x, 11), y = seq(0, 100, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep(hash.x, 11), y = seq(0, 100, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  scale_y_continuous("", breaks = NULL, lim = c(-10, 110)) + 
  scale_x_continuous("", breaks = NULL, lim = c(-1*xlim, xlim)) + 
  annotate("rect", xmin=-1*xlim, xmax=xlim, ymin=-10, ymax=110, fill="palegreen", alpha=0.1)  + 
  theme.football()

base.football
