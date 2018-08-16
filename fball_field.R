

theme.football <- function(){
  theme(panel.grid.minor = element_blank(), 
        axis.text.y = element_text(angle=270, hjust=0.5), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "black"), 
        axis.ticks = element_blank()) 
}


base.football <- ggplot() + xlab("") + ylab("") + 
  theme_minimal() +
  annotate("segment", x = c(-26.6, -26.6, 26.6, 26.6), 
           y = c(-10, 110, 110, -10), 
           xend = c(-26.6, 26.6, 26.6, -26.6), 
           yend = c(110, 110, -10, -10), colour = "black") + 
  annotate("segment", x = c(-3.1, 3.1), 
           y = c(0, 0), 
           xend =  c(-3.1, 3.1), 
           yend = c(100, 100), colour = "black", lty = 2, alpha = 0.5) + 
  annotate("segment", x = rep(-26.6, 11), 
           y = seq(0, 100, by = 10), 
           xend =  rep(26.6, 11), 
           yend = seq(0, 100, by = 10)) +
  annotate("text", x = rep(-20.5, 9), y = seq(10, 90, by = 10), 
           label = c(seq(10, 50, by = 10),rev(seq(10, 40, by = 10))), 
           angle = 270, size = 5) + 
  annotate("text", x = rep(20.5, 9), y = seq(10, 90, by = 10), 
           label = c(seq(10, 50, by = 10),rev(seq(10, 40, by = 10))), 
           angle = 90, size = 5) + 
  scale_y_continuous("", breaks = NULL, lim = c(-10, 110)) + 
  scale_x_continuous("", breaks = NULL, lim = c(-26.6, 26.6)) + 
  annotate("rect", xmin=-26.6, xmax=26.6, ymin=-10, ymax=110, fill="palegreen", alpha=0.1)  + 
  theme.football()
