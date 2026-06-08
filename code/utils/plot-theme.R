
##plot template====
temp <-
  theme(axis.ticks.length = unit(.2, "cm"),
        plot.title = element_text(size = 12,face="bold"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black"),
        panel.spacing = unit(0.1, "cm"),
        plot.background=element_blank(),
        strip.background = element_rect(colour="black",linewidth=0.5),
        panel.background = element_rect(fill = "grey90"),
        panel.grid.major = element_line(colour = "white"),
        axis.ticks = element_line(colour = "black"),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        strip.text = element_text(size=12),
        legend.position = "right", 
        legend.background = element_rect(NA),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank())
