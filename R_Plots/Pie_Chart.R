#Sectors diagrams

library(tidyverse)
library(ggplot2)
library(showtext)

font_add(family = "CMU Serif", regular = "cmunrm.ttf")

showtext_auto()

pc <- c(68.7, 1.2, 30.1)

ar <- c("Ice and snow", "Geographic Areas", "Groundwater")

ar <- ar[order(pc)]; pc <- sort(pc)

ar.factor <- factor(ar, levels = as.character(ar))

ypos <- cumsum(pc) - 0.5 * pc

ypos <- 100 - ypos

ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = pc, fill = ar.factor),
           stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  ggtitle("Distribution of fresh drinking  water") +
  theme(plot.title = element_text(hjust = 0.1, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "CMU Serif")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "YlGnBu", name = "Zones") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(hjust = 0.5, size = 18),
        legend.key.size = unit(1, "cm")) +
  geom_text(aes(x = "", y = ypos, label = paste0(pc, "%")),
            size = 6, family = "CMU Serif")

lbl <- paste(pc, "%", sep = " ")

clr <- c("purple", "orange", "blue")

pie(pc, labels = lbl, clockwise = TRUE, col = clr, 
    main = "Fresh drinking water")

legend("topright", c("Ice and snow", "Geographic Areas", "Groundwater"), 
       cex = 0.5, fill = clr)