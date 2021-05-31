library(grid)
library(tidyverse)
library(png)
library(extrafont)
library(ggimage)
library(magick)
library(pacman)
library(grid)
library(png)
library(lubridate)
library(extrafont)
#extrafont::font_import()
#font_import()
loadfonts(device = "win")
tuesdata <- tidytuesdayR::tt_load(2021,week = 22)


drivers <- tuesdata$drivers
records <- tuesdata$records




 records_dec <- records %>%
mutate(shortcut = as.factor(shortcut)) %>% 
  mutate(decade = as.factor(floor(year(as.Date(date))/10)*10)) 


 
 testy <- records_dec  %>% 
  group_by(decade, shortcut) %>% 
  summarise(n = n()) %>% 
   mutate(freq = n / sum(n))
 
mariobkg <- png::readPNG('img/mariocricuit2.png')
  

 p <- ggplot(testy,aes(x = shortcut, y = freq)) +
geom_col(aes(fill = shortcut)) +
  scale_fill_manual(values = c('#30993A',"#ed0f0f"), guide = FALSE)+
  facet_grid(~decade)+
  theme(
    strip.background = element_blank(),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(family = windowsFont(family = "Press Start 2P"), color = "gold", size = 8),
    strip.text = element_text(color = "gold", family = windowsFont(family = "Press Start 2P") ),
   axis.text.x  = element_text(family = windowsFont(family = "Press Start 2P"), color = "gold", size = 8,margin = margin(t =0, b = 12)),
   plot.title =  element_text(family = windowsFont(family = "SNES"), size = 24, face="bold.italic", color = "White",hjust = 0.5),
  axis.title = element_text(family = windowsFont(family = "SNES"), size = 22, color = "white"),
  #panel.grid.major = element_blank(), 
  panel.grid.major.x = element_blank(),
  plot.margin = unit(c(2,4,1.5,1.5), "cm")
  )+
  xlab("\nDid they use shortcuts?")+
  ylab("Proportion\n")+
   labs(title="MARIO KART RECORDS AND SHORTCUTS by decade")


  ggbackground(p,"img/maxresdefault.jpg", alpha = 0.3, by = "width")
  ggsave("week2.png",device = "png")
