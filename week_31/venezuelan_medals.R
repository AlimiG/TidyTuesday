olympics <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv"
)

library(tidyverse) 
veneques <- olympics %>% 
  filter(team == 'Venezuela', !is.na(medal)) %>% 
  select(name,year,city,sport,medal)

veneques[10,"name"] <- "Ruben Limardo" # Fixed mispelling name of Ruben Limardo
medals_levels <- c("Bronze", "Silver", "Gold")
medals_colors <- c("#A77044", "#757579", "#C19E31")

veneques$medal <- factor(veneques$medal, levels = medals_levels,ordered = T)

positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)


line_pos <- data.frame(
  "year"=unique(veneques$year),
  "position"=rep(positions, length.out=length(unique(veneques$year))),
  "direction"=rep(directions, length.out=length(unique(veneques$year)))
)

veneques <- merge(x = veneques, y = line_pos, by = "year",all = TRUE) 

text_offset <- 0.075

veneques$year_count <- ave(veneques$year==veneques$year, veneques$year, FUN=cumsum)
veneques$text_position <- (veneques$year_count * text_offset * veneques$direction) + veneques$position
head(veneques)


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}


l <- get_png("1200px-Olympic_rings_without_rims.svg.png")
f <- get_png("1280px-Flag_of_Venezuela.svg.png")

timeline_plot<-ggplot(veneques,aes(x = year,y = 0, col = medal, label = city))+
  labs(col="Medals")+
  scale_color_manual(values=medals_colors, labels=medals_levels, drop = FALSE)+
  theme_classic()+
  geom_hline(yintercept=0, color = "black", size=0.3)+
  geom_segment(data = veneques,aes(y=position,yend=0,xend=year),color='grey', size=0.2)+
  geom_point(aes(y=0), size=3)+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 30,  vjust = 1),
        plot.subtitle = element_text(size = 13, color = "grey", face = "italic"),
        plot.caption = element_text(color = "grey", size = 13, face = "italic")
        )+
  xlim(1950, 2020)+
  geom_text(data=veneques, aes(x=year,y=-0.1,label=year),size=2.5,vjust=0.5, color='black', angle=0)+
  geom_text(aes(x= year, y = -0.2 , label = city,fontface= "bold"), size = 3.5, vjust = 0.5, color = 'black')+
  geom_text(data = veneques,aes(y=text_position,label=name, fontface = "bold"),size=3.5)+
  labs(title = "Venezuelan medals", 
       subtitle ="A time line with all the medals Venezuela has won in the Olympic Summer Games",
       caption = "Data source: Kaggle")+ 
  annotation_custom(l,xmin = 2000, xmax = 2010, ymin = -1, ymax = -1.5)+
  annotation_custom(f,xmin=1952,xmax = 1962,ymin = 1, ymax = 1.5)

timeline_plot

