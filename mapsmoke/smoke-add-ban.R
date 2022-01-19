library(tidyverse)
library(ggmap)
library(viridis)
library(extrafont)
library(magick)

setwd("~/datagravis/mapsmoke/") 

smokedata <-  read.csv("enforcement-of-bans-on-tobacco-advertising.csv") %>% 
  rename(region = Entity, 
         adsind = Indicator.Enforce.bans.on.tobacco.advertising) %>% 
  filter(Year == 2014)

smokedata$region[smokedata$region=="United States"] = "USA"
smokedata$region[smokedata$region=="United Kingdom"] = "UK"
smokedata$region[smokedata$region=="Congo"] = "Republic of Congo"
smokedata$region[smokedata$region=="Democratic Republic of Congo"] = "Democratic Republic of the Congo"

world_map <- map_data("world")

world_map <- left_join(world_map, smokedata, by = "region")

finalplot <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = adsind), size=0, alpha=0.8) +
  #theme_void() +
  scale_fill_viridis(trans = "log", 
                     breaks=c(2, 3, 4, 5), 
                     labels = c("Tidak Ada", "Media \n(TV/Radio/Cetak)", "Larangan \nLangsung dan Tidak", "Larangan Penuh"),
                     name="Jenis Larangan", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = "top", 
                                           nrow=1) ) +
  labs(
    title = "Penegakan Larangan Iklan Rokok, 2014",
    subtitle = "Di beberapa negara iklan rokok telah benar-benar dilarang tayang di berbagai media. \nIndonesia menjadi salah satu negara yang tidak melarang iklan rokok",
    caption = "Source: Our World in Data \nChart: Mely Santoso @melysantoso"
  ) +
  theme(
    plot.title = element_text(family = "Geometr212 BkCn BT", face = "bold", size = 24,  vjust = 1),
    plot.subtitle = element_text(size = 18, vjust = 1),
    plot.caption = element_text(color = 'grey40', size = 10, hjust = 0),
    legend.title=element_blank(), 
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.text = element_text(size = 10, family = "Trebuchet MS"),
    legend.position = c(0.7, 0.22),
    panel.border = element_blank(), # ilangin panel border
    panel.grid.major = element_blank(), # ngilangin grid border
    panel.grid.minor = element_blank(), # ngilangin grid border 
    #panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
    )


logo <- image_read("~/datagravis/world-regime/weightaverage.png") #pake library(magick)
finalplot
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo


