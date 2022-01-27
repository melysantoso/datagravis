library(tidyverse)
library(osmdata)
library(sf)
library(showtext)
library(here)


# Get The OSM Data --------------------------------------------------------


# Data main street di Malang 

main_street <- getbb("Malang Indonesia") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "teritary")) %>% 
  osmdata_sf()

# residential street di malang 
small_street <- getbb("Malang Indonesia") %>% 
  opq() %>% 
  add_osm_feature(key = "highway", 
                  value = c("residential",
                            "unclassified"
                            )) %>% 
  osmdata_sf()

# Sungai di Malang 
malang_river <- getbb("Malang Indonesia")%>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

# import my place data 

my_data <- read_csv("~/datagravis/map-ngopi-malang/ma-map.csv") %>% 
  rename(Kategori = Type)



# Import the Font ---------------------------------------------------------


font_add("Lemon Milk Bold", "/Windows/Fonts/LEMONMILK-Bold.otf")
title_font <- "Lemon Milk Bold"

font_add("Lemon Milk Light", "/Windows/Fonts/LEMONMILK-Light.otf")
chart_font <- "Lemon Milk Light"

showtext_auto()
showtext_opts(dpi = 320)

# Dark Map ----------------------------------------------------------------


# set color for the dark map

background_color<-'#151515'
street_color<-'#BCBDC0'
river_color<-'#1C7293'
font_color<-'#FFFFFF'
dot_colors <-c('#FFD23F','#3BCEAC','#EE4266','#6976EA')

# create map - dark theme 

ggplot() +
  geom_sf(data = main_street$osm_lines, 
          inherit.aes = F, 
          color = street_color,
          size = .6, 
          alpha = .8) +
  
  geom_sf(data = small_street$osm_lines,
          inherit.aes = FALSE, 
          color = '#ffbe7f', 
          size = .2, 
          alpha = .8) +
  geom_sf(data = malang_river$osm_lines,
          inherit.aes = FALSE, 
          color = river_color, 
          size = 1, 
          alpha = .8) +
  coord_sf(xlim = c(112.57, 112.70), 
           ylim = c(-8.06, -7.91),
           expand = FALSE) + 
  geom_point(data = my_data, aes(Longitude, Latitude, color = Kategori)) +
  scale_color_manual(values = dot_colors) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(title = "Yoiki Ngalam Sam", 
       subtitle = "Wes Ngopi a sam?") +
  theme_void() +
  theme(plot.title = element_text(family = title_font, size = 24, 
                                  color = font_color, face = "bold", 
                                  hjust = .5),
        plot.subtitle = element_text(family = chart_font, size = 16, 
                                     hjust = .5, color = font_color, 
                                     margin = margin(2, 0, 5, 0)),
        plot.background = element_rect(fill = background_color),
        legend.position = "top",
        legend.text = element_text(color = font_color, size = 10, 
                                   family = chart_font),
        legend.title = element_text(color = font_color, size = 12, 
                                    family = chart_font), 
        plot.margin = unit(c(0,0,0,0), "cm")
        )


# Save The Map ------------------------------------------------------------

ggsave(here::here("render", paste0("malang_map", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width=7, height=9.35)





# Light Theme -------------------------------------------------------------

# Set the light theme color 

font_color2<-'#151515'
street_color2<-'#746C70'
river_color2<-'#57B8FF'
background_color2<-'#FFFFFF'
dot_colors2 <-c('#f8bf04','#3BCEAC','#EE4266','#6976EA')


# create map - light theme 

ggplot() +
  geom_sf(data = main_street$osm_lines, 
          inherit.aes = F, 
          color = street_color2,
          size = .6, 
          alpha = .8) +
  geom_sf(data = small_street$osm_lines,
          inherit.aes = FALSE, 
          color = street_color2, 
          size = .2, 
          alpha = .8) +
  geom_sf(data = malang_river$osm_lines,
          inherit.aes = FALSE, 
          color = river_color2, 
          size = 1, 
          alpha = .8) +
  coord_sf(xlim = c(112.57, 112.70), 
           ylim = c(-8.06, -7.91),
           expand = FALSE) + 
  geom_point(data = my_data, aes(Longitude, Latitude, color = Kategori)) +
  scale_color_manual(values = dot_colors2) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(title = "Yoiki Ngalam Sam", 
       subtitle = "Wes Ngopi a sam?") +
  theme_void() +
  theme(plot.title = element_text(family = title_font, size = 24, 
                                  color = font_color2, face = "bold", 
                                  hjust = .5),
        plot.subtitle = element_text(family = chart_font, size = 16, 
                                     hjust = .5, color = font_color2,
                                     margin = margin(2, 0, 5, 0)),
        plot.background = element_rect(fill = background_color2),
        legend.position = "top",
        legend.text = element_text(color = font_color2, size = 10, 
                                   family = chart_font),
        legend.title = element_text(color = font_color2, size = 12, 
                                    family = chart_font),
        plot.margin = unit(c(0,0,0,0), "cm")
  )


# Save The Map ------------------------------------------------------------

ggsave(here::here("render", paste0("malang_map", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width=7, height=9.35)


