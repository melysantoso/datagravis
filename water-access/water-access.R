
# Load Library ------------------------------------------------------------


library(tidyverse)
library(magick)
library(ggtext)


# Get the data ------------------------------------------------------------


df <- read.csv("~/datagravis/water-access/access-drinking-water-stacked.csv")


# Wrangling data  ---------------------------------------------------------


# if want to add some continent name 
# df$continent <- countrycode(sourcevar = df[, "Entity"],
#                            origin = "country.name", 
#                            destination = "continent")

# Entity == c("High income", "North America and Europe", "Eastern and South-Eastern Asia", 
#"Upper-middle income", "World", "Central and Southern Asia", "Lower-middle income",
#"Western Asia and Northern Africa","Latin America and the Caribbean", "Sub-Saharan Africa", "Low income")

df1 <- df %>% 
  filter(Year == 2020,
         Entity %in% c("High income", 
                       "North America and Europe", 
                       "Upper-middle income", 
                       "World", 
                       "Central and Southern Asia", 
                       "Lower-middle income",
                       "Western Asia and Northern Africa",
                       "Latin America and the Caribbean", 
                       "Sub-Saharan Africa", 
                       "Low income")) %>% 
  select(-c("Code")) %>% 
  gather(key = type, value = count, -c("Entity", "Year")) %>% 
  mutate(type = recode_factor(type, wat_sm = 'Safely managed',
                              wat_bas_minus_sm = 'Basic',
                              wat_lim = 'Limited',
                              wat_unimp = 'Unimproved',
                              wat_sur = 'No access (surface water only)'))


df2 <- df1 %>% mutate(type = recode_factor(type, wat_sm = 'Safely managed',
                                     wat_bas_minus_sm = 'Basic',
                                     wat_lim = 'Limited',
                                     wat_unimp = 'Unimproved',
                                     wat_sur = 'No access (surface water only)'))




# Basic plot  -------------------------------------------------------------

df1 %>% 
  ggplot(aes(x = count,
             y = Entity,
             fill = type)) +
  geom_bar(position = "stack", stat = "identity") 

# Final Plot --------------------------------------------------------------------


plotwater <- df2 %>% 
  ggplot(aes(fill = type, 
                y = Entity,
                x = count)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(x = count, label = paste0(round(count), '%')),
            colour = 'white', size = 3, position = position_stack(vjust=0.5)) +
  #bbc_style() + 
  labs(title = "Access to Safe Drinking Water",
       subtitle = "Share of population with access to drinking water facilities, 2020",
       caption = "Source: WHO/UNICEF Joint Monitoring Programme (JMP) for Water Supply and Sanitation | Our World in Data \nRedesign chart: Mely Santoso") +
  scale_fill_manual(values = c("#3c4e66", "#00847e", "#e7c370", "#f18336", "#c72e43")) +
  theme(plot.title = element_text(size = 24, vjust = 1),
        plot.subtitle = element_text(size = 18, vjust = 1),
        plot.caption = element_text(color = 'grey40', size = 10, hjust = 0),
        legend.title=element_blank(), 
        legend.position='top',
        panel.background = element_rect(fill = "#f0f0f0"), # ganti backgound jadi abu-abu
        plot.background = element_rect(fill = "#f0f0f0"), # ganti background plot jadi abu-abu
        legend.background = element_rect(fill = "#f0f0f0"), # ganti background legend jadi abu-abu
        panel.border = element_blank(), # ilangin panel border
        panel.grid.major = element_blank(), # ngilangin grid border
        panel.grid.minor = element_blank(), # ngilangin grid border 
        #panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.x = element_blank(),
        legend.key.size=unit(0.4, 'cm'),
        legend.text=element_text(size=12, hjust = 0),
        axis.text=element_text(face = "bold", size=11)) 
plotwater



# Enter logo  -------------------------------------------------------------

logo <- image_read("~/datagravis/world-regime/weightaverage.png") #pake library(magick)
plotwater
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo

