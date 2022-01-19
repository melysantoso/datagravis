library(tidyverse)
library(magick)
library(scales)
library(extrafont)

setwd("~/datagravis/democratic/")
regime <- read.csv("~/datagravis/democratic/world-population-by-political-regime.csv")
glimpse(regime)


# Data Wrangling ----------------------------------------------------------


regime <- regime %>% 
  select(-c("Code")) %>% 
  gather(key = type, value = count, -c("Entity", "Year")) %>% 
  mutate(lab = case_when(
    type == "People.living.in.countries.with.missing.regime.data"~"No regime data", 
    type == "People.living.in.closed.autocracies"~"Closed autocracies",
    type == "People.living.in.electoral.autocracies"~"Electoral autocracies", 
    type == "People.living.in.electoral.democracies"~"Electoral democracies",
    type == "People.living.in.liberal.democracies"~"Liberal democracies",
  )) %>% 
  group_by(Year, lab) %>% 
  summarise(n = sum(count)) %>% 
  mutate(percentage = n / sum(n))


# kategori data (lab) yang telah dibuat dijadikan factor 

regime$lab <- factor(
  regime$lab,
  c("No regime data",
    "Closed autocracies",
    "Electoral autocracies",
    "Electoral democracies",
    "Liberal democracies")
)



final1 <-  regime %>% 
  filter(Year == "2020") %>% 
  arrange(desc(lab)) %>% 
  mutate(
    ypos = cumsum(percentage))

pal <-  c("#0a0a0a", "#d7191c", "#fdae61", "#abd9e9", "#2c7bb6")  

regime <- regime %>%
  mutate(
    col_lab = case_when(
      lab == "No regime data"~"#0a0a0a",
      lab == "Closed autocracies"~"#d7191c",
      lab == "Electoral autocracies"~"#fdae61",
      lab == "Electoral democracies"~"#abd9e9",
      lab == "Liberal democracies"~"#2c7bb6",
    ))






# Visualization -----------------------------------------------------------

chartperc <- ggplot(regime, aes(x = Year, y = percentage, fill = lab)) +
  geom_area(alpha = 0.65) +
  scale_x_continuous(limits = c(1800, 2020),
                     breaks = c(1800, 1850, 1900, 1950, 2000,2020)) +
  scale_y_continuous(breaks = seq(0.00, 1.00, by = 0.25),
                     labels = c("0 %", "25 %", "50 %", "75 %", "100 %")) +
  scale_fill_manual(breaks = regime$lab,
                    values = regime$col_lab) +
  scale_color_manual(breaks = regime$lab,
                     values = regime$col_lab) +
  labs(title = "Warga dunia yang hidup \ndi bawah rezim politik berbeda",
       subtitle = "Hampir setiap orang tidak memiliki hak politik demokratis di abad ke-19, \ntetapi banyak yang telah memperolehnya sejak itu.",
       caption = "Source : Our World in Data \nChart : Mely Santoso (@melysantoso)") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(plot.title = element_text(family = "Verdana", size = 20, face = "bold", vjust = 1),
        plot.subtitle = element_text(color = 'grey30', size = 12),
        plot.caption = element_text(family = "Arial", colour = 'grey40', size = 9, hjust = 0),
        legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        legend.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(linetype = "dotted", colour = "grey70"),
        panel.grid.major.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.x = element_blank(),
        legend.key.size=unit(0.3, 'cm'),
        legend.text=element_text(family = "Calibri", size=10, hjust = 0),
        axis.text=element_text(face = "bold", size=12)
        )
chartperc


logo <- image_read("~/datagravis/democratic/weightaverage.png") #pake library(magick)
chartperc
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo


# Coretan -----------------------------------------------------------------