
# Load library ------------------------------------------------------------


library(tidyverse)
library(magick)
library(scales)
library(extrafont)


# Data manipulation -------------------------------------------------------



df <- read.csv("~/datagravis/world-regime/world-population-by-political-regime.csv")

df <- df %>% 
  select(-c("Code")) %>% 
  gather(key = type, value = count, -c("Entity", "Year")) %>%  # mengubah susunan data 
  mutate(lab = case_when( #membuat lab/kategori untuk masing-masing kategori data 
    type == "People.living.in.countries.with.missing.regime.data"~"No regime data",
    type == "People.living.in.closed.autocracies"~"Closed autocracy",
    type == "People.living.in.electoral.autocracies"~"Electoral autocracy",
    type == "People.living.in.electoral.democracies"~"Electoral democracy",
    type == "People.living.in.liberal.democracies" ~ "Liberal democracy",
  ))
  


# kategori data (lab) yang telah dibuat dijadikan factor 

df$lab <- factor(
  df$lab, 
  c("No regime data",
    "Closed autocracy",
    "Electoral autocracy",
    "Electoral democracy",
    "Liberal democracy"
  ))



final1 <-  df %>% 
  filter(Year == "2020") %>% 
  arrange(desc(lab)) %>% 
  mutate(
    ypos = cumsum(count))

pal <-  c("#0a0a0a", "#d7191c", "#fdae61", "#abd9e9", "#2c7bb6")  

df <- df %>%
  mutate(
    col_lab = case_when(
      lab == "No regime data"~"#0a0a0a",
      lab == "Closed autocracy"~"#d7191c",
      lab == "Electoral autocracy"~"#fdae61",
      lab == "Electoral democracy"~"#abd9e9",
      lab == "Liberal democracy"~"#2c7bb6",
    ))


# Plot  -------------------------------------------------------------------


finalplot <-  ggplot(df, aes(x = Year, y = count, fill = lab)) +
  geom_area(alpha = 0.65) +
  scale_x_continuous(limits = c(1800, 2020),
                     breaks = c(1800, 1850, 1900, 1950, 2000, 2020)) +
  scale_y_continuous(breaks = seq(0, 7000000000, by = 1000000000), #Tentukan skala y
                     labels = c("0", "1 Miliar", "2 Miliar", "3 Miliar", "4 Miliar", 
                                "5 Miliar", "6 Miliar", "7 Miliar")) +
  scale_fill_manual(breaks = df$lab,
                    values = df$col_lab) +
  scale_color_manual(breaks = df$lab,
                     values = df$col_lab) +
  labs(title = "Warga dunia yang hidup \ndi bawah rezim politik berbeda",
       subtitle = "Hampir setiap orang tidak memiliki hak politik demokratis di abad ke-19, \ntetapi banyak yang telah memperolehnya sejak itu.",
       caption = "Source : Our World in Data \nChart : Mely Santoso (@melysantoso)") +
  #geom_text(data = final1, aes(y = ypos-2000000, label = lab, color = lab), x =2020, hjust = 0, vjust = 5, family = "Trebuchet MS", size = 4, fontface = "bold") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(plot.title = element_text(family = "Verdana", size = 20, face = "bold", vjust = 1),
        plot.subtitle = element_text(color = 'grey30', size = 12),
        plot.caption = element_text(family = "Arial", colour = 'grey40', size = 9, hjust = 0),
        legend.title=element_blank(), 
        legend.position = "top",
        panel.background = element_rect(fill = "#ffffff"), # ganti backgound jadi abu-abu
        plot.background = element_rect(fill = "#ffffff"), # ganti background plot jadi abu-abu
        legend.background = element_blank(), #element_rect(fill = "#f0f0f0"), # ganti background legend jadi abu-abu
        panel.border = element_blank(), # ilangin panel border
        panel.grid.major = element_blank(), # ngilangin grid border
        panel.grid.minor = element_blank(), # ngilangin grid border 
        #panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", colour = "grey70"),
        panel.grid.major.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.x = element_blank(),
        legend.key.size=unit(0.5, 'cm'),
        legend.text=element_text(family = "Calibri", size=11, hjust = 0),
        axis.text=element_text(face = "bold", size=12))

finalplot


# Add logo  ---------------------------------------------------------------


logo <- image_read("~/datagravis/world-regime/weightaverage.png") #pake library(magick)
finalplot
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo

