
# Import Library ----------------------------------------------------------

library(tidyverse) #packages andialan boq
library(magick) #buat proses png biar sok-sokan dikasih logo gitu 
library(ggtext)
library(extrafont)

# Set Working Directory ---------------------------------------------------

setwd("~/datagravis/tobaco-consu/") #biar kerjaan ini ngumpul di satu tempat 


# Data Manipulation  ------------------------------------------------------

# Import data dan modifikasi nama kolom 
data <- read.csv("~/datagravis/tobaco-consu/share-of-adults-who-smoke.csv") %>% 
  rename(Smoking = Smoking.prevalence..total..ages.15..)

# Filter data yang pengen kamu pake 
multlinesmoke <- data %>% 
  filter(Entity %in% c("Indonesia", "World", "United States", 
                       "Lower middle income", "High income",
                       "Upper middle income", "Middle income"))



# Plot --------------------------------------------------------------------


multichart <- ggplot(multlinesmoke, aes(x = Year, y = Smoking, colour = Entity)) + #Basic ggplot
  geom_line(size = 2) + # Model plot sama ukuran line-nya
  guides(color = guide_legend(override.aes = list(size = 5))) + # Modifikasi kotak legend
  scale_color_manual(values = c("#3c4e66", "#00847e", "#e7c370", "#f18336", "#c72e43", "#b1c734", "#9c1a73")) + #Setting warna manual 
  scale_x_continuous(breaks = seq(2000, 2016, by = 4)) + #Tentukan skala x
  scale_y_continuous(breaks = seq(20, 40, by = 5), #Tentukan skala y
                     labels = c("20%", "25%", "30%", "35%", "40%"))+ #Ganti nama label
  labs(title = "Share of adults who smoke, 2000-2016", #Bikin judul, subjudul, sama caption 
       subtitle = "The share of men and women aged 15 and older who smoke any \ntobacco product on a daily or non-daily basis. It excludes smokeless \ntobacco use.",
       caption = "Primary Source: World Health Organization | Source: Our World in Data \nCreated by: Mely Santoso") +
  theme(plot.title = element_text(size = 24, vjust = 1), #Modifikasi elemen judul
        plot.subtitle = element_text(size = 18, vjust = 1), #Modifikasi elemen subtitle
        plot.caption = element_text(color = 'grey40', size = 11, hjust = 0), #Modifikasi elemen caption
        legend.title=element_blank(), #ngilangin judulnya legend 
        legend.key = element_blank(), #ngilangin kotak lingkaran di legend yang harusnya ada 
        legend.position='top', #posisi legend
        panel.background = element_rect(fill = "#f0f0f0"), # ganti backgound jadi abu-abu
        plot.background = element_rect(fill = "#f0f0f0"), # ganti background plot jadi abu-abu
        legend.background = element_rect(fill = "#f0f0f0"), # ganti background legend jadi abu-abu
        panel.border = element_blank(), # ilangin panel border
        panel.grid.major = element_blank(), # ngilangin grid border
        panel.grid.minor = element_blank(), # ngilangin grid border 
        panel.grid.major.y = element_line(colour = "#f0ebeb"), #setting panel y burem
        axis.title.x = element_blank(), #ngilangin judul axis x
        axis.title.y = element_blank(), #ngilangin judul axis y 
        axis.line = element_line(size = 1, colour = "#f0ebeb"), #setting garis axis x dan y jadi burem
        #axis.text.x = element_blank(), #anjir ini lupa buat apaan 
        legend.key.size = unit(0.4, 'cm'), #setting legend key
        legend.text = element_text(size=12, hjust = 0), #setting text yang ada di legend itu 
        axis.text = element_text(face = "bold", size=14)) #buat setting axis x dan y 

# Proses Logo -------------------------------------------------------------

logo <- image_read("~/datagravis/world-regime/weightaverage.png") #pake library(magick)
multichart
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo



