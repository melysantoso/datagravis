# Load Library ------------------------------------------------------------

library(tidyverse)
library(maps)
library(ggtext)
library(showtext)
library(ggrepel)
library(tibble)
library(patchwork)


# Preparation -------------------------------------------------------------

#fonts
font_add_google("Merriweather", "mrwt")
font_add_google("Bebas Neue", "bebs")
font_add_google("Staatliches", "stat")
font_add_google("Poppins", "pop")

showtext_auto()
showtext_opts(dpi = 320)

# Data Preparation --------------------------------------------------------

df <- read.csv("~/Databank/global-peace-terrorism-index/global-terrorism-index.csv") %>% 
  rename("2011" = "X2011", 
         "2012" = "X2012", 
         "2013" = "X2013", 
         "2014" = "X2014", 
         "2015" = "X2015", 
         "2016" = "X2016", 
         "2017" = "X2017", 
         "2018" = "X2018", 
         "2019" = "X2019", 
         "2020" = "X2020", 
         "2021" = "X2021") %>% 
  gather(key = type, value = count, -c(Country)) %>% 
  # pr : menemukan cara agar data urut sesuai Country baru type 
  filter(type == 2021)

# Map Data 
world_map <- map_data("world") %>% 
  filter(region!="Antarctica")


# Change Country Name


df$Country[df$Country == "United States of America"] <- "USA"
df$Country[df$Country == "United Kingdom"] <- "UK"
df$Country[df$Country == "Kyrgyz Republic"] <- "Kyrgyzstan"
df$Country[df$Country == "Cote d' Ivoire"] <- "Ivory Coast"
df$Country[df$Country == "Republic of the Congo"] <- "Republic of Congo"


# Make categories 
df$bin <- ifelse(df$count>=8.000, "Pengaruh Terorisme Sangat Tinggi", 
                 ifelse(df$count>=6.000, "Tinggi", 
                        ifelse(df$count>=4.000, "Sedang", 
                               ifelse(df$count>=2.000, "Rendah", 
                                      ifelse(df$count>=0.001, "Sangat Rendah",
                                             ifelse(df$count>=0.000, "Tidak Ada Pengaruh Terorisme"))))))
# bikin pelevelan 
df$bin <- factor(df$bin, levels = c("Tidak Ada Pengaruh Terorisme", 
                                    "Sangat Rendah", 
                                    "Rendah", 
                                    "Sedang", 
                                    "Tinggi", 
                                    "Pengaruh Terorisme Sangat Tinggi"))

# satukan data map dan data dampak terorisme pada negara-negara
world_map_df <- left_join(world_map, df, by = c("region"="Country"))

# Aesthethics  ------------------------------------------------------------

colors <- c("#770a1e",
            "#ed1c24", 
            "#f37053",
            "#fec779", 
            "#f6e38d", 
            "#b2ded1"
) 

colors1 <- c("#b2ded1", 
             "#f6e38d",
             "#fec779",
             "#f37053",
             "#ed1c24",
             "#770a1e")


# Basic Visualization -----------------------------------------------------

ggplot(world_map_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = bin), size = 0.2, colour = "#05322B") +
  guides(fill = guide_legend(title.position = "top", 
                             label.position = "bottom",
                             keyheight = unit(0.1, "cm"),
                             keywidth = unit(0.1, "cm"),
                             title.hjust = 0.5, 
                             nrow = 1))+
  geom_polygon(
    data = world_map_df %>% filter(is.na(count)), 
    aes(x = long, y = lat, group = group), 
    fill = "#f5f5f5", 
    colour = "#dee1e6", 
    size = 0.1
  ) +
  scale_fill_manual(values = colors1, na.translate = FALSE,
                    labels = c("Tidak Ada \nPengaruh Terorisme", "Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Pengaruh Terorisme \nSangat Tinggi")) +
  labs(title="Global Terrorism Index 2022",
       subtitle="Dampak Terorisme di Seluruh Dunia",
       fill="Kategori",
       caption="Data : Institute for Economics & Peace | Visualisasi : mely") +
  theme_void()+
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = "pop"),
        #legend.key.size = unit(0.3, "cm"),
        #legend.text = element_text(size = 10, family = "pop"),
        text=element_text(color="black"),
        plot.title = element_text(family="bebs", hjust = .5, face = "bold", size = 30),
        plot.subtitle =element_text(hjust=.5, vjust=1, family = "stat", size = 24),
        plot.caption = element_text(family = "pop", hjust = 0.3))
ggsave("gti-4.jpg", dpi = 320, width = 32.4, height = 17, units='cm')


