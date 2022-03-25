
# Making Interactive Bubble Chart  ----------------------------------------


# Load the Libraries  -----------------------------------------------------



library(tidyverse)
library(plotly)
library(viridis)
library(hrbrthemes)


# Import Data  ------------------------------------------------------------


indexcovID <- read.csv("indexcovidnew.csv")
str(indexcovID)



# Plot  -------------------------------------------------------------------


ggplot(indexcovID, aes(x = faskes, y = kerentanan, fill = positif)) +
  geom_point(alpha = 0.7) 


# Custom the Plot  --------------------------------------------------------


indexplotpos <- indexcovID %>% 
  arrange(desc(positif)) %>%
  mutate(provinsi = factor(provinsi, provinsi)) %>%
  mutate(text = paste("Provinsi: ", provinsi, 
                            "\nKasus Positif: ", positif, 
                            "\nFasilitas Kesehatan: ", faskes, 
                            "\nIndex Kerentanan: ", kerentanan, 
                            sep="")) %>%
  ggplot(aes(x = faskes, y = kerentanan, size = positif, color = pulau, text = text)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(.1, 24), name = "Kasus Positif") +
  scale_color_viridis(discrete = TRUE, guide = FALSE) + 
  scale_x_continuous(breaks = seq(0, 100, by = 2)) +
  scale_y_continuous(breaks = seq(0, 70, by = 2)) +
  theme_ipsum() +
  labs(title = "Index Kerentanan Covid-19 dan Fasilitas Kesehatan Provinsi",
       x = "Skor Fasilitas Kesehatan",
       y = "Index Kerentanan COVID-19") +
  theme(legend.position = "none")
indexplotpos


# Make Interactive  -------------------------------------------------------


ggplotly(indexplotpos, tooltip = "text")  





