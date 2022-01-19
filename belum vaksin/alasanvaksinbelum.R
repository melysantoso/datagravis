
# Load Library ------------------------------------------------------------



library(tidyverse)
library(systemfonts)
library(extrafont)
library(magick)


# Prepare the Data --------------------------------------------------------



vakdat <- read.csv("~/datagravis/belum vaksin/survei-vaksin.csv")

vakdat <- vakdat %>% 
  mutate(nama_data = forcats::fct_rev(forcats::fct_inorder(nama_data)))
         
ggplot(vakdat, aes(x = value, y = nama_data)) +
  geom_col(fill = "gray70") +
  theme_minimal()

vakdat <- vakdat %>% 
  dplyr::mutate(
    perc = paste(value, "%"),
    ## customize label for the first category
    perc = if_else(row_number() == 1, paste(perc, "dari responden survey"), perc)
  )

vakdat <-  vakdat %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "goldenrod1",
      row_number() == 2 ~ "#f18336",
      row_number() == 3 ~ "#e7c370",
      nama_data == "Lainnya" ~ "gray85",
      ## all others should be gray
      TRUE ~ "gray70"
    ))



# Plot --------------------------------------------------------------------



final_chart <-  ggplot(vakdat, aes(x = value, y = nama_data, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = perc), 
    hjust = 1, nudge_x = -.5,
    size = 5, fontface = "bold", family = "Trebuchet MS"
  ) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  labs(title = "Alasan Orang Indonesia Belum Vaksin",
       subtitle = "Fixpoll melakukan survei terhadap 1.240 responden yang tersebar di seluruh \nprovinsi Indonesia pada 16-27 Juli 2021. Survei tersebut menggunakan metode \nmultistage random sampling dengan tingkat toleransi kesalahan (margin of error) \ndi bawah 2,89% dan tingkat kepercayaan 95%.",
       caption = "Source : Fixpoll Research and Strategic Consulting (Via Katadata Databoks) \nCreated by: Mely Santoso") +
  theme_void() +
  theme(plot.title = element_text(family = "Geometr212 BkCn BT", face = "bold", size = 24, vjust = 1),
        plot.subtitle = element_text(color = 'grey30', size = 14, vjust = 1),
        plot.caption = element_text(color = 'grey40', size = 11, hjust = 0),
        plot.background = element_rect(fill = "#f0f0f0"),
        axis.text.y = element_text(size = 14, hjust = 1, family = "Trebuchet MS"),
        plot.margin = margin(rep(15, 4)))


# Add Logo ----------------------------------------------------------------



logo <- image_read("~/datagravis/world-regime/weightaverage.png") #pake library(magick)
final_chart
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo

