
# Load libraries ----------------------------------------------------------


library(hrbrthemes)
library(tidyverse)
library(ggchicklet)
library(showtext)
library(ggtext)
library(magick)


# Data Wrangling  ---------------------------------------------------------



data("debates2019")

df <- debates2019 %>% 
  filter(debate_group == 1) %>% 
  mutate(speaker = fct_reorder(speaker, elapsed, sum, .desc = FALSE)) %>% 
  mutate(topic = fct_other(
    topic, 
    c("Immigration", "Economy", "Climate", "Gun Control", "Healthcare", "Foreign Policy", "Civil Rights")
  ))

# Add font using showtext package 
font_add(family = "kanit", regular = "Kanit-Regular.ttf")
font_add(family = "bebas", regular = "BebasNeue-Regular.ttf")
font_add(family = "Fira Sans", regular = "FiraSans-Regular.ttf")
font_add(family = "georgia", regular = "georgia.ttf")
font_add(family = "tnr", regular = "times.ttf")
showtext_auto()


# Plot --------------------------------------------------------------------


plot <- df %>% ggplot(aes(speaker, elapsed, group = timestamp, fill = topic)) +
  geom_chicklet(width = 0.75) +
  annotate("text", x = "Swalwell", y = 5.8, 
           label = "Setiap segmen bar menunjukkan panjang jawaban \nkandidat terhadap sebuah pertanyaan.",
           color = "black", family = "kanit", hjust = 0) +
  scale_y_continuous(
    expand = c(0, 0.0625),
    position = "right",
    breaks = seq(0, 14, 2),
    labels = c(0, sprintf("%d min.", seq(2, 14, 2)))
  ) +
  scale_fill_manual(
    name = NULL, 
    values = c("Immigration" = "#ae4544",
               "Economy" = "#d8cb98",
               "Climate" = "#a4ad6f",
               "Gun Control" = "#cc7c3a",
               "Healthcare" = "#436f82",
               "Foreign Policy" = "#7c5981",
               "Civil Rights" = "#8fbacc",
               "Other" = "#cccccc"
               ),
    breaks = setdiff(unique(debates2019$topic), "Other")
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = "<span style='color:#E60822;'>**Kandidat**</span> dan Topik Mana yang Paling Banyak <br>Mendapat Waktu dalam Debat Demokrat",
    subtitle = "Malam Pertama dan Kedua Debat Kandidat dari Democrats Juni 2019",
    caption = "Baca selengkapnya di: https://nyti.ms/3EqgNDC \nVisualisasi dibuat ulang oleh: @melysantoso"
    ) +
  theme_ipsum_rc(grid="X") +
  theme(plot.title = element_markdown(family = "georgia", size = 24, vjust = 0, hjust = 0.5),
        plot.subtitle = element_text(family = "tnr", size = 12, hjust = 0.5, vjust = 0.5),
        plot.caption = element_text(family = "Fira Sans", size = 9, hjust = 0.5),
        legend.position = "top",
        axis.text.x = element_text(color = "gray60", size = 10),
        )

logo <- image_read("~/datagravis/world-regime/weightaverage.png") #pake library(magick)
plot
grid::grid.raster(logo, x = 0.20, y = 0.94, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo



