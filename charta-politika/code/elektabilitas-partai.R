
# Load Library ------------------------------------------------------------


library(tidyverse)
library(grid)
library(ggtext)
library(showtext)
library(here)
library(ragg)


# Typography Preparation --------------------------------------------------

#fonts
font_add_google("Merriweather", "mrwt")
font_add_google("Bebas Neue", "bebs")
font_add_google("Staatliches", "stat")
font_add_google("Poppins", "pop")
font_add_google("Roboto", "rbt")
showtext_auto()
showtext_opts(dpi=100)


# Data Wrangling ----------------------------------------------------------



setwd("~/Databank/charta-politika/data/")
df <- read.csv("elektabilitas-partai.csv")
glimpse(df)

df <- df %>% 
  mutate(partai = forcats::fct_rev(forcats::fct_inorder(partai)))

df <- df %>% 
  dplyr::mutate(
    perc = paste(elektabilitas, "%"),
    perc = if_else(row_number() == 1, paste(perc, "dari responden survei"), perc)
  )

df <-  df %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "#DC0000",
      row_number() == 2 ~ "#850000",
      row_number() == 3 ~ "#FFFF00",
      row_number() == 4 ~ "#285430",
      row_number() == 5 ~ "#001253",
      row_number() == 6 ~ "#FF731D",
      partai == "TT/TJ" ~ "#7F7F7F",
      TRUE ~ "#D0ECEF"
    ))





# Plot --------------------------------------------------------------------


final_plot <- df %>% 
  mutate(place = if_else(row_number() == 1, 1, 0),
         perc = paste(" ", perc, " ")) %>% 
ggplot(aes(x = elektabilitas, y = partai, fill = color)) +
  geom_col(position = "dodge", alpha = 0.9) +
  geom_text(
    aes(label = perc, hjust = place), size = 3.5, family = "rbt"
  ) +
  scale_x_continuous(expand = c(.01, .01)) + 
  scale_fill_identity(guide = "none") +
  labs(
    title = "<span style='color:#D13223'>GATRA</span>",
    subtitle = "<span style='color:#ff0000'>Elektabilitas Partai Politik Menurut Charta Politika</span> 
    <br>Survei yang dilakukan oleh Charta Politika periode 8-16 Desember menemukan 
    <br>PDIP menempati posisi elektabilitas partai tertinggi dari parpol lain",
    caption = "Source: Charta Politika | Viz: GATRA/Mely", 
    x = NULL,
    y = NULL,
    col = NULL
  ) +
  theme_minimal(base_family = "pts") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.3, color = "#DAD9D9"),
        plot.background = element_rect(color = NA, fill = "white"),
        text = element_text(family = "rbt"),
        axis.line.x = element_line(color = "black", size = 0.3),
        axis.ticks.x = element_line(color = "black", size = 0.3), 
        axis.ticks.length.x = unit(2, "mm"), 
        axis.title = element_text(family = "rbt"),
        # axis.text.y = element_text(),
        axis.text.y = element_text(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.justification = "left",
        legend.text = element_markdown(),
        plot.title = element_markdown(face = "bold", family = "mrwt", size = 26, margin = margin(t=16, b = 3)),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(margin = margin(b = 8), family = "pop", lineheight = 1, size = 11),
        plot.caption = element_text(hjust = 0, size = 8, family = "rbt"),
        plot.caption.position = "plot"
  )

final_plot




# Save Plot ---------------------------------------------------------------


ragg::agg_png(here("elektabilitas-parpol.png"), res = 100, width = 650, height = 433, units = "px")
final_plot
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#D13223", lwd = 2)
)
grid.rect(
  x = 0,
  y = 1,
  width = 0.2, # TODO 10 % of line / image width
  height = 0.05,  # TODO ~2 % of line / image height
  gp = gpar(fill = "#D13223", col = NA)
)
final_plot

  







