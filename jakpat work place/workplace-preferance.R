library(tidyverse)
library(grid)
library(ggtext)
library(showtext)
library(here)
library(ragg)
library(treemapify)

## alt col #d71440 - pink pekat / #96d5d2 - biru muda / #243842 - biru donker / #ff6900 - orange 





showtext_auto()
showtext_opts(dpi=100)
font_add_google("PT Serif", "pts")
font_add_google("EB Garamond", "ebg")

setwd("~/Databank/jakpat work place/")
name <- c("WFO", "WFH", "HYBRID", "WFA")
value <- c(44, 15, 21, 19)
df <-  data.frame(name, value)

glimpse(df)

df <- df %>% 
  mutate(name = forcats::fct_rev(forcats::fct_inorder(name)))

df <- df %>% 
  dplyr::mutate(
    perc = paste(value, "%")
  )

glimpse(df)

ggplot(df, aes(area = value, fill = value)) +
  geom_treemap()

ggplot(df, aes(area = value, fill = name)) +
  geom_treemap()

tree_map <- ggplot(df, aes(area = value, fill = name, label = paste(name, perc, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(place = "centre", colour = "white", family = "ebg", size = 14) + 
  scale_fill_manual(values = c("WFO" = "#d71440", "WFH" = "#96d5d2", 
                               "HYBRID" = "#243842", "WFA" = "#ff6900")) +
  labs(
    title = "<span style='color:#D13223'>GATRA</span>",
    subtitle = "<span style='color:#ff0000'>Preferensi Tempat Kerja Pascapandemi</span> 
    <br>Survei yang dilakukan oleh JakPat menunjukkan preferensi tempat kerja oleh pegawai
    <br>pascapandemi Covid-19. Mayoritas pegawai lebih memilih bekerja di kantor dibandingkan,
    <br>bekerja di rumah, secara hibrida, atau bahkan dari mana saja",
    caption = "Source: Jakpat | Viz: GATRA/Mely",) +
  theme_minimal() +
  theme(
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_markdown(family = "ebg", size = 8),
        legend.key.size = unit(0.3, "cm"),
        plot.title = element_markdown(family = "pts", face = "bold", size = 30, margin = margin(t=16, b = 3)),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(margin = margin(b = 8),lineheight = 1, linewidth = 5, family = "ebg", size = 12),
        plot.caption = element_text(hjust = 0, size = 8, family = "ebg"),
        plot.caption.position = "plot")

tree_map
ragg::agg_png(here("workplace_preference_tree_map.png"), res = 100, width = 650, height = 433, units = "px")
tree_map
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
tree_map
