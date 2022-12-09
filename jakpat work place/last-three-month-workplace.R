library(tidyverse)
library(grid)
library(ggtext)
library(showtext)
library(here)
library(ragg)


## alt col #d71440 - pink pekat / #96d5d2 - biru muda / #243842 - biru donker / #ff6900 - orange 



showtext_auto()
showtext_opts(dpi=100)
font_add_google("PT Serif", "pts")
font_add_google("EB Garamond", "ebg")

setwd("~/Databank/jakpat work place/")
df <- read.csv("work-preference.csv")
glimpse(df)

df <- df %>% 
  mutate(name = forcats::fct_rev(forcats::fct_inorder(name)))

df <- df %>% 
  dplyr::mutate(
    perc = paste(value, "%"),
    perc = if_else(row_number() == 1, paste(perc, "dari responden survei"), perc)
  )

glimpse(df)

p1 <- df %>% 
  mutate(place = if_else(row_number() == 1, 1.1, 0),
         perc = paste(" ", perc, " ")) %>% 
  ggplot(aes(x = value, y = name, fill = name)) +
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(
    aes(label = perc, hjust = place), size = 3.5, family = "pts"
  ) +
  scale_x_continuous(limits = c(0, 85), 
                     labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_fill_manual(values = c("WFO" = "#d71440", "WFH" = "#96d5d2", 
                               "HYBRID" = "#243842", "WFA" = "#ff6900"))+
  labs(
    title = "<span style='color:#D13223'>GATRA</span>",
    subtitle = "<span style='color:#ff0000'>Tempat Kerja Karyawan dalam Tiga Bulan Terakhir</span> 
    <br>Survei yang dilakukan oleh JakPat menunjukkan hasil tempat kerja responden
    <br>pasca pandemi Covid-19. Selama tiga bulan terakhir, mayoritas responden
    <br> mengaku bekerja di kantor.",
    caption = "Source: Jakpat | Viz: GATRA/Mely", 
    x = NULL,
    y = NULL,
    col = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.3, color = "#DAD9D9"),
        plot.background = element_rect(color = NA, fill = "white"),
        text = element_text(),
        axis.line.x = element_line(color = "black", size = 0.3),
        axis.ticks.x = element_line(color = "black", size = 0.3), 
        axis.ticks.length.x = unit(2, "mm"), 
        axis.title = element_text(family = "pts"),
        # axis.text.y = element_text(),
        axis.text.y = element_text(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.justification = "left",
        legend.text = element_markdown(family = "ebg", size = 8),
        legend.key.size = unit(0.3, "cm"),
        plot.title = element_markdown(family = "pts", face = "bold", size = 30, margin = margin(t=16, b = 3)),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(margin = margin(b = 8),lineheight = 1, linewidth = 5, family = "ebg", size = 12),
        plot.caption = element_text(hjust = 0, size = 8, family = "ebg"),
        plot.caption.position = "plot"
  )

p1
ragg::agg_png(here("last_three_month_4.png"), res = 100, width = 650, height = 433, units = "px")
p1
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
p1









