library(tidyverse)
library(grid)
library(ggtext)
library(showtext)
library(here)
library(ragg)
library(MetBrewer)

showtext_auto()
showtext_opts(dpi=100)
font_add_google("PT Serif", "pts")

setwd("~/Databank/survey generasi alvara/")
df <- read.csv("kecemasan-generasi.csv")
glimpse(df)

p <- df %>% 
  ggplot(aes(x = generasi, y = persentase, fill = status)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.7)+
  scale_y_continuous(limits = c(0,50), 
                     labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_fill_manual(values = c("Sangat Cemas" = "#a70000", "Cukup Cemas" = "#ff0000", 
                               "Cemas" = "#ff5252", "Tidak Cemas" = "#ffbaba"))+
  labs(
    title = "<span style='color:#D13223'>GATRA</span>",
    subtitle = "<span style='color:#ff0000'>Yang Muda Yang Cemas</span> <br>Survei Alvara Research Center menemukan bahwa Gen Z dan Milenial <br>lebih banyak merasa cemas dibandingkan generasi senior mereka. ",
    caption = "Source: Alvara Research Center | Viz: Mely/Gatra", 
    x = NULL,
    y = NULL,
    col = NULL
  ) +
  theme_minimal(base_family = "pts") +
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
        legend.text = element_markdown(),
        plot.title = element_markdown(face = "bold", size = 26, margin = margin(t=16, b = 3)),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(margin = margin(b = 8), lineheight = 1, size = 12),
        plot.caption = element_text(hjust = 0, size = 10, family = "pts"),
        plot.caption.position = "plot"
  )

p

ragg::agg_png(here("generasi-cemas-6.png"), res = 100, width = 650, height = 433, units = "px")
p
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
p


ggsave("kecemasan-generasi-8.png", width = 8, height = 8)






c("Sangat Cemas" = "#a70000", "Cukup Cemas" = "#ff0000", 
  "Cemas" = "#ff5252", "Tidak Cemas" = "#ffbaba")
