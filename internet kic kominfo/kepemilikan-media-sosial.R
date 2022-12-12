
# load library ------------------------------------------------------------


library(tidyverse)
library(grid)
library(ggtext)
library(showtext)
library(here)
library(ragg)


# preparation -------------------------------------------------------------

showtext_auto()
showtext_opts(dpi=100)
font_add_google("PT Serif", "pts")
font_add_google("EB Garamond", "ebg")



# data loading  -----------------------------------------------------------

setwd("~/Databank/internet kic kominfo/")
df <- read_csv("kepemilikan-medsos.csv") 

glimpse(df)




# plot --------------------------------------------------------------------


p <- df %>% 
  ggplot(aes(x = value, y = medos, fill = kepemilikan)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  geom_text(aes(x = value, label = paste0(value, '%')),
            colour = 'white', size = 3, position = position_stack(vjust=0.5)) +
  scale_fill_manual(values = c("Punya" = "#2D7D7E", "Tidak Punya" = "#EC111A")) +
  labs(
    title = "<span style='color:#D13223'>GATRA</span>",
    subtitle = "<span style='color:#EC111A'>Kepemilikian Media Sosial</span>
    <br>Studi Status Literasi Digital 2021 menemukan bahwa aplikasi WhatsApp merupakan
    <br>media sosial yang paling banyak dimiliki oleh masyarakat Indonesia.",
    caption = "Source: KIC & Kominfo | Viz: GATRA/Mely", 
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
        plot.subtitle = element_markdown(margin = margin(b = 8),lineheight = 1, family = "ebg", size = 10),
        plot.caption = element_text(hjust = 0, size = 8, family = "ebg"),
        plot.caption.position = "plot"
  )

p
ragg::agg_png(here("kepemilikan_medsos_2.png"), res = 100, width = 650, height = 433, units = "px")
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
