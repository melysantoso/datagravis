
# load library ------------------------------------------------------------


library(tidyverse)
library(grid)
library(ggtext)
library(showtext)
library(here)
library(MetBrewer)
library(ragg)


# preparation -------------------------------------------------------------

showtext_auto()
showtext_opts(dpi=100)
font_add_google("PT Serif", "pts")
font_add_google("EB Garamond", "ebg")



# data loading  -----------------------------------------------------------

setwd("~/Databank/internet kic kominfo/")
df <- read_csv("medsos-paling-sering.csv") %>%
  gather(key = type, value = count, -c("nama_alias"))



# plot --------------------------------------------------------------------

## color
# blue - #2D7D7E
# light blue - #7ECCCC
# pink - #EB949E
# red - #E04555


p <- df %>% 
  ggplot(aes(x = count, y = nama_alias, fill = fct_reorder(type, desc(count)))) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(x = count, label = paste0(count, '%')),
            colour = 'white', size = 3, position = position_stack(vjust=0.5)) +
  scale_fill_manual(values = c("> 8 jam/hari" = "#E04555", "5-8 jam/hari" = "#EC7C71", 
                               "2-5 jam/hari" = "#EB949E", "< 2 jam/hari" = "#2D7D7E")) +
  labs(
    title = "<span style='color:#D13223'>GATRA</span>",
    subtitle = "<span style='color:#EC111A'>Media Sosial Yang Paling Sering Digunakan</span>. 
    <br>Survei Literasi Digital 2021 menemukan bahwa Whatsapp merupakan media sosial 
    <br>yang paling sering diakses dan digunakan masyarakat Indonesia.",
    caption = "Source: KIC & Kominfo | Viz: GATRA/Mely", 
    x = NULL,
    y = NULL,
    col = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.3, colour = "#DAD9D9"), 
        plot.background = element_rect(color = NA, fill = "white"),
        axis.line.x = element_line(color = "black", size = 0.3),
        axis.ticks.x = element_line(color = "black", size = 0.3), 
        axis.ticks.length.x = unit(2, "mm"), 
        axis.title = element_text(family = "pts"),
        axis.text.y = element_text(),
        plot.title = element_markdown(family = "pts", face = "bold", size = 30, margin = margin(t=16, b = 3)), 
        plot.title.position = "plot",
        plot.subtitle = element_markdown(margin = margin(b = 8),lineheight = 1, family = "ebg", size = 11),
        plot.caption = element_text(hjust = 0, size = 8, family = "ebg"),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(family = "ebg", size = 8),
        legend.key.size = unit(0.3, "cm")
        
        )



# save the plot  ----------------------------------------------------------


p
ragg::agg_png(here("medsos_sering_dipake_5.png"), res = 100, width = 650, height = 433, units = "px")
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
