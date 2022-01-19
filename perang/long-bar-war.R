library(tidyverse)
library(ggtext)
library(showtext)
library(here)

font_add_google("Merriweather", "merr")
font_add_google("EB Garamond", "ebg")
font_add_google("Lato", "lato")
font_add_google("Roboto Condensed", "rbtc")
showtext_auto()

war <- read_csv("~/datagravis/perang/battle-related-deaths-in-state-based-conflicts-since-1946-by-world-region.csv") %>% 
  rename(Died = `Battle-related deaths`)

setwd("~/datagravis/perang/")

war %>% 
  ggplot(aes(x = Year, y = Died, fill = reorder(Entity, Died))) +
  geom_bar(position = "stack", alpha = 0.55, stat = "identity", width = 1) +
  geom_segment(aes(x = 1950, y = 560000, xend = 1953, yend = 560000), size = 1) +
  #geom_curve(aes(x = 1952, y = 560000, xend = 1955, yend = 500000)) +
  #geom_text(aes(x = 1950, y = 570000, label = "1950 - 1953"), size = 3, 
  #           hjust = 0, vjust = 0.5, family = "ebg") +
  #geom_text(aes(x = 1955, y = 490000, label= "Perang Korea"), size = 5, 
  #          hjust = 0, vjust = 0.05, family = "ebg", fontface = "plain") +
  geom_segment(aes(x = 1965, y = 310000, xend = 1975, yend = 310000), size = 1) +
  geom_segment(aes(x = 1980, y = 285000, xend = 1988, yend = 285000), size = 1) +
  geom_segment(aes(x = 1979, y = 250000, xend = 1989, yend = 250000), size = 1) +
  geom_segment(aes(x = 2011, y = 120000, xend = 2016.9, yend = 120000), size = 1) +
  annotate(geom = "richtext", label = "<span style='color:#fc3903;'>Jumlah kematian akibat perang menurun sejak 1945</span>", 
           x = 1960, y = 600000, size = 5, hjust = 0, vjust = 1, 
           fill = "#fffafa", label.color = NA, family = "merr", fontface = "bold") +
  annotate(geom = "richtext", label = "Jumlah mutlak kematian perang telah menurun sejak 1946. <br>Dalam beberapa tahun di awal era pasca perang, sekitar setengah <br>juta orang tewas melalui kekerasan langsung dalam perang; <br>sebaliknya, pada tahun 2016 jumlah semua kematian terkait <br>pertempuran dalam konflik yang melibatkan setidaknya satu negara <br>adalah 87.432.",
           x = 1960, y = 580000, size = 5.5, hjust = 0, vjust = 1, 
           fill = "#fffafa", label.color = NA, lineheight = 1.2, family = "ebg") +
  annotate(geom = "richtext", label = "Penurunan jumlah absolut kematian pertempuran dapat dilihat <br>pada visualisasi di sini yang menunjukkan kematian pertempuran <br>global per tahun berdasarkan wilayah dunia. Ada tiga puncak <br>yang ditandai dalam kematian perang sejak saat itu: <br>Perang Korea (awal 1950-an), Perang Vietnam (sekitar 1970), dan <br>perang Iran-Irak dan Afghanistan (1980-an). <br>Baru-baru ini terjadi peningkatan kematian akibat pertempuran <br>yang didorong oleh konflik di Timur Tengah, <br>khususnya di Suriah, Irak dan Afghanistan.",
           x = 1960, y = 490000, size = 5.5, hjust = 0, vjust = 1, 
           fill = "#fffafa", label.color = NA, lineheight = 1.2, family = "ebg") +
  #annotate(geom = "richtext", label = "Hanya konflik di mana setidaknya satu pihak adalah pemerintah negara <br>dan yang menghasilkan lebih dari 25 kematian terkait pertempuran. <br>Data hanya mengacu pada kematian akibat kekerasan langsung <br>(tidak termasuk wabah penyakit atau kelaparan).",
  #         x = 1993, y = 250000, size = 4.5, hjust = 0, vjust = 1, 
  #         fill = NA, label.color = NA, lineheight = 1.2, family = "ebg") + 
  annotate(geom = "richtext", x = 1950, y = 570000, label = "Perang Korea", 
           family = "lato", size = 3.5, fill = NA, label.color = NA, 
           hjust = 0.1, vjust = 1) +
  annotate(geom = "richtext", x = 1970, y = 320000, label = "Perang Amerika Vietnam", 
           family = "lato", size = 3.5, fill = NA, label.color = NA, 
           hjust = 0.5, vjust = 1) +
  annotate(geom = "richtext", x = 1983, y = 285000, label = "Perang Iran-Iraq", 
           family = "lato", size = 3.5, fill = NA, label.color = NA, 
           hjust = 0.3, vjust = 0) +
  annotate(geom = "richtext", x = 1983, y = 250000, label = "Perang Soviet Afghanistan",
           family = "lato", size = 3.5, fill = NA, label.color = NA, 
           hjust = 0.4, vjust = 0) +
  annotate(geom = "richtext", x = 2013, y = 120000, label = "Perang Suriah", 
           family = "lato", size = 3.5, fill = NA, label.color = NA, 
           hjust = 0.5, vjust = 0) +
  scale_fill_manual(values=c("#3B9AB2", "#354823", "#FAD510", "#0d3559", "#ff0505")) +
  scale_x_continuous(
    expand = c(0.01, 0.3),
    limits = c(1945, 2017),
    breaks = c(1946, 1953, 1960, 1970, 1980, 1990, 2000, 2010, 2016)) +
  scale_y_continuous(limits = c(0, 600000),
                     breaks = seq(0, 500000, 100000),
                     labels = c("0", "100000", "200000", "300000", "400000", "500000")) +
  labs(title = "No Man's Land",
       subtitle = "Jumlah korban meninggal perang dari 1946-2016 dikelompokkan <br>berdasar wilayah: <span style='color:#3B9AB2;'>**Amerika**</span>, <span style='color:#354823;'>**Eropa**</span>, <span style='color:#FAD510;'>**Timur Tengah**</span>, <span style='color:#0d3559;'>**Afrika**</span>, serta <br> <span style='color:#ff0505;'>**Asia dan Oceania**</span>.",
       caption = "<span style='color:#525252'>Data: Our World in Data <br> Chart: @melysantoso</span>") +
  theme(
    plot.title = element_markdown(family = "merr", size = 34),
    plot.subtitle = element_markdown(family = "lato", size = 18, lineheight = 1.2, linewidth = 2),
    plot.caption = element_markdown(family = "lato", size = 12, hjust = 0),
    plot.background = element_rect(fill = "#fffafa"),
    #plot.background = element_rect(fill = "#f8f7ff"),
    #plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e3e3e3", size = 0.05),
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #axis.text=element_text(family = "rbtc", face = "bold", size=12),
    axis.text.x = element_text(family = "lato", face = "bold", size = 16),
    axis.text.y = element_text(family = "lato", face = "bold", size = 13)
  )

ggsave("long-bar-war.png", dpi = 100, height = 17, width = 8) 

