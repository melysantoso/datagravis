library(tidyverse)
library(ggtext)
library(showtext)
library(ggalluvial)
library(here)

font_add_google("Merriweather", "merr")
font_add_google("EB Garamond", "ebg")
font_add_google("Lato", "lato")
font_add_google("Roboto Condensed", "rbtc")
showtext_auto()

war_pr <- read_csv(here("./battle-related-deaths-in-state-based-conflicts-since-1946.csv")) %>% 
  rename(Jumlah = `Violent deaths in conflicts and one-sided violence`)

war1 <- war_pr %>% 
  filter(!(Entity %in% c("One-sided violence", "Non-state conflicts", "State-based conflicts")))

war_pr %>% 
  ggplot(aes(x = Year, y = Jumlah, fill = reorder(Entity, Jumlah))) +
  geom_bar(position = "stack", stat = "identity") +
  theme_bw()

war1 %>% 
  ggplot(aes(x = Year, y = Jumlah, alluvium = reorder(Entity, Jumlah))) +
  geom_alluvium(aes(fill = reorder(Entity, Jumlah), color = reorder(Entity, Jumlah), alpha = 0.75)) +
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
           fill = "#fafffb", label.color = NA, family = "merr", fontface = "bold") +
  annotate(geom = "richtext", label = "Jumlah mutlak kematian perang telah menurun sejak 1946. <br>Dalam beberapa tahun di awal era pasca perang, sekitar setengah <br>juta orang tewas melalui kekerasan langsung dalam perang; <br>sebaliknya, pada tahun 2016 jumlah semua kematian terkait <br>pertempuran dalam konflik yang melibatkan setidaknya satu negara <br>adalah 87.432.",
           x = 1960, y = 580000, size = 5.5, hjust = 0, vjust = 1, 
           fill = "#fafffb", label.color = NA, lineheight = 1.2, family = "ebg") +
  annotate(geom = "richtext", label = "Penurunan jumlah absolut kematian pertempuran dapat dilihat <br>pada visualisasi di sini yang menunjukkan kematian pertempuran <br>global per tahun berdasarkan wilayah dunia. Ada tiga puncak <br>yang ditandai dalam kematian perang sejak saat itu: <br>Perang Korea (awal 1950-an), Perang Vietnam (sekitar 1970), dan <br>perang Iran-Irak dan Afghanistan (1980-an). <br>Baru-baru ini terjadi peningkatan kematian akibat pertempuran <br>yang didorong oleh konflik di Timur Tengah, <br>khususnya di Suriah, Irak dan Afghanistan.",
           x = 1960, y = 490000, size = 5.5, hjust = 0, vjust = 1, 
           fill = "#fafffb", label.color = NA, lineheight = 1.2, family = "ebg") +
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
  scale_fill_manual(values=c("#3B9AB2", "#354823", "#FAD510", "#0d3559")) +
  scale_color_manual(values=c("#3B9AB2", "#354823", "#FAD510", "#0d3559")) +
  scale_x_continuous(
    expand = c(0.01, 0.3),
    limits = c(1945, 2017),
    breaks = c(1946, 1953, 1960, 1970, 1980, 1990, 2000, 2010, 2016)) +
  scale_y_continuous(limits = c(0, 600000),
                     breaks = seq(0, 500000, 100000),
                     labels = c("0", "100000", "200000", "300000", "400000", "500000")) +
  labs(title = "No Man's Land",
       subtitle = "Jumlah korban meninggal perang dari 1946-2016 dikelompokkan <br>jenis konflik: <span style='color:#3B9AB2;'>**Konflik kolonial atau kekaisaran**</span>, <span style='color:#354823;'>**Konflik sipil dengan <br>intervensi negara asing**</span>, <span style='color:#FAD510;'>**Konflik sipil**</span>, <span style='color:#0d3559;'>**Konflik antar negara**</span>.",
       caption = "<span style='color:#525252'>visualization by Mely Santoso | Data from Our World in Data") +
  theme(
    plot.title = element_markdown(family = "merr", size = 34),
    plot.subtitle = element_markdown(family = "lato", size = 16, lineheight = 1.2, linewidth = 2.5),
    plot.caption = element_markdown(family = "lato", size = 12, hjust = 0.5),
    plot.background = element_rect(fill = "#fafffb"),
    #plot.background = element_rect(fill = "#f8f7ff"),
    #plot.background = element_blank(),
    #plot.background = element_rect(fill = "#f2f7ff"),
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

ggsave("alluvium-war-type.png", dpi = 100, height = 16, width = 9) 
