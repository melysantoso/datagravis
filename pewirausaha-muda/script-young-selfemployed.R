# Visualization of Young Self-Employed through time 
# the data can be download here https://data.oecd.org/entrepreneur/young-self-employed.htm

# Get the Tools On --------------------------------------------------------


library(tidyverse)
library(ggtext)
library(showtext)
library(countrycode)

# Font
font_add_google("Fira Sans Condensed", "fira")
showtext_opts(dpi = 320)
showtext_auto(enable = T)

# Import data
data <-  read_csv("young-self-employed-oecd.csv") %>% janitor::clean_names()



# Wrangle the Data --------------------------------------------------------


selected <- data %>% 
  count(location, sort=T) %>% 
  filter(n>37) %>% 
  pull(location)
data1 <-  data %>% 
  filter(location %in% selected) %>%
  mutate(name = countrycode(location, origin="iso3c", destination="country.name")) 
lab_df <- data1 %>% 
  select(name, subject, time, value) %>% 
  group_by(name, time) %>% 
  summarise(diff=diff(value)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  filter(time==max(time)) %>% 
  mutate(lab=glue::glue("**{name}**<br>??? {round(diff,2)}% in {time}")) %>% 
  arrange(diff) %>% 
  select(name, lab)
lev <- lab_df %>% pull(lab) 


# Make the Plot  ----------------------------------------------------------


data1 %>%  
  left_join(lab_df, by="name") %>% 
  ggplot() +
  geom_point(aes(time, value, color = subject, group = subject), 
             size = .8) +
  scale_color_manual(values = c("#12c7c4", "#bd119d")) +
  scale_y_continuous(limits = c(0,20), 
                     labels = scales::percent_format(scale = 1, accuracy = 1)) +
  facet_wrap(~factor(lab, levels = lev)) +
  labs(title = "**Self-employed kalangan muda <span style='color:#12c7c4'>laki-laki</span> dan <span style='color:#bd119d'>perempuan</span>** (1990 - 2020)", 
       subtitle = "Pangsa wiraswasta berusia 20-29 di antara semua pekerja yang bekerja berusia 20-29 untuk setiap kelompok (laki-laki/perempuan), <br>dari 16 negara terpilih (dengan catatan terbanyak dalam periode waktu tersebut). Disusun dalam urutan menaik dari perbedaan <br>tahun terakhir dengan catatan dari kedua kelompok, yang diberi label di bawah nama negara.", 
       caption = "Data source: data.oecd.org | Visualization by <span style='color:#ff3512'>Mely Santoso</span>") +
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="fira"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(size=.3),
        plot.margin=margin(.5,.75,.5,.5, unit="cm"),
        plot.title.position = "plot",
        plot.title = element_markdown(size= 15),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text=element_markdown(lineheight = 1.2, size=9.5),
        plot.subtitle=element_markdown(size=9, lineheight = 1.2, color="grey20"),
        plot.caption=element_markdown(),
        axis.title=element_blank()
  )

ggsave("self-employed-young.png", width = 8, height = 8)
