library(tidyverse)
library(maps)
library(ggtext)
library(showtext)
library(ggrepel)
library(tibble)
library(patchwork)


# Preparation -------------------------------------------------------------

# font

font_add("Lemon Milk", "/Windows/Fonts/LEMONMILK-Regular.otf")
text_font <- "Lemon Milk"
font_add_google("Merriweather", "mrwt")
sub_cap_font <- "mrwt"

showtext_auto()
showtext_opts(dpi = 320)

# color 
colors <- c(
  "#ffffcc",
  "#c7e9b4",
  "#7fcdbb",
  "#41b6c4",
  "#2c7fb8",
  "#253494"
)

background_col <- "black"
font_color <- "white"



# Data Wrangling  ---------------------------------------------------------


df <- read.csv("~/datagravis/fish-overfishing/fish-seafood-production.csv") %>% 
  rename(fish_tonnes = New.Food.Balances...Fish..Seafood...2960...Production...5511...1000.tonnes) %>% 
  filter(Year == 2017)

# world map 

w_map <- map_data("world") %>% 
  filter(region!="Antarctica")

# some country data are missing because of different label antara data OWID dan map_data("world)
# jadi data dari OWID disesuaikan dengan data map_data("world)

df$Entity[df$Entity == "United States"] <- "USA"
df$Entity[df$Entity == "United Kingdom"] <- "UK"
df$Entity[df$Entity == "Congo"] <- "Republic of Congo"
df$Entity[df$Entity == "Czechia"] <- "Czech Republic"

df$bin <- 
  ifelse(df$fish_tonnes>=50000000, "+50 Juta",
         ifelse(df$fish_tonnes>=11000000, "11-49 Juta",
                ifelse(df$fish_tonnes>=1000000, "1-10 Juta",
                       ifelse(df$fish_tonnes>=500000, "500k-900K",
                              ifelse(df$fish_tonnes>=100000, "100K-400K",
                                     ifelse(df$fish_tonnes>=0, "0-99K"))))))
df$bin <- factor(df$bin, levels = c("0-99K", "100K-400K", "500k-900K", "1-10 Juta", "11-49 Juta", "+50 Juta"))

#join the df and w_map
world_map_df <- left_join(w_map, df, by = c("region"="Entity"))



# Plot the Data  ----------------------------------------------------------

# Sebelumnya hanya ingin membuat choropleth, tapi seketika ada ide untuk gabungin dua chart -choropleth dan col chart
# Jadi kode ini agak berantakan 

map_fish <- ggplot(world_map_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = bin), size = 0.2, color = background_col) +
  guides(fill = 
           guide_legend(title.position = "top",
                        title.hjust =0.5,
                        nrow = 1))+
  geom_polygon(
    data = world_map_df %>% filter(is.na(fish_tonnes)), 
    aes(x = long, y = lat, group = group), 
    fill = "#eeeeee", color = background_col, size = 0.2
  ) +
  scale_fill_manual(values = colors, na.translate = F) +
  labs(#title = "Peta Distribusi Penghasil Seafood, 2017", 
  #     subtitle = "Produksi ikan dan makanan laut diukur sebagai jumlah makanan laut <br>dari tangkapan liar dan budidaya ikan (akuakultur).",
       fill = "dalam ton"
  #     caption = "Sumber: Food and Agriculture Organization of the United Nations (via OWID) <br> Visualisasi: @melysantoso"
       ) +
  theme_void() +
  theme(plot.background=element_rect(fill=background_col),
        legend.position=c(0.55, 0.05),
        legend.title = element_text(family = sub_cap_font, color = "black"),
        text=element_text(color=font_color, family=text_font),
        plot.title=element_text(family=sub_cap_font,hjust=.5, vjust=5),
        #plot.subtitle =element_markdown(hjust=.5, vjust=5, family = sub_cap_font),
        #plot.caption = element_markdown(hjust = 0, vjust = 0.3, family = sub_cap_font),
        plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

map_fish

ggsave("fish-production-10.png", width = 34.2, height = 20, units='cm')



# Bar Chart ---------------------------------------------------------------


# Data Manipulation  ------------------------------------------------------



df_col <- df %>% 
  arrange(desc(fish_tonnes)) %>% # urutkan dari yang paling tinggi
  head(15) # ambil 15 negara teratas saja 


# uncoment this dan don't run code di atas jika ingin filter manual dan memilih negara sesuka hati  
# df_col <- df %>% 
#  arrange(desc(fish_tonnes)) %>% 
#  filter(Entity %in% c("China", "Indonesia", "India", "Vietnam", "USA", "Russia", "Japan",
#                    "Myanmar", "Thailand", "Malaysia", "Brazil", "Cambodia", "Argentina", "UK",
#                    "Denmark", "France", "South Africa", "Namibia", "Senegal", "Netherlands", 
#                    "Italy", "Germany", "Algeria", "Republic of Congo", "Croatia", "United Arab Emirates"))


# factor inorder entity untuk ngontrol bar chart nanti 
# kalau ini ga dipake, nanti col chartnya amburadul 
df_col <- df_col %>% 
  mutate(Entity = forcats::fct_rev(forcats::fct_inorder(Entity)))

# membuat label untuk col chart jadi tidak usah pake axis title dan ticks
df_col <- df_col %>% 
  dplyr::mutate(
    ton = paste(fish_tonnes, "Ton"),
    ton = if_else(row_number() == 1, paste(ton), ton)
  )


# kasih warna 
df_col <- df_col %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "#253494",
      row_number() %in% c(2, 3) ~ "#2c7fb8",
      TRUE ~ "#41b6c4"
    )
  )

# membuat warna negara pilihan 

#df_col <- df_col %>% 
#  mutate(
#    color = case_when(
#      Entity == "China" ~ "#253494",
#      Entity %in% c("Indonesia", "India") ~ "#2c7fb8",
#      Entity %in% c("Vietnam", "USA", "Russia", "Japan", "Myanmar", "Thailand", "Malaysia", "Brazil") ~ "#41b6c4",
#      Entity %in% c("Cambodia", "Argentina", "UK", "Denmark", "France", "South Africa") ~ "#7fcdbb",
#      Entity %in% c("Namibia", "Senegal", "Netherlands", "Italy", "Germany") ~ "#c7e9b4",
#      TRUE ~ "#ffffcc"
#    )
#  )

# agar posisi label row number 1 di dalam colom dan yang lainnya di luar 
df_col <- df_col %>% 
  mutate(
    place = if_else(row_number() == 1, 1, 0), 
    ton = paste(" ", ton, " ")
  )


# Plot the Data -----------------------------------------------------------

# Combine the plot 

ggplot(data = df_col, aes(x = fish_tonnes, y = Entity, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = ton, hjust = place),
    size = 3, family = text_font, color = "white"
  ) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  labs(title = "15 Negara dengan Produksi Seafood Terbesar di Dunia, 2017", 
       subtitle = "Produksi seafood dan ikan merupakan jumlah dari tangkapan liar dan budidaya ikan (akuakultur). <br>
       <span style='color:#253494;'>China</span> merupakan negara produsen seafood terbesar di dunia yang memproduksi 62 juta ton seafood pada 2017. <br> 
       <span style='color:#2c7fb8;'>Indonesia</span> berada di peringkat ke-dua disusul oleh <span style='color:#2c7fb8;'>India</span>, <span style='color:#41b6c4;'>Vietnam, USA, Russia</span>.",
       caption = "Source: Food and Agriculture Organization of the United Nation (via OWID) | Visualisasi: @melysantoso") +
  theme_void() +
  theme(plot.title = element_text(family = text_font, face = "bold", size = 20, vjust = 1, color = font_color),
        plot.subtitle = element_markdown(color = 'white', size = 12, vjust = 1, family = sub_cap_font),
        plot.caption = element_text(color = font_color, family = sub_cap_font, size = 8, hjust = 0.5, vjust = 0),
        plot.background = element_rect(fill = "black"),
        axis.text.y = element_text(size = 14, hjust = 1, family = "Trebuchet MS", colour = "white"),
        plot.margin = margin(rep(15, 4))) +
  inset_element(map_fish, left = 0.31, right = 1, bottom = 0.1, top = 0.87)



# Save the Plot  ----------------------------------------------------------

ggsave("bar-chart-10.png", width = 36, height = 19, units='cm')


