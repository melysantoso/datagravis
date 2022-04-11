library(tidyverse)
library(here)
library(glue)
library(ggtext)
library(jsonlite)
library(lubridate)
library(magick)

chunk_ids <- 0:2
api_urls <- glue(
  "https://haexpeditions.com/wp-admin/admin-ajax.php?action=wp_ajax_ninja_tables_public_action&table_id=1084&target_action=get-all-data&default_sorting=old_first&skip_rows=0&limit_rows=0&chunk_number={chunk_ids}&ninja_table_public_nonce=4421ea962c"
)

# Pull the Data
climbers_raw <- map_dfr(api_urls, read_json, simplify = TRUE)
write_rds(climbers_raw, here("mtverest_climbers_raw.rds"))
str(climbers_raw)

# Prepare the Data 
climbers <- tibble(climbers_raw$value) %>% 
  mutate(date = mdy(date), 
         year = year(date), 
         gender = factor(gender)) %>% 
  select(-"___id___")

climbers %>% 
  count(year) %>% 
  arrange(desc(year))

climbers %>% 
  ggplot(aes(year)) +
  geom_bar() +
  scale_y_continuous(position = "right") +
  theme_minimal()
ggsave(here("mt-everest-bar.png"), width = 6, height = 5)

climbers %>% 
  ggplot(aes(year)) +
  stat_count(geom = "area")


#' Image credits:
#' https://commons.wikimedia.org/wiki/File:Mount_Everest_as_seen_from_Drukair2.jpg
#' by shrimpo1967
img_everest <- image_read(here("800px-Mount_Everest_as_seen_from_Drukair2.jpg"))
img_everest <- image_scale(img_everest, "x1200")
img_everest_info <- image_info(img_everest)

img_everest_edited <- image_modulate(img_everest, saturation = 25, brightness = 60) 


annotations <- tibble(
  year = c(1953, 1978, 1996, 2000, 2014, 2020), 
  y = c(100, 200, 500, 400, 500, 300), 
  label = c(
    "<b>1953</b> | Everest pertama kali ditaklukkan",
    "<b>1978</b> | R. Messer & P. Habeler <br>merupakan para pendaki <br>pertama tanpa supplementary <br>oxygen",
    "<b>1996</b> | Tahun terburuk dalam <br>sejarah pendakian Everst",
    "Pendakian Everest<br>semakin popular",
    "<b>2014/15</b> | Karena avalanche<br> yang menelan banyak korban,<br>
    tidak ada pendakian selama 2015",
    "Pandemi<br>Covid-19")
  )


annotations <- climbers %>% 
  count(year, name = "yend") %>% 
  right_join(annotations) %>% 
  mutate(yend = yend + 5, 
         y = y - 1)

img_plot <- image_graph(res = 300, width = img_everest_info$width,
                        height = img_everest_info$height, bg = "transparent")

climbers %>% 
  ggplot(aes(year)) +
  geom_bar(fill = "white", color = "grey50", size = 0.05) +
  geom_segment(data = annotations, aes(x = year, xend = year, y = y, yend = yend), 
               inherit.aes = FALSE, col = alpha("white", 0.9), size = 0.2) +
  geom_richtext(data = annotations, aes(x = year - 1, y, label = label), 
                inherit.aes = FALSE, fill = alpha("white", 0.9), color = "grey4", label.color = NA, 
                family = "PT Serif", size = 2, hjust = 0, vjust = 0, 
                label.size = 0, label.r = unit(0, "mm")) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(NA, 2025)) +
  scale_y_continuous(position = "right", limits = c(0, 600), breaks = seq(0, 500, 100)) +
  labs(
    title = "Ego Manusia Menaklukan Everest",
    subtitle = "6.014 orang yang berbeda telah mendaki Gunung Everest sejak
     Tenzing Norgay dan Edmund Hillary mencapai puncaknya pada tahun 1953.
     Beberapa, seperti Nawang Gombu dan Reinhold Messner, bahkan mendaki Gunung Everest dua kali.
     Grafik menghitung setiap pendaki hanya sekali dan dengan tanggal pendakian pertama.",
    caption = "**Source:** The Himalayan Database, haexpeditions.com, CNN Indonesia. **Image credit:** shrimpo1967, Wikipedia.
    **Visualization:** Mely Santoso"
  ) + 
  theme_void(base_family = "PT Serif", base_size = 8) +
  theme(
    text = element_text(color = "white"),
    axis.text = element_text(),
    axis.text.y = element_text(
      vjust = 0.2, hjust = 0, margin = margin(l = 3), size = 6),
    axis.line.x = element_line(color = "white"),
    axis.ticks.y.right = element_line(color = "white"),
    axis.ticks.length.y.right = unit(2, "mm"),
    plot.margin = margin(3, 10, 10, 10),
    plot.title = element_markdown(
      size = 20, fill = alpha("#f50000", 0.9), face = "bold", 
      padding = margin(t = 2, b = 2, l = 6, r = 6)),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 6, b = 24), width = unit(0.6, "npc"), hjust = 0
    ),
    plot.caption = element_markdown(
      hjust = 1, fill = alpha("grey10", 0.9), margin = margin(t = 8))
  )
  

img_combined <- image_composite(img_everest_edited, img_plot)
image_write(img_combined, here("mt-everest-9.png"))
  
  
  
  
  
  



