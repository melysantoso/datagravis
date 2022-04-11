
# Load Library ------------------------------------------------------------


library(tidyverse)
library(ggchicklet)
library(hrbrthemes)
library(ggtext)
library(showtext)
library(MetBrewer)

font_add_google("Crete Round", "crete")
font_add_google("Playfair Display", "pd")
showtext_auto()
showtext.opts(dpi=320)


# Data Wrangling ----------------------------------------------------------


df <- read_csv("animal-protein-consumption.csv") %>% 
  rename("Poultry" = "New Food Balances - Poultry Meat - 2734 - Protein supply quantity (g/capita/day) - 674 - g/capita/day", 
         "Pork" = "New Food Balances - Pigmeat - 2733 - Protein supply quantity (g/capita/day) - 674 - g/capita/day", 
         "Beef" = "New Food Balances - Bovine Meat - 2731 - Protein supply quantity (g/capita/day) - 674 - g/capita/day",
         "Lamb & Goat" = "New Food Balances - Mutton & Goat Meat - 2732 - Protein supply quantity (g/capita/day) - 674 - g/capita/day", 
         "Other Meat" = "New Food Balances - Meat, Other - 2735 - Protein supply quantity (g/capita/day) - 674 - g/capita/day",
         "Eggs" = "New Food Balances - Eggs - 2744 - Protein supply quantity (g/capita/day) - 674 - g/capita/day", 
         "Dairy" = "New Food Balances - Milk - Excluding Butter - 2848 - Protein supply quantity (g/capita/day) - 674 - g/capita/day",
         "Fish" = "New Food Balances - Fish, Seafood - 2960 - Protein supply quantity (g/capita/day) - 674 - g/capita/day"
         ) %>% 
  select(-c("Code")) %>% 
  gather(key = type, value = count, -c(Entity, Year)) %>% 
  filter(Year == 2017, 
         Entity %in% c("United States", "United Kingdom", "Spain", 
                       "Brazil", "Japan", "China", "India", "Indonesia", 
                       "Malaysia")) %>% 
  mutate(Entity = fct_reorder(Entity, count, sum, .desc = FALSE)) %>% 
  mutate(type = fct_reorder(type, count, sum, .desc = TRUE))
  


# Plot  -------------------------------------------------------------------


df %>% 
  ggplot(aes(Entity, count, group = type, fill = type)) +
  geom_chicklet(width = 0.75) +
  scale_y_continuous(
    expand = c(0, 0.0625),
    position = "right", 
    breaks = seq(0, 70, 10), 
    labels = c(0, sprintf("%d g", seq(10, 70, 10)))
  ) +
  scale_fill_manual(
    name = NULL, 
    values = met.brewer("Nizami")) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Konsumsi Protein Hewani, 2017", 
       subtitle = "Diukur dari rata-rata persediaan (supply) harian per orang", 
       caption = "Source : Our World in Data | Chart : Mely Santoso") +
  theme_ipsum_rc(grid = "X") +
  theme(plot.title = element_text(family = "crete", size = 26),
        plot.subtitle = element_text(family = "pd"),
        plot.caption = element_text(family = "crete"), 
        axis.text.x = element_text(color = "gray60", size = 10),
        legend.position = "top")

ggsave("animal-protein-3.png", width = 12, height = 10)


# Export Data -------------------------------------------------------------

write.csv(df, "animal-protein-wrangled.csv")







# Alternatif values untuk scale_fill_manual()
# c("Poultry" = "#ae4544",
#  "Beef" = "#d8cb98",
#  "Pork" = "#a4ad6f",
#  "Lamb & Goat" = "#cc7c3a",
#  "Eggs" = "#436f82",
#  "Dairy" = "#7c5981",
#  "Fish" = "#8fbacc",
#  "Other Meat" = "#cccccc")
