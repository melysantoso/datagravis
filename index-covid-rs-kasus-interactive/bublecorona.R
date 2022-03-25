#Trying making bubble charts of covid-19

#load data 
#call library needed
library(ggplot2)
library(dplyr)

#load the data
corona <- read.csv("indexcovidnew.csv", header = T)
View(corona)
ggplot(corona, aes(x = faskes, y = kerentanan, size = positif)) +
  geom_point(alpha=0.7)


#changing the size using scale_size & add color
corona %>% 
  arrange(desc(positif)) %>% 
  mutate(Provinsi = factor(Provinsi, Provinsi)) %>% 
  ggplot(aes(
    x = kerentanan, 
    y = faskes, 
    size = positif, color = Provinsi)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name = "Kasus Positif")

#make it fuckin interactive 
library(hrbrthemes)
library(viridis)

covid <- corona %>% 
  mutate(faskes= round(faskes,0)) %>% 
  mutate(kerentanan=round(kerentanan,0)) %>% 
  
  #reorder prov to having big bubbles on top
  arrange(desc(positif)) %>% 
  mutate(Provinsi = factor(Provinsi, Provinsi)) %>% 
  
  #prepare text for tooltip
  mutate((text = paste("Provinsi: ", Provinsi, 
                       "\nKasus Positif: ", positif, 
                       "\nKondisi Faskes: ", faskes, 
                       "\nIndeks Kerentanan: ", kerentanan, 
                       sep = "" ))) %>% 
  #Classic ggplot
  ggplot( aes(
    x = kerentanan, 
    y = faskes, 
    size = positif, 
    color = Provinsi, 
    text = ))+
    geom_point(alpha=07) +
    scale_size(range = c(1.4, 19), name="Kasus Positif)") +
    scale_color_viridis(discrete = T, guide = F) +
    theme_ipsum()+
    theme(legend.position = "none")

covid

#turn ggplot interactive with plotly 
coronavirus <- ggplotly(covid, tooltip="text")
coronavirus 












