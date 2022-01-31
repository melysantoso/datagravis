library(tidyverse)
library(maps)
library(showtext)
library(ggtext)
library(ggrepel)
library(tibble)


df <-  read.csv("beef-and-buffalo-meat-consumption-per-person.csv") %>% 
  rename(kg_capita = Food.Balance.Sheets..Bovine.Meat...Food.supply.quantity..kg.capita.yr...FAO..2017..) %>% 
  filter(Year == 2013)

world_map <- map_data("world") %>% 
  filter(region!="Antarctica")


df$Entity[df$Entity == "United States"] <- "USA"
df$Entity[df$Entity == "United Kingdom"] <- "UK"

df$bin<-
  ifelse(df$kg_capita>=40,"+40",
         ifelse(df$kg_capita>=30,"30-39",
                ifelse(df$kg_capita>=20,"20-29",
                       ifelse(df$kg_capita>=15,"15-19",
                              ifelse(df$kg_capita>=10,"10-14",
                                     ifelse(df$kg_capita>=5,"5-9",
                                            "<5"))))))

df$bin<-factor(df$bin, levels=c("<5","5-9","10-14","15-19","20-29","30-39","+40"))

the_df <- left_join(world_map, df, by = c("region"="Entity"))
summary(df$kg_capita)

#fonts
font_add_google("Merriweather", "mrwt")
showtext_auto()
showtext_opts(dpi = 320)

chart_font <- "mrwt"
title_font <- "mrwt"

#text for annotations
text_label <-c ("Konsumsi daging sapi dan kerbau teratas: Argentina (55,5kg) dan Brasil (39,3kg)",
              "Amerika Serikat menduduki peringkat ke-3 konsumsi tertinggi (32,2kg)",
              "Konsumsi terendah: Liberia (0,78kg)",
              "Konsumsi terendah kedua di India (0,81kg). Sekitar 80% populasi menganut agama Hindu (melarang daging sapi)")
text_lat <- c(-50,30,-10,-20)
text_long <- c(-28,-150,-10,75)
text_desc <- c("C1","C2","C3","C4")
text_df <- data.frame(text_label,text_lat,text_long,text_desc)

#points
point_desc <- c("ARG","IND","USA","LIB","BRZ")
point_lat <- c(-36.416,20.593,40,7.4,-14.23)
point_long <-c (-63.616,78.96,-110,-9.4,-51.92)
point_df <- data.frame(point_desc,point_lat,point_long)

#x is long, #y is lat
#arrows to point between annotations and dots
arrows <- tibble(
  x2 = c(-63.616, #Long Arg Point
         78.96, #Long India Point
         -110, #Long USA Point
         -9.4, #Lont Liberia Point
         -51.92 #Long Brazil Point
  ),
  x1 = c(-28,  #Long Arg Comm
         75, #Long India Comm
         -150, #Long USA Comm
         -10, #Long Lib Comm
         -28 #Long Braz Comm
  ),
  y2 = c(-36.416,  #Lat Arg Point
         20.593,  #Lat Ind Point
         40, #Lat USA Point
         7.4, #Lat Lib Point
         -14.23 #Lat Brazil Point
  ),
  y1 = c(-50,  #Lat Arg Comm
         -20, #Lat India Comm
         30, #Lat US Comm
         -10, #Lat Lib Comm
         -50 #Lat Brazil Comm
  ) 
)

#setcolors
#fill colors
colors <- c('#fee5d9', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#99000d')
background <- '#1C1D21'
font_color <- "white"
annotation_color <- "#FF9D00"


ggplot()+
  geom_polygon(data=the_df, aes(x = long, y = lat, group = group, fill = bin),
               color = background, size = 0.2)+
  guides(fill = guide_legend(title.position = "top", title.hjust =0.5,nrow = 1))+
  geom_polygon(data = the_df %>% filter(is.na(kg_capita)),
               aes(x=long,y=lat,group=group), fill="grey", 
               color=background, size=0.2)+
  scale_fill_manual(values=colors, na.translate = F)+
  geom_point(data = point_df,
             inherit.aes = F, aes(x = point_long, y = point_lat), color = '#2E1F27')+
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
               color = annotation_color)+
  geom_textbox(data = text_df %>% filter(text_desc %in% c("C1","C4")),
               aes(x = text_long, y = text_lat,label = text_label), size = 2.5, 
               width = unit(0.1, "npc"), fill = background, color = "white", 
               box.color=annotation_color)+
  geom_textbox(data = text_df %>% filter(text_desc %in% c("C3","C2")),
               aes(x = text_long, y = text_lat, label = text_label), size = 2.5,
               width = unit(0.07, "npc"), fill = background, color="white", 
               box.color=annotation_color)+
  #coord_map(xlim=c(-180,180))+
  labs(title="Tingkat Konsumsi Daging Sapi & Kerbau (2013)",
       subtitle="Tingkat konsumsi berdasarkan kg per kapita",
       fill="Kg Per Capita",
       caption="Data : OurWorldInData.org | Visualisasi @melysantoso") +
  theme_void()+
  theme(plot.background=element_rect(fill="white"),
        legend.position="top",
        text=element_text(color="#1C1D21", family=chart_font),
        plot.title=element_text(family=title_font,hjust=.5, face = "bold",
                            vjust=5),
        plot.subtitle =element_text(hjust=.5, vjust=5),
        plot.margin = unit(c(0.8, 4.2, 0.8, 4.2), "cm"))

ggsave("konsumsi_daging.jpg", width = 34.2, height = 17, units='cm')


