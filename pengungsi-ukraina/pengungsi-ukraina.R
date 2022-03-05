library(maps)
library(tidyverse)
library(showtext)
library(mapproj)
library(ggtext)

# tambah font menggunakan showtext()

font_add_google("Merriweather", "mrwt")
font_add_google("Roboto", "rbt")
showtext.auto()
showtext.opts(dpi = 320)

# load map data dunia menggunakan library maps()
world_map <- map_data("world")

# buat tibble untuk countries dan refugees 
countries <- c("Ukraine", "Poland", "Hungary", "Moldova", "Slovakia", "Romania", "Russia", "Belarus")
refugees <- c(0, 756303, 157004, 103254, 101529, 63192, 53300, 406)

# jadikan data frame
data<-data.frame(countries,refugees)

# menentukan warna: Ukraina : Magenta, Negara tetangga : Blue Jeans, lainnya orange pudar 
world_map <- world_map%>%
  mutate(fill = case_when(region == "Ukraine"~"#aa1974",
                        region %in% countries ~ "#086878",
                        TRUE ~ "#fff2e8"
  ))

# membuat data frame untuk geom_point()
point_data <- world_map%>%
  group_by(region)%>%
  # membuat centeroids 
  filter(region %in% countries & region != "Ukraine")%>%
  summarise(lat = mean(lat),
            long = mean(long))%>%
  # join atau menambahkan join left_join()
  left_join(data, by=c("region"="countries"))%>%
  mutate(
    #override Russia centroid, map cut-off
    lat = replace(lat, region=="Russia", 52),
    long = replace(long, region=="Russia",39),
    #include ukranian label coordinates as starting point for migration lines
    lat_ukr = 49.25, 
    long_ukr = 31.5,
    #add vjust parameters to adjust text data in plots
    region_vjust = case_when(refugees>60000 ~ -.8, TRUE ~ -4),
    stat_vjust = case_when(refugees>60000 ~1.1, TRUE~0.1),
    name_color = case_when(refugees>60000 ~ '#ffffff', TRUE ~"white")
  )

#Ukraine label
text_data<-data.frame(
  text= "UKRAINE", 
  lat = 49.25, 
  long = 31.5
)

# tambah informasi kota 
data(world.cities)

# filter untuk kota di ukraina dan ambil 5 lalu arrange dari - populasi 
ukr_cities <- world.cities %>% 
  filter(country.etc =='Ukraine') %>% 
  arrange(-pop) %>% 
  head(5)


# plot data 
ggplot(world_map, aes(long, lat, group=group))+
  geom_polygon(aes(fill=fill), color="black", size=0.3)+
  # jalur pengungsi - garis dash
  geom_segment(inherit.aes=FALSE, data=point_data, 
               aes(x=long_ukr, xend=long, y=lat_ukr, yend=lat), 
               size=0.5, linetype="longdash", color='white')+
  # membuat batasan peta ukraina menjadi putih 
  geom_polygon(data = world_map%>%filter(region=="Ukraine"), 
               aes(fill=fill), color="white", size=0.3)+
  # plot balon untuk pengungsi 
  geom_point(inherit.aes=FALSE, data = point_data, 
             aes(long, lat, size=refugees/1000), 
             fill='#b33960', color='#b33960', shape=21)+
  # setting aes pengungsi 
  geom_text(inherit.aes=FALSE, data=point_data, 
            aes(long, lat,label=scales::comma(refugees), vjust=stat_vjust),
            size=3.5, family="rbt", color='#ffffff')+
  # nama kota 
  geom_text(inherit.aes=FALSE, data= point_data, 
            aes(long, lat, label=toupper(region), 
                vjust=region_vjust, color=name_color), 
            size=4, family="mrwt", fontface = "bold")+
  # Label ukraina 
  geom_text(inherit.aes=FALSE, data=text_data, 
            aes(long, lat, label=text), 
            size=10, family="mrwt", color='#ffffff')+
  # 5 kota di ukraina yang telah di-filter (point dan nama/label)
  geom_point(inherit.aes=FALSE, 
             data=ukr_cities, 
             aes(long,lat), 
             color='black')+
  geom_text(inherit.aes=FALSE, data=ukr_cities, 
            aes(long,lat,label=name), 
            size=4, vjust=1.7, family="mrwt", 
            color='#fce6e6')+
  # agar warna match 
  scale_fill_identity()+
  scale_color_identity()+
  scale_size(range=c(9,37), breaks=c(100,150, 200, 250,500), guide="none")+
  # setting untuk memunculkan seberapa luas peta akan ditampilkan 
  coord_map(xlim=c(14,40),
            ylim=c(43,55))+
  # tambah judul, subjudul, dan caption 
  labs(title="LEBIH DARI SATU JUTA PENDUDUK TELAH MENINGGALKAN UKRAINA",
       subtitle="Jumlah pengungsi Ukraina di negara tetangga. <br>Grafik tidak termasuk 110 ribu pengungsi di negara-negara Eropa lainnya.",
       caption="Data dari UNHCR 4/3/2022  (Data Pengungsi ke Russia dan Moldova 3/3/2022 - Data diambil pada 5/3/2022 9:15 PM WIB) <br> Chart Mely Santoso")+
  theme_void()+
  theme(text=element_text(family="Gill Sans"),
        plot.title=element_text(size=18, family="mrwt", face = "bold", hjust = 0.5),
        plot.subtitle=element_markdown(size=16, family = "mrwt", hjust = 0.5),
        plot.caption=element_markdown(size=12, family = "rbt", hjust = 0)
  )

ggsave("pengungsi-ukraina-4.png", height=10, width=12)


# sumber data : https://data2.unhcr.org/en/situations/ukraine
# sumber plot : Tanya Shapiro 