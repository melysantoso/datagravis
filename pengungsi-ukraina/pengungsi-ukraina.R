
# Load packages  ----------------------------------------------------------


library(maps)
library(tidyverse)
library(showtext)
library(mapproj)
library(ggtext)
library(patchwork)

# tambah font menggunakan showtext()

font_add_google("Merriweather", "mrwt")
font_add_google("Roboto", "rbt")
showtext.auto()
showtext.opts(dpi = 320)



# Get and tidy the data ---------------------------------------------------


# load map data dunia menggunakan library maps()
world_map <- map_data("world")

# data for time series analysis - line chart
# data dari UNHCR -diakses pada 5/3/2020
df_times <- read.csv("~/ukraine-refugee/refugee-time.csv") %>% 
  mutate(date = as.Date(data_date, format = "%m/%d/%Y")) 

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



# Plot the data  ----------------------------------------------------------


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
  # tambah judul, subjudul, dan caption - uncoment labs() pertama jika hanya ingin membuat map 
  # uncoment labs() kedua jika ingin membuat gabungan chart 
  #labs(title="LEBIH DARI SATU JUTA PENDUDUK TELAH MENINGGALKAN UKRAINA",
  #     subtitle="Jumlah pengungsi Ukraina di negara tetangga. <br>Grafik tidak termasuk 133 ribu pengungsi di negara-negara Eropa lainnya.",
  #     caption="Data dari UNHCR 4/3/2022 (Data Pengungsi ke Russia dan Moldova 3/3/2022 - Data diambil pada 5/3/2022 9:15 PM WIB) <br> Chart Mely Santoso")+
  labs(title = "<span style='color:#fc4503'>&gt;</span>Sebaran pengungsi dari Ukraina ke negara tetangga",
       subtitle = "Data tidak termasuk 133 ribu pengungsi ke negara Eropa lainnya") +
  theme_void() + 
  theme(text=element_text(family="Gill Sans"),
        plot.title=element_markdown(size=12, family="rbt", margin = margin(t=12, b = 5)),
        plot.subtitle=element_markdown(size=10, family = "rbt", margin = margin(b=5)),
        plot.caption=element_markdown(size=10, family = "rbt", hjust = 0)
  )


# plot line chart 

line_ukr <- df_times %>% ggplot(aes(x = date, y = individuals)) +
  geom_line(colour = "#aa1974", size = 2) +
  geom_point(shape=21, color="#086878" , fill="#086878", size=6) +
  scale_y_continuous(limits = c(50000, 250000),
                     breaks = seq(50000, 200000, 50000), 
                     labels = c("50K", "100K", "150K", "200K")) +
  labs(title = "<span style='color:#fc4503'>&gt;</span>Time Series") +
  theme(plot.title = element_markdown(family = "rbt", size = 12),
        axis.title = element_blank(),
        axis.text = element_text(size = 8, family = "rbt"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

line_ukr



# Join plots in one layout and add title  ---------------------------------


layout <- c(
  area(t = 1, l = 1, b = 2, r = 12
  ),
  area(t = 3, l = 1, b = 8, r = 12
  ))

#plot(layout)

line_ukr + map_ukr +
  plot_layout(design = layout) + 
  plot_annotation(title = 'Lebih dari 1juta orang telah meninggalkan Ukraina',
                  subtitle = "Pengungsi yang datang dari Ukraina ke negara Eropa tetangga <br> 24 Feb - 4 Mar 2022",
                  caption = "Data dari UNHCR 4/3/2022 (Data Pengungsi ke Russia dan Moldova 3/3/2022 - Data diambil pada 5/3/2022 9:15 PM WIB) <br> Chart Mely Santoso)",
                  theme = theme(panel.background = element_rect(fill = "white"),
                                plot.background = element_rect(fill = "white", colour = "white"),
                                plot.title = element_text(size = 22,
                                                          colour = "#f21000", family = "mrwt",
                                                          face = "bold"),
                                plot.subtitle = element_markdown(family = "mrwt", size = 12, 
                                                                 colour = "black",
                                                                 lineheight = 1.5),
                                plot.caption = element_markdown(family = "rbt", size = 10,
                                                                colour = "black", hjust = 0)))



ggsave(paste0("ukrain_refugees_", format(Sys.time(), "%d%m%Y-%H-%M"), ".png"),
       dpi = 320,
       width = 8,
       height = 9)


# sumber data : https://data2.unhcr.org/en/situations/ukraine
# sumber plot : Tanya Shapiro 