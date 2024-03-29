# start up ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(showtext)
library(ggtext)
library(glue)
library(colorspace)
library(snakecase)
library(ggbump)
library(geofacet)
library(crayon)
library(magick)
library(ggrepel)
library(countrycode)

# rich wrap ---------------------------------------------------------------

str_rich_wrap <- function(x, n) {
  parts <- str_extract_all(x, "<.*?>")[[1]]
  x1 <- str_replace_all(x, "<.*?>", "XZ")
  x1 <- str_wrap(x1, n)
  for(k in parts) {
    x1 <- str_replace(x1, "XZ", k)
  }
  str_replace_all(x1, "\\n", "<br>")
}

# palettes ----------------------------------------------------------------

honey <- c("#E3D7C1", "#C8B188", "#C4952E", "#BE7C22", "#93500C")
forest <- c("#445544", "#779977", "#99AA77", "#CCCC88", "#BBBB88")
good_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")
bad_pal <- rev(c("#ef6351", "#f38375", "#f7a399", "#fbc3bc", "#ffe3e0", "white"))
bright <- c("#540d6e", "#ee4266", "#ffd23f", "#3bceac")
dark <- "#181510"
light <- "#FFF6EC"
lakes <- c("#788FCE", "#e07a5f", "#f2cc8f", "#81b29a", "#f4f1de")
bg_lakes <- "#3d405b"
spec <- c("#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012")

# fonts -------------------------------------------------------------------

font_add_google("Abel", "abel")
font_add_google("Barlow Condensed", "barlow")
font_add_google("Oswald", "oswald")
font_add_google("Secular One", "sec")
font_add_google("Patrick Hand", "hand")
font_add_google("Palette Mosaic", "pm")
font_add_google("Crete Round", "crete")
showtext_auto()

# time log ----------------------------------------------------------------

time_log <- function(df, ...) {
  write_lines(Sys.time(), file = log_file, append = TRUE)
  df
}

get_time <- function(.duration = 5, ...) {
  
  stamp <- str_replace(log_file, ".txt", "-posted.txt")
  if(file.exists(stamp)) {
    posted <- read_table(str_replace(log_file, ".txt", "-posted.txt"), col_names = c("date", "time"), col_types = cols())
    posted <- ymd_hms(paste(posted$date, posted$time))
    
    cat(cyan(glue("\n\nPosted at {posted}\n\n\n")))
    
  } else {
    posted <- ymd_hms("2999-01-01 00:00:00")
  }
  
  read_table(log_file, col_names = c("date", "time"), col_types = cols()) %>% 
    mutate(date_time = ymd_hms(paste(date, time))) %>% 
    filter(date_time <= posted) %>% 
    mutate(
      duration = difftime(date_time, lag(date_time), units = "mins"),
      duration = replace_na(duration, 0),
      duration = pmin(duration, .duration)
    ) %>% 
    summarise(total = sum(duration)) %>% 
    mutate(hours = glue("{floor(total/60)}h{round(60*(total/60 - floor(total/60)))}m"))
}

posted <- function(...) {
  stamp <- str_replace(log_file, ".txt", "-posted.txt")
  write_lines(Sys.time(), file = stamp, append = TRUE)
}

# min max -----------------------------------------------------------------

min_max <- function(x, a, b) {
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a
}


# get icon ----------------------------------------------------------------

get_icon <- function(name, size = 50, fill = NULL) {
  img <- glue('C:/Users/User/Documents/tidytuesday/news-orgs/{name}.png')
  
  if(!is.null(fill)) {
    
    if(name == "github") {
      pars <- list(
        bg = list(col = fill$bg, point = "+1+1"),
        img = list(col = fill$img, point = "+900+200")
      )
    }
    
    if(name == "twitter") {
      pars <- list(
        bg = list(col = fill$bg, point = "+1+1"),
        img = list(col = fill$img, point = "+1200+200")
      )
    }
    
    if(name == "lightning-bolt") {
      pars <- list(
        bg = list(col = fill$bg, point = "+1+1"),
        bg2 = list(col = fill$bg2, point = "+1400+100"),
        img = list(col = fill$img, point = "+700+900")
      )
    }
    
    x <- image_read(img)
    if(!is.null(fill$bg)) x <- image_fill(x, color = pars$bg$col, point = pars$bg$point, fuzz = 0)
    if(!is.null(fill$bg2)) x <- image_fill(x, color = pars$bg2$col, point = pars$bg2$point, fuzz = 0)
    if(!is.null(fill$img)) x <- image_fill(x, color = pars$img$col, point = pars$img$point, fuzz = 0)
    img <- str_replace(img, ".png", "-fill.png")
    image_write(x, img)
  }
  glue("<img src='{img}' height = {size} width = {size}>")
}



# ggtext colour -----------------------------------------------------------

col_generic <- function(text, colour) {
  glue("<span style='color:{colour}'>{text}</span>")
}

b <- function(text) {
  paste0("<strong>", text, "</strong>")
}

