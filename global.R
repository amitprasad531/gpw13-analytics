library(shiny)
library(dplyr)
library(DT)
library(markdown)

library(shinyjs)
library(shinyBS)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(devtools)
library(jsonlite)

library(plotly)
library(viridis)
library(gplots) 
library(DT)
library(dplyr)
library(pracma)
library(extrafont)
library(scales)
library(shinyLP)
library(tidyr)

library(lattice)
library(ggplot2)
library(rvest) 
library(reshape2)
library(xml2)
library(stringr)

library(leaflet)
library(maps)
library(htmltools)
library(rgeos)
library(rgdal)
library(RColorBrewer)

library(tidyverse)
library(heatmaply)

gpw <- read.csv("data/gpw-hale.csv", header = TRUE)
gpw_or <- gpw

# Load map json
map <-geojsonio::geojson_read("data/countries_official.json", what = "sp")

source("www/gpwvectors.R")
source("www/metadata_data.R")

for (i in 1:length(gpw_ind)) {
  names(gpw)[names(gpw) == gpw_ind[i]] <- gpw_colnames[i]
}

# ggplot theme
index_theme <- theme(panel.grid.minor.y = element_blank(),
                     panel.grid.major.y = element_line(linetype = 3, size = 0.3),
                     panel.grid.major.x = element_blank(),
                     axis.title = element_text(size = 13),
                     axis.title.y = element_text(margin = margin(r = 10)),
                     axis.title.x = element_text(margin = margin(t = 10)),
                     axis.text = element_text(size = 11))

# Extracting the Latest Available available data and year
latest <- function(b, y = rev(levels(as.factor(gpw$Year))), a = NA, z = NA) {
  for (i in 1:length(b)) {
    ifelse(is.na(b[i]), a, a <- b[i])
    if (!is.na(a)) {
      z <- y[i]
      return(c(a, z))
    }
  }
  return(c(a, z))
}

map_latest <- function(a, b, yr = NA, value = NA) {
  for (i in 1:length(a)) {
    ifelse(is.na(b[i]), value, value <- b[i])
    if (!is.na(value)) {
      yr <- a[i]
      return(c(yr, value))
    }
  }
  return(c(yr, value))
}

# Selecting appropriate map
map_type <- function(region, df) {
  switch(region,
         "All" = leaflet(df) %>% setView(20, 20, zoom = 2) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 1, maxZoom = 5)),
         "AFRO" = leaflet(df) %>% setView(30, 0, zoom = 3) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 2, maxZoom = 5)),
         "EMRO" = leaflet(df) %>% setView(38, 25, zoom = 4) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 3, maxZoom = 5)),
         "EURO" = leaflet(df) %>% setView(80, 55, zoom = 3) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 2, maxZoom = 5)),
         "PAHO" = leaflet(df) %>% setView(-90, 20, zoom = 2) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 2, maxZoom = 5)),
         "SEARO" = leaflet(df) %>% setView(120, 18, zoom = 4) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 3, maxZoom = 5)),
         "WPRO" = leaflet(df) %>% setView(140, 5, zoom = 3) %>% addProviderTiles("CartoDB.PositronNoLabels", options = providerTileOptions(minZoom = 2, maxZoom = 5))
  )
}

bar_colors <- function(region) {
  switch(region,
         "All" = c('AFRO' = '#fda09a', 'EMRO'='#cdbc64', 'EURO'='#36c14c', 'PAHO' = '#00bfc3', 'SEARO' = '#92baff', 'WPRO' = '#fa95eb'),
         "AFRO" = c('AFRO' = '#fda09a', 'EMRO'='#F0E5E5', 'EURO'='#F0E5E5', 'PAHO' = '#F0E5E5', 'SEARO' = '#F0E5E5', 'WPRO' = '#F0E5E5'),
         "EMRO" = c('AFRO' = '#F0E5E5', 'EMRO'='#cdbc64', 'EURO'='#F0E5E5', 'PAHO' = '#F0E5E5', 'SEARO' = '#F0E5E5', 'WPRO' = '#F0E5E5'),
         "EURO" = c('AFRO' = '#F0E5E5', 'EMRO'='#F0E5E5', 'EURO'='#36c14c', 'PAHO' = '#F0E5E5', 'SEARO' = '#F0E5E5', 'WPRO' = '#F0E5E5'),
         "PAHO" = c('AFRO' = '#F0E5E5', 'EMRO'='#F0E5E5', 'EURO'='#F0E5E5', 'PAHO' = '#00bfc3', 'SEARO' = '#F0E5E5', 'WPRO' = '#F0E5E5'),
         "SEARO" = c('AFRO' = '#F0E5E5', 'EMRO'='#F0E5E5', 'EURO'='#F0E5E5', 'PAHO' = '#F0E5E5', 'SEARO' = '#92baff', 'WPRO' = '#F0E5E5'),
         "WPRO" = c('AFRO' = '#F0E5E5', 'EMRO'='#F0E5E5', 'EURO'='#F0E5E5', 'PAHO' = '#F0E5E5', 'SEARO' = '#F0E5E5', 'WPRO' = '#fa95eb')
  ) 
}

fonts <- list(
  family = "sans-serif",
  size = 12
)

legend.Settings <- list(
  font = fonts
)

tf <- list(
  family = "sans-serif",
  size = 14
)

txline <- list(
  family = "sans-serif",
  size = 10
)

tyline <- list(
  family = "sans-serif",
  size = 10
)
