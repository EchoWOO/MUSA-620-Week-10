
library(rgdal)
library(sf)
library(maptools)
library(tidyverse)
library(viridis)

myTheme <- function() {
  theme_void() + 
    theme(
      text = element_text(size = 9),
      plot.title = element_text(size = 14, color = "#111111", hjust = 0, vjust = 0, face = "bold"), 
      plot.subtitle = element_text(size = 12, color = "#333333", hjust = 0, vjust = 0),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(colour = "white"),
      legend.direction = "horizontal", 
      legend.position = "bottom",
      plot.margin = margin(0, 0, 0.2, 0, 'cm'),
      legend.key.height = unit(0.4, "cm"), legend.key.width = unit(1, "cm"),
      legend.title = element_text(size = 9, color = "#111111", hjust = 0, vjust = 0),
      legend.text = element_text(size = 8, color = "#333333", hjust = 0, vjust = 0)
    )
}



ogrListLayers("https://www.washingtonpost.com/graphics/business/sunlight/data/us.json")


# you can specify the layer ("counties") when you load the file
counties <- readOGR("https://www.washingtonpost.com/graphics/business/sunlight/data/us.json","counties") %>%
  st_as_sf()

# read in the csv
sunlight <- read.csv("https://www.washingtonpost.com/graphics/business/sunlight/data/sunlight.csv")

# join the csv data to the map
sunlight$code <- as.character(sunlight$code)
counties$id <- as.character(counties$id)
countysunlight <- left_join(counties,sunlight,by=c("id"="code"))

# set the CRS 
st_crs(countysunlight) = 4326

# Remove HI, AK, PR for display purposes
countysunlight$county <- as.character(countysunlight$county)
countysunlight <- mutate(countysunlight,state = substr(county,nchar(county)-1,nchar(county)))
countysunlight <- filter(countysunlight, state != "HI") %>%
  filter(state != "AK") %>%
  filter(state != "PR")


ggplot() +
  geom_sf(data = countysunlight, aes(fill=avg), color=alpha("black",0.3), size=0.1) +
  scale_fill_viridis(discrete=FALSE , direction = 1, option="magma", name="Daily sunlight  ") +
  coord_sf(crs = st_crs(102003)) +
  labs(
    title = 'Average Daily Sunlight by County',
    subtitle = "Measured as solar radiation per square meter",
    caption = "Source: Washington Post"
  ) +
  myTheme()


# Our sunlight map has been getting shared around on Twitter
# https://twitter.com/NinjaEconomics/status/978300567381520384

# Now we are going to make a D3 version

# Export in geojson format
write_sf(countysunlight, dsn = "d:/shadertest/countysunlight2.geojson",delete_dsn=TRUE)




