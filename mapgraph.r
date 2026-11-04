library(tidyverse)
library(osmdata)
library(osmextract)
library(igraph)
library(sf)
library(ggplot2)
library(plotly)

# HELPERS


get_city_road_network <- function(city_name) {
	bounds <- getbb(city_name, format = "sf_polygon")

	road_types <- c("living_street", "motorway", "primary", "residential", "tertiary", "trunk")


	road_network <- oe_get_network(city_name,
		       mode = "driving",
		       boundary = bounds)["highway"]

	return(road_network)

}

# SCRIPT
mapdata <- get_city_road_network("Kelowna, BC")
summary(mapdata)

p <- plot(mapdata, lwd = 2, key.pos = 4, key.width = lcm(2.75))

p <- ggplot(mapdata)

ggplotly(p, tooltip="map")
