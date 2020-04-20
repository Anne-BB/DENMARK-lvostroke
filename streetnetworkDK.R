
library(dodgr)
library(tidyverse)
library(sf)

load("euro_nuts2_sf.Rda")

#subset 5 regions of Denmark
DKnuts2_sf<- euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK"))

#extract streetnetwork for Denmark
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database


#bounding polygom
bounding_polygon <- sf::st_transform(DKnuts2_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

DK_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(DK_streets,file="DK_streets.Rds")

#load streetnetwork
#DK_streets<-readRDS("DK_streets.Rds")

#number of distinct street lines
format (nrow (DK_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net <- weight_streetnet (DK_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
