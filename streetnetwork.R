
library(dodgr)
library(tidyverse)
library(sf)

load("euro_nuts2_sf.Rda")

#subset 5 regions of Denmark
#DKnuts2_sf<- euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK"))

#extract streetnetwork for Midtjylland
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database
Midtjylland<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
filter (NUTS_NAME=="Midtjylland")

Midtjylland_sf<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Midtjylland")


#bounding polygom
bounding_polygon <- sf::st_transform(Midtjylland_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

Midtjylland_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(Midtjylland_streets,file="Midtjylland_streets.Rds")

#load streetnetwork
#Midtjylland_streets<-readRDS("Midtjylland_streets.Rds")

#number of distinct street lines
format (nrow (Midtjylland_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net <- weight_streetnet (Midtjylland_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

######

#extract streetnetwork for Nordjylland
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database
Nordjylland<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Nordjylland")

Nordjylland_sf<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Nordjylland")


#bounding polygom
bounding_polygon <- sf::st_transform(Nordjylland_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

Nordjylland_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(Nordjylland_streets,file="Nordjylland_streets.Rds")

#load streetnetwork
#Nordjylland_streets<-readRDS("Nordjylland_streets.Rds")

#number of distinct street lines
format (nrow (Nordjylland_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net <- weight_streetnet (Nordjylland_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#extract streetnetwork for Hovedstaden
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database
Hovedstaden<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Hovedstaden")

Hovedstaden_sf<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Hovedstaden")


#bounding polygom
bounding_polygon <- sf::st_transform(Hovedstaden_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

Hovedstaden_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(Hovedstaden_streets,file="Hovedstaden_streets.Rds")

#load streetnetwork
#Hovedstaden_streets<-readRDS("Hovedstaden_streets.Rds")

#number of distinct street lines
format (nrow (Hovedstaden_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net <- weight_streetnet (Hovedstaden_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#####
#extract streetnetwork for Sjælland
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database
Sjælland<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Sjælland")

Sjælland_sf<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Sjælland")


#bounding polygom
bounding_polygon <- sf::st_transform(Sjælland_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

Sjælland_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(Sjælland_streets,file="Sjælland_streets.Rds")

#load streetnetwork
#Sjælland_streets<-readRDS("Sjælland_streets.Rds")

#number of distinct street lines
format (nrow (Sjælland_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net <- weight_streetnet (Sjælland_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#####
#extract streetnetwork for Syddanmark
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database
Syddanmark<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Syddanmark")

Syddanmark_sf<-euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK")) %>% 
  filter (NUTS_NAME=="Syddanmark")


#bounding polygom
bounding_polygon <- sf::st_transform(Syddanmark_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

Syddanmark_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(Syddanmark_streets,file="Syddanmark_streets.Rds")

#load streetnetwork
#Syddanmark_streets<-readRDS("Syddanmark_streets.Rds")

#number of distinct street lines
format (nrow (Syddanmark_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net <- weight_streetnet (Syddanmark_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"
