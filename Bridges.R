library(eurostat)
library(leaflet)
library(mapview)
library(tmaptools)
load("euro_nuts2_sf.Rda")

DKnuts2_sf<- euro_nuts2_sf%>% filter(str_detect(NUTS_ID,"^DK"))
Bridges<- c(Storebaeltsbroen= "storebæltsbroen, Denmark",
            Lillebaeltsbroen= "lillebæltsbroen, Denmark")
Bridges <- tmaptools::geocode_OSM(Bridges, as.sf=TRUE)
mapview(Bridges)