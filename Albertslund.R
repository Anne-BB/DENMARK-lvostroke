

library(dodgr)
library(tidyverse)
library(sf)

load("Demostroke_sf.Rda")
#https://richardbeare.github.io/GeospatialStroke/RehabCatchment/README.html#7_create_a_street_network_database

#extract streetnetwork for Albertslund
if(file.exists("Albertslund")) {readRDS("Albertslund_streets.Rds")
}else {

Albertslund<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Albertslund")) %>% 
filter (Uoplyst=="Albertslund")

Albertslund_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Albertslund")) %>% 
                                          filter(Uoplyst=="Albertslund")

#bounding polygom
bounding_polygon <- sf::st_transform(Albertslund_sf,
                                     sf::st_crs(4326)) %>%
  sf::st_union () %>%
  sf::st_coordinates ()
bounding_polygon <- bounding_polygon [, 1:2]

Albertslund_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)

#save street network
saveRDS(Albertslund_streets, file="Albertslund_streets.Rds")}

#number of distinct street lines
format (nrow (Albertslund_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Albertslund <- weight_streetnet (Albertslund_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Allerød
if(file.exists("Allerød")) {readRDS("Allerød_streets.Rds")
}else {
  
  Allerød<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Allerød")) %>% 
    filter (Uoplyst=="Allerød")
  
  Allerød_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Allerød")) %>% 
    filter(Uoplyst=="Allerød")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Allerød_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Allerød_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Allerød_streets, file="Allerød_streets.Rds")}

#number of distinct street lines
format (nrow (Allerød_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Allerød <- weight_streetnet (Allerød_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Ballerup
if(file.exists("Ballerup")) {readRDS("Ballerup_streets.Rds")
}else {
  
  Ballerup<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ballerup")) %>% 
    filter (Uoplyst=="Ballerup")
  
  Ballerup_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ballerup")) %>% 
    filter(Uoplyst=="Ballerup")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Ballerup_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Ballerup_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Ballerup_streets, file="Ballerup_streets.Rds")}

#number of distinct street lines
format (nrow (Ballerup_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Ballerup <- weight_streetnet (Ballerup_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Bornholms Regionskommune
if(file.exists("Bornholms Regionskommune")) {readRDS("Bornholms_Regionskommune_streets.Rds")
}else {
  
  Bornholms_Regionskommune<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Bornholms Regionskommune")) %>% 
    filter (Uoplyst=="Bornholms Regionskommune")
  
  Bornholms_Regionskommune_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Bornholms Regionskommune")) %>% 
    filter(Uoplyst=="Bornholms Regionskommune")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Bornholms_Regionskommune_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Bornholms_Regionskommune_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Bornholms_Regionskommune_streets, file="Bornholms Regionskommune_streets.Rds")}

#number of distinct street lines
format (nrow (Bornholms_Regionskommune_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Bornholm <- weight_streetnet (Bornholms_Regionskommune_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#

#extract streetnetwork for Brøndby
if(file.exists("Brøndby")) {readRDS("Brøndby_streets.Rds")
}else {
  
  Brøndby<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Brøndby")) %>% 
    filter (Uoplyst=="Brøndby")
  
  Brøndby_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Brøndby")) %>% 
    filter(Uoplyst=="Brøndby")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Brøndby_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Brøndby_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Brøndby_streets, file="Brøndby_streets.Rds")}

#number of distinct street lines
format (nrow (Brøndby_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Brøndby <- weight_streetnet (Brøndby_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Dragør
if(file.exists("Dragør")) {readRDS("Dragør_streets.Rds")
}else {
  
  Dragør<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Dragør")) %>% 
    filter (Uoplyst=="Dragør")
  
  Dragør_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Dragør")) %>% 
    filter(Uoplyst=="Dragør")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Dragør_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Dragør_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Dragør_streets, file="Dragør_streets.Rds")}

#number of distinct street lines
format (nrow (Dragør_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Dragør <- weight_streetnet (Dragør_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Egedal
if(file.exists("Egedal")) {readRDS("Egedal_streets.Rds")
}else {
  
  Egedal<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Egedal")) %>% 
    filter (Uoplyst=="Egedal")
  
  Egedal_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Egedal")) %>% 
    filter(Uoplyst=="Egedal")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Egedal_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Egedal_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Egedal_streets, file="Egedal_streets.Rds")}

#number of distinct street lines
format (nrow (Egedal_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Egedal <- weight_streetnet (Egedal_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Fredensborg
if(file.exists("Fredensborg")) {readRDS("Fredensborg_streets.Rds")
}else {
  
  Fredensborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Fredensborg")) %>% 
    filter (Uoplyst=="Fredensborg")
  
  Fredensborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Fredensborg")) %>% 
    filter(Uoplyst=="Fredensborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Fredensborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Fredensborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Fredensborg_streets, file="Fredensborg_streets.Rds")}

#number of distinct street lines
format (nrow (Fredensborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Fredensborg <- weight_streetnet (Fredensborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"
#
#extract streetnetwork for Frederiksberg
if(file.exists("Frederiksberg")) {readRDS("Frederiksberg_streets.Rds")
}else {
  
  Frederiksberg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederiksberg")) %>% 
    filter (Uoplyst=="Frederiksberg")
  
  Frederiksberg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederiksberg")) %>% 
    filter(Uoplyst=="Frederiksberg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Frederiksberg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Frederiksberg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Frederiksberg_streets, file="Frederiksberg_streets.Rds")}

#number of distinct street lines
format (nrow (Frederiksberg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Frederiksberg <- weight_streetnet (Frederiksberg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Frederikssund
if(file.exists("Frederikssund")) {readRDS("Frederikssund_streets.Rds")
}else {
  
  Frederikssund<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederikssund")) %>% 
    filter (Uoplyst=="Frederikssund")
  
  Frederikssund_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederikssund")) %>% 
    filter(Uoplyst=="Frederikssund")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Frederikssund_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Frederikssund_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Frederikssund_streets, file="Frederikssund_streets.Rds")}

#number of distinct street lines
format (nrow (Frederikssund_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Frederikssund <- weight_streetnet (Frederikssund_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Furresø
if(file.exists("Furresø")) {readRDS("Furresø_streets.Rds")
}else {
  
  Furresø<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Furresø")) %>% 
    filter (Uoplyst=="Furresø")
  
  Furresø_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Furresø")) %>% 
    filter(Uoplyst=="Furresø")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Furresø_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Furresø_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Furresø_streets, file="Furresø_streets.Rds")}

#number of distinct street lines
format (nrow (Furresø_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Furresø <- weight_streetnet (Furresø_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Gentofte
if(file.exists("Gentofte")) {readRDS("Gentofte_streets.Rds")
}else {
  
  Gentofte<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Gentofte")) %>% 
    filter (Uoplyst=="Gentofte")
  
  Gentofte_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Gentofte")) %>% 
    filter(Uoplyst=="Gentofte")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Gentofte_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Gentofte_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Gentofte_streets, file="Gentofte_streets.Rds")}

#number of distinct street lines
format (nrow (Gentofte_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Gentofte <- weight_streetnet (Gentofte_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Gladsaxe
if(file.exists("Gladsaxe")) {readRDS("Gladsaxe_streets.Rds")
}else {
  
  Gladsaxe<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Gladsaxe")) %>% 
    filter (Uoplyst=="Gladsaxe")
  
  Gladsaxe_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Gladsaxe")) %>% 
    filter(Uoplyst=="Gladsaxe")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Gladsaxe_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Gladsaxe_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Gladsaxe_streets, file="Gladsaxe_streets.Rds")}

#number of distinct street lines
format (nrow (Gladsaxe_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Gladsaxe <- weight_streetnet (Gladsaxe_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Glostrup
if(file.exists("Glostrup")) {readRDS("Glostrup_streets.Rds")
}else {
  
  Glostrup<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Glostrup")) %>% 
    filter (Uoplyst=="Glostrup")
  
  Glostrup_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Glostrup")) %>% 
    filter(Uoplyst=="Glostrup")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Glostrup_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Glostrup_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Glostrup_streets, file="Glostrup_streets.Rds")}

#number of distinct street lines
format (nrow (Glostrup_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Glostrup <- weight_streetnet (Glostrup_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Gribskov
if(file.exists("Gribskov")) {readRDS("Gribskov_streets.Rds")
}else {
  
  Gribskov<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Gribskov")) %>% 
    filter (Uoplyst=="Gribskov")
  
  Gribskov_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Gribskov")) %>% 
    filter(Uoplyst=="Gribskov")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Gribskov_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Gribskov_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Gribskov_streets, file="Gribskov_streets.Rds")}

#number of distinct street lines
format (nrow (Gribskov_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Gribskov <- weight_streetnet (Gribskov_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Halsnæs
if(file.exists("Halsnæs")) {readRDS("Halsnæs_streets.Rds")
}else {
  
  Halsnæs<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Halsnæs")) %>% 
    filter (Uoplyst=="Halsnæs")
  
  Halsnæs_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Halsnæs")) %>% 
    filter(Uoplyst=="Halsnæs")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Halsnæs_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Halsnæs_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Halsnæs_streets, file="Halsnæs_streets.Rds")}

#number of distinct street lines
format (nrow (Halsnæs_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Halsnæs <- weight_streetnet (Halsnæs_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Albertslund
if(file.exists("Helsingør")) {readRDS("Helsingør_streets.Rds")
}else {
  
  Helsingør<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Helsingør")) %>% 
    filter (Uoplyst=="Helsingør")
  
  Helsingør_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Helsingør")) %>% 
    filter(Uoplyst=="Helsingør")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Helsingør_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Helsingør_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Helsingør_streets, file="Helsingør_streets.Rds")}

#number of distinct street lines
format (nrow (Helsingør_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Helsingør <- weight_streetnet (Helsingør_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Herlev
if(file.exists("Herlev")) {readRDS("Herlev_streets.Rds")
}else {
  
  Herlev<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Herlev")) %>% 
    filter (Uoplyst=="Herlev")
  
  Herlev_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Herlev")) %>% 
    filter(Uoplyst=="Herlev")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Herlev_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Herlev_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Herlev_streets, file="Herlev_streets.Rds")}

#number of distinct street lines
format (nrow (Herlev_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Herlev <- weight_streetnet (Herlev_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Hillerød
if(file.exists("Hillerød")) {readRDS("Hillerød_streets.Rds")
}else {
  
  Hillerød<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hillerød")) %>% 
    filter (Uoplyst=="Hillerød")
  
  Hillerød_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hillerød")) %>% 
    filter(Uoplyst=="Hillerød")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Hillerød_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Hillerød_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Hillerød_streets, file="Hillerød_streets.Rds")}

#number of distinct street lines
format (nrow (Hillerød_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Hillerød <- weight_streetnet (Hillerød_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Hvidovre
if(file.exists("Hvidovre")) {readRDS("Hvidovre_streets.Rds")
}else {
  
  Hvidovre<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hvidovre")) %>% 
    filter (Uoplyst=="Hvidovre")
  
  Hvidovre_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hvidovre")) %>% 
    filter(Uoplyst=="Hvidovre")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Hvidovre_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Hvidovre_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Hvidovre_streets, file="Hvidovre_streets.Rds")}

#number of distinct street lines
format (nrow (Hvidovre_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Hvidovre <- weight_streetnet (Hvidovre_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Høje-Taastrup
if(file.exists("Høje-Taastrup")) {readRDS("Høje-Taastrup_streets.Rds")
}else {
  
  Høje-Taastrup<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Høje-Taastrup")) %>% 
    filter (Uoplyst=="Høje-Taastrup")
  
  Høje-Taastrup_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Høje-Taastrup")) %>% 
    filter(Uoplyst=="Høje-Taastrup")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Høje-Taastrup_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Høje-Taastrup_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Høje-Taastrup_streets, file="Høje-Taastrup_streets.Rds")}

#number of distinct street lines
format (nrow (Høje-Taastrup_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Høje-Taastrup <- weight_streetnet (Høje-Taastrup_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Hørsholm
if(file.exists("Hørsholm")) {readRDS("Hørsholm_streets.Rds")
}else {
  
  Hørsholm<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hørsholm")) %>% 
    filter (Uoplyst=="Hørsholm")
  
  Hørsholm_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hørsholm")) %>% 
    filter(Uoplyst=="Hørsholm")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Hørsholm_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Hørsholm_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Hørsholm_streets, file="Hørsholm_streets.Rds")}

#number of distinct street lines
format (nrow (Hørsholm_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Hørsholm <- weight_streetnet (Hørsholm_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Ishøj
if(file.exists("Ishøj")) {readRDS("Ishøj_streets.Rds")
}else {
  
  Ishøj<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ishøj")) %>% 
    filter (Uoplyst=="Ishøj")
  
  Ishøj_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ishøj")) %>% 
    filter(Uoplyst=="Ishøj")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Ishøj_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Ishøj_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Ishøj_streets, file="Ishøj_streets.Rds")}

#number of distinct street lines
format (nrow (Ishøj_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Ishøj <- weight_streetnet (Ishøj_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for København
if(file.exists("København")) {readRDS("København_streets.Rds")
}else {
  
  København<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"København")) %>% 
    filter (Uoplyst=="København")
  
  København_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"København")) %>% 
    filter(Uoplyst=="København")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(København_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  København_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(København_streets, file="København_streets.Rds")}

#number of distinct street lines
format (nrow (København_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_København <- weight_streetnet (København_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Lyngby-Tårbæk
if(file.exists("Lyngby-Tårbæk")) {readRDS("Lyngby-Tårbæk_streets.Rds")
}else {
  
  Lyngby-Tårbæk<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lyngby-Tårbæk")) %>% 
    filter (Uoplyst=="Lyngby-Tårbæk")
  
  Lyngby-Tårbæk_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lyngby-Tårbæk")) %>% 
    filter(Uoplyst=="Lyngby-Tårbæk")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Lyngby-Tårbæk_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Lyngby-Tårbæk_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Lyngby-Tårbæk_streets, file="Lyngby-Tårbæk_streets.Rds")}

#number of distinct street lines
format (nrow (Lyngby-Tårbæk_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Lyngby-Tårbæk <- weight_streetnet (Lyngby-Tårbæk_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Rudersdal
if(file.exists("Rudersdal")) {readRDS("Rudersdal_streets.Rds")
}else {
  
  Rudersdal<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Rudersdal")) %>% 
    filter (Uoplyst=="Rudersdal")
  
  Rudersdal_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Rudersdal")) %>% 
    filter(Uoplyst=="Rudersdal")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Rudersdal_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Rudersdal_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Rudersdal_streets, file="Rudersdal_streets.Rds")}

#number of distinct street lines
format (nrow (Rudersdal_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Rudersdal <- weight_streetnet (Rudersdal_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Rødovre
if(file.exists("Rødovre")) {readRDS("Rødovre_streets.Rds")
}else {
  
  Rødovre<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Rødovre")) %>% 
    filter (Uoplyst=="Rødovre")
  
  Rødovre_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Rødovre")) %>% 
    filter(Uoplyst=="Rødovre")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Rødovre_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Rødovre_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Rødovre_streets, file="Rødovre_streets.Rds")}

#number of distinct street lines
format (nrow (Rødovre_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Rødovre <- weight_streetnet (Rødovre_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Tårnby
if(file.exists("Tårnby")) {readRDS("Tårnby_streets.Rds")
}else {
  
  Tårnby<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Tårnby")) %>% 
    filter (Uoplyst=="Tårnby")
  
  Tårnby_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Tårnby")) %>% 
    filter(Uoplyst=="Tårnby")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Tårnby_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Tårnby_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Tårnby_streets, file="Tårnby_streets.Rds")}

#number of distinct street lines
format (nrow (Tårnby_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Tårnby <- weight_streetnet (Tårnby_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Vallensbæk
if(file.exists("Vallensbæk")) {readRDS("Vallensbæk_streets.Rds")
}else {
  
  Vallensbæk<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vallensbæk")) %>% 
    filter (Uoplyst=="Vallensbæk")
  
  Vallensbæk_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vallensbæk")) %>% 
    filter(Uoplyst=="Vallensbæk")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Vallensbæk_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Vallensbæk_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Vallensbæk_streets, file="Vallensbæk_streets.Rds")}

#number of distinct street lines
format (nrow (Vallensbæk_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Vallensbæk <- weight_streetnet (Vallensbæk_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Faxe
if(file.exists("Faxe")) {readRDS("Faxe_streets.Rds")
}else {
  
  Faxe<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Faxe")) %>% 
    filter (Uoplyst=="Faxe")
  
  Faxe_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Faxe")) %>% 
    filter(Uoplyst=="Faxe")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Faxe_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Faxe_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Faxe_streets, file="Faxe_streets.Rds")}

#number of distinct street lines
format (nrow (Faxe_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Faxe <- weight_streetnet (Faxe_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Greve
if(file.exists("Greve")) {readRDS("Greve_streets.Rds")
}else {
  
  Greve<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Greve")) %>% 
    filter (Uoplyst=="Greve")
  
  Greve_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Greve")) %>% 
    filter(Uoplyst=="Greve")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Greve_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Greve_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Greve_streets, file="Greve_streets.Rds")}

#number of distinct street lines
format (nrow (Greve_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Greve <- weight_streetnet (Greve_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Guldborgsund
if(file.exists("Guldborgsund")) {readRDS("Guldborgsund_streets.Rds")
}else {
  
  Guldborgsund<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Guldborgsund")) %>% 
    filter (Uoplyst=="Guldborgsund")
  
  Guldborgsund_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Guldborgsund")) %>% 
    filter(Uoplyst=="Guldborgsund")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Guldborgsund_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Guldborgsund_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Guldborgsund_streets, file="Guldborgsund_streets.Rds")}

#number of distinct street lines
format (nrow (Guldborgsund_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Guldborgsund <- weight_streetnet (Guldborgsund_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Holbæk
if(file.exists("Holbæk")) {readRDS("Holbæk_streets.Rds")
}else {
  
  Holbæk<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Holbæk")) %>% 
    filter (Uoplyst=="Holbæk")
  
  Holbæk_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Holbæk")) %>% 
    filter(Uoplyst=="Holbæk")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Holbæk_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Holbæk_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Holbæk_streets, file="Holbæk_streets.Rds")}

#number of distinct street lines
format (nrow (Holbæk_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Holbæk <- weight_streetnet (Holbæk_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Kalundborg
if(file.exists("Kalundborg")) {readRDS("Kalundborg_streets.Rds")
}else {
  
  Kalundborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Kalundborg")) %>% 
    filter (Uoplyst=="Kalundborg")
  
  Kalundborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Kalundborg")) %>% 
    filter(Uoplyst=="Kalundborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Kalundborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Kalundborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Kalundborg_streets, file="Kalundborg_streets.Rds")}

#number of distinct street lines
format (nrow (Kalundborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Kalundborg <- weight_streetnet (Kalundborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Køge
if(file.exists("Køge")) {readRDS("Køge_streets.Rds")
}else {
  
  Køge<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Køge")) %>% 
    filter (Uoplyst=="Køge")
  
  Køge_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Køge")) %>% 
    filter(Uoplyst=="Køge")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Køge_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Køge_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Køge_streets, file="Køge_streets.Rds")}

#number of distinct street lines
format (nrow (Køge_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Køge <- weight_streetnet (Køge_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Lejre
if(file.exists("Lejre")) {readRDS("Lejre_streets.Rds")
}else {
  
  Lejre<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lejre")) %>% 
    filter (Uoplyst=="Lejre")
  
  Lejre_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lejre")) %>% 
    filter(Uoplyst=="Lejre")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Lejre_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Lejre_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Lejre_streets, file="Lejre_streets.Rds")}

#number of distinct street lines
format (nrow (Lejre_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Lejre <- weight_streetnet (Lejre_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Lolland
if(file.exists("Lolland")) {readRDS("Lolland_streets.Rds")
}else {
  
  Lolland<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lolland")) %>% 
    filter (Uoplyst=="Lolland")
  
  Lolland_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lolland")) %>% 
    filter(Uoplyst=="Lolland")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Lolland_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Lolland_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Lolland_streets, file="Lolland_streets.Rds")}

#number of distinct street lines
format (nrow (Lolland_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Lolland <- weight_streetnet (Lolland_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Næstved
if(file.exists("Næstved")) {readRDS("Næstved_streets.Rds")
}else {
  
  Næstved<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Næstved")) %>% 
    filter (Uoplyst=="Næstved")
  
  Næstved_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Næstved")) %>% 
    filter(Uoplyst=="Næstved")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Næstved_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Næstved_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Næstved_streets, file="Næstved_streets.Rds")}

#number of distinct street lines
format (nrow (Næstved_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Næstved <- weight_streetnet (Næstved_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Odsherred
if(file.exists("Odsherred")) {readRDS("Odsherred_streets.Rds")
}else {
  
  Odsherred<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Odsherred")) %>% 
    filter (Uoplyst=="Odsherred")
  
  Odsherred_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Odsherred")) %>% 
    filter(Uoplyst=="Odsherred")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Odsherred_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Odsherred_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Odsherred_streets, file="Odsherred_streets.Rds")}

#number of distinct street lines
format (nrow (Odsherred_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Odsherred <- weight_streetnet (Odsherred_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Ringsted
if(file.exists("Ringsted")) {readRDS("Ringsted_streets.Rds")
}else {
  
  Ringsted<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ringsted")) %>% 
    filter (Uoplyst=="Ringsted")
  
  Ringsted_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ringsted")) %>% 
    filter(Uoplyst=="Ringsted")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Ringsted_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Ringsted_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Ringsted_streets, file="Ringsted_streets.Rds")}

#number of distinct street lines
format (nrow (Ringsted_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Ringsted <- weight_streetnet (Ringsted_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Roskilde
if(file.exists("Roskilde")) {readRDS("Roskilde_streets.Rds")
}else {
  
  Roskilde<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Roskilde")) %>% 
    filter (Uoplyst=="Roskilde")
  
  Roskilde_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Roskilde")) %>% 
    filter(Uoplyst=="Roskilde")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Roskilde_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Roskilde_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Roskilde_streets, file="Roskilde_streets.Rds")}

#number of distinct street lines
format (nrow (Roskilde_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Roskilde <- weight_streetnet (Roskilde_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Slagelse
if(file.exists("Slagelse")) {readRDS("Slagelse_streets.Rds")
}else {
  
  Slagelse<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Slagelse")) %>% 
    filter (Uoplyst=="Slagelse")
  
  Slagelse_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Slagelse")) %>% 
    filter(Uoplyst=="Slagelse")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Slagelse_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Slagelse_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Slagelse_streets, file="Slagelse_streets.Rds")}

#number of distinct street lines
format (nrow (Slagelse_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Slagelse <- weight_streetnet (Slagelse_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Solrød
if(file.exists("Solrød")) {readRDS("Solrød_streets.Rds")
}else {
  
  Solrød<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Solrød")) %>% 
    filter (Uoplyst=="Solrød")
  
  Solrød_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Solrød")) %>% 
    filter(Uoplyst=="Solrød")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Solrød_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Solrød_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Solrød_streets, file="Solrød_streets.Rds")}

#number of distinct street lines
format (nrow (Solrød_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Solrød <- weight_streetnet (Solrød_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Stevns
if(file.exists("Stevns")) {readRDS("Stevns_streets.Rds")
}else {
  
  Stevns<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Stevns")) %>% 
    filter (Uoplyst=="Stevns")
  
  Stevns_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Stevns")) %>% 
    filter(Uoplyst=="Stevns")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Stevns_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Stevns_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Stevns_streets, file="Stevns_streets.Rds")}

#number of distinct street lines
format (nrow (Stevns_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Stevns <- weight_streetnet (Stevns_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Vordingborg
if(file.exists("Vordingborg")) {readRDS("Vordingborg_streets.Rds")
}else {
  
  Vordingborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vordingborg")) %>% 
    filter (Uoplyst=="Vordingborg")
  
  Vordingborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vordingborg")) %>% 
    filter(Uoplyst=="Vordingborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Vordingborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Vordingborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Vordingborg_streets, file="Vordingborg_streets.Rds")}

#number of distinct street lines
format (nrow (Vordingborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Vordingborg <- weight_streetnet (Vordingborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Aabenraa
if(file.exists("Aabenraa")) {readRDS("Aabenraa_streets.Rds")
}else {
  
  Aabenraa<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Aabenraa")) %>% 
    filter (Uoplyst=="Aabenraa")
  
  Aabenraa_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Aabenraa")) %>% 
    filter(Uoplyst=="Aabenraa")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Aabenraa_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Aabenraa_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Aabenraa_streets, file="Aabenraa_streets.Rds")}

#number of distinct street lines
format (nrow (Aabenraa_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Aabenraa <- weight_streetnet (Aabenraa_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Assens
if(file.exists("Assens")) {readRDS("Assens_streets.Rds")
}else {
  
  Assens<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Assens")) %>% 
    filter (Uoplyst=="Assens")
  
  Assens_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Assens")) %>% 
    filter(Uoplyst=="Assens")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Assens_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Assens_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Assens_streets, file="Assens_streets.Rds")}

#number of distinct street lines
format (nrow (Assens_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Assens <- weight_streetnet (Assens_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Billund
if(file.exists("Billund")) {readRDS("Billund_streets.Rds")
}else {
  
  Billund<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Billund")) %>% 
    filter (Uoplyst=="Billund")
  
  Billund_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Billund")) %>% 
    filter(Uoplyst=="Billund")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Billund_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Billund_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Billund_streets, file="Billund_streets.Rds")}

#number of distinct street lines
format (nrow (Billund_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Billund <- weight_streetnet (Billund_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Esbjerg
if(file.exists("Esbjerg")) {readRDS("Esbjerg_streets.Rds")
}else {
  
  Esbjerg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Esbjerg")) %>% 
    filter (Uoplyst=="Esbjerg")
  
  Esbjerg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Esbjerg")) %>% 
    filter(Uoplyst=="Esbjerg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Esbjerg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Esbjerg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Esbjerg_streets, file="Esbjerg_streets.Rds")}

#number of distinct street lines
format (nrow (Esbjerg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Esbjerg <- weight_streetnet (Esbjerg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Faaborg-Midtfyn
if(file.exists("Faaborg-Midtfyn")) {readRDS("Faaborg-Midtfyn_streets.Rds")
}else {
  
  Faaborg-Midtfyn<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Faaborg-Midtfyn")) %>% 
    filter (Uoplyst=="Faaborg-Midtfyn")
  
  Faaborg-Midtfyn_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Faaborg-Midtfyn")) %>% 
    filter(Uoplyst=="Faaborg-Midtfyn")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Faaborg-Midtfyn_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Faaborg-Midtfyn_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Faaborg-Midtfyn_streets, file="Faaborg-Midtfyn_streets.Rds")}

#number of distinct street lines
format (nrow (Faaborg-Midtfyn_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Faaborg-Midtfyn <- weight_streetnet (Faaborg-Midtfyn_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Fanø
if(file.exists("Fanø")) {readRDS("Fanø_streets.Rds")
}else {
  
  Fanø<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Fanø")) %>% 
    filter (Uoplyst=="Fanø")
  
  Fanø_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Fanø")) %>% 
    filter(Uoplyst=="Fanø")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Fanø_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Fanø_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Fanø_streets, file="Fanø_streets.Rds")}

#number of distinct street lines
format (nrow (Fanø_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Fanø <- weight_streetnet (Fanø_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Frederecia
if(file.exists("Frederecia")) {readRDS("Frederecia_streets.Rds")
}else {
  
  Frederecia<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederecia")) %>% 
    filter (Uoplyst=="Frederecia")
  
  Frederecia_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederecia")) %>% 
    filter(Uoplyst=="Frederecia")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Frederecia_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Frederecia_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Frederecia_streets, file="Frederecia_streets.Rds")}

#number of distinct street lines
format (nrow (Frederecia_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Frederecia <- weight_streetnet (Frederecia_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Haderslev
if(file.exists("Haderslev")) {readRDS("Haderslev_streets.Rds")
}else {
  
  Haderslev<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Haderslev")) %>% 
    filter (Uoplyst=="Haderslev")
  
  Haderslev_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Haderslev")) %>% 
    filter(Uoplyst=="Haderslev")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Haderslev_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Haderslev_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Haderslev_streets, file="Haderslev_streets.Rds")}

#number of distinct street lines
format (nrow (Haderslev_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Haderslev <- weight_streetnet (Haderslev_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Kerteminde
if(file.exists("Kerteminde")) {readRDS("Kerteminde_streets.Rds")
}else {
  
  Kerteminde<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Kerteminde")) %>% 
    filter (Uoplyst=="Kerteminde")
  
  Kerteminde_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Kerteminde")) %>% 
    filter(Uoplyst=="Kerteminde")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Kerteminde_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Kerteminde_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Kerteminde_streets, file="Kerteminde_streets.Rds")}

#number of distinct street lines
format (nrow (Kerteminde_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Kerteminde <- weight_streetnet (Kerteminde_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Kolding
if(file.exists("Kolding")) {readRDS("Kolding_streets.Rds")
}else {
  
  Kolding<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Kolding")) %>% 
    filter (Uoplyst=="Kolding")
  
  Kolding_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Kolding")) %>% 
    filter(Uoplyst=="Kolding")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Kolding_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Kolding_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Kolding_streets, file="Kolding_streets.Rds")}

#number of distinct street lines
format (nrow (Kolding_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Kolding <- weight_streetnet (Kolding_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Langeland
if(file.exists("Langeland")) {readRDS("Langeland_streets.Rds")
}else {
  
  Langeland<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Langeland")) %>% 
    filter (Uoplyst=="Langeland")
  
  Langeland_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Langeland")) %>% 
    filter(Uoplyst=="Langeland")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Langeland_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Langeland_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Langeland_streets, file="Langeland_streets.Rds")}

#number of distinct street lines
format (nrow (Langeland_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Langeland <- weight_streetnet (Langeland_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Middelfart
if(file.exists("Middelfart")) {readRDS("Middelfart_streets.Rds")
}else {
  
  Middelfart<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Middelfart")) %>% 
    filter (Uoplyst=="Middelfart")
  
  Middelfart_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Middelfart")) %>% 
    filter(Uoplyst=="Middelfart")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Middelfart_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Middelfart_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Middelfart_streets, file="Middelfart_streets.Rds")}

#number of distinct street lines
format (nrow (Middelfart_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Middelfart <- weight_streetnet (Middelfart_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Nordfyns
if(file.exists("Nordfyns")) {readRDS("Nordfyns_streets.Rds")
}else {
  
  Nordfyns<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Nordfyns")) %>% 
    filter (Uoplyst=="Nordfyns")
  
  Nordfyns_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Nordfyns")) %>% 
    filter(Uoplyst=="Nordfyns")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Nordfyns_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Nordfyns_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Nordfyns_streets, file="Nordfyns_streets.Rds")}

#number of distinct street lines
format (nrow (Nordfyns_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Nordfyns <- weight_streetnet (Nordfyns_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Nyborg
if(file.exists("Nyborg")) {readRDS("Nyborg_streets.Rds")
}else {
  
  Nyborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Nyborg")) %>% 
    filter (Uoplyst=="Nyborg")
  
  Nyborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Nyborg")) %>% 
    filter(Uoplyst=="Nyborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Nyborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Nyborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Nyborg_streets, file="Nyborg_streets.Rds")}

#number of distinct street lines
format (nrow (Nyborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Nyborg <- weight_streetnet (Nyborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Odense
if(file.exists("Odense")) {readRDS("Odense_streets.Rds")
}else {
  
  Odense<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Odense")) %>% 
    filter (Uoplyst=="Odense")
  
  Odense_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Odense")) %>% 
    filter(Uoplyst=="Odense")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Odense_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Odense_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Odense_streets, file="Odense_streets.Rds")}

#number of distinct street lines
format (nrow (Odense_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Odense <- weight_streetnet (Odense_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Svendborg
if(file.exists("Svendborg")) {readRDS("Svendborg_streets.Rds")
}else {
  
  Svendborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Svendborg")) %>% 
    filter (Uoplyst=="Svendborg")
  
  Svendborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Svendborg")) %>% 
    filter(Uoplyst=="Svendborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Svendborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Svendborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Svendborg_streets, file="Svendborg_streets.Rds")}

#number of distinct street lines
format (nrow (Svendborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Svendborg <- weight_streetnet (Svendborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Sønderborg
if(file.exists("Sønderborg")) {readRDS("Sønderborg_streets.Rds")
}else {
  
  Sønderborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Sønderborg")) %>% 
    filter (Uoplyst=="Sønderborg")
  
  Sønderborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Sønderborg")) %>% 
    filter(Uoplyst=="Sønderborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Sønderborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Sønderborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Sønderborg_streets, file="Sønderborg_streets.Rds")}

#number of distinct street lines
format (nrow (Sønderborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Sønderborg <- weight_streetnet (Sønderborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Tønder
if(file.exists("Tønder")) {readRDS("Tønder_streets.Rds")
}else {
  
  Tønder<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Tønder")) %>% 
    filter (Uoplyst=="Tønder")
  
  Tønder_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Tønder")) %>% 
    filter(Uoplyst=="Tønder")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Tønder_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Tønder_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Tønder_streets, file="Tønder_streets.Rds")}

#number of distinct street lines
format (nrow (Tønder_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Tønder <- weight_streetnet (Tønder_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Varde
if(file.exists("Varde")) {readRDS("Varde_streets.Rds")
}else {
  
  Varde<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Varde")) %>% 
    filter (Uoplyst=="Varde")
  
  Varde_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Varde")) %>% 
    filter(Uoplyst=="Varde")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Varde_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Varde_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Varde_streets, file="Varde_streets.Rds")}

#number of distinct street lines
format (nrow (Varde_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Varde <- weight_streetnet (Varde_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Vejen
if(file.exists("Vejen")) {readRDS("Vejen_streets.Rds")
}else {
  
  Vejen<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vejen")) %>% 
    filter (Uoplyst=="Vejen")
  
  Vejen_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vejen")) %>% 
    filter(Uoplyst=="Vejen")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Vejen_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Vejen_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Vejen_streets, file="Vejen_streets.Rds")}

#number of distinct street lines
format (nrow (Vejen_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Vejen <- weight_streetnet (Vejen_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Vejle
if(file.exists("Vejle")) {readRDS("Vejle_streets.Rds")
}else {
  
  Vejle<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vejle")) %>% 
    filter (Uoplyst=="Vejle")
  
  Vejle_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vejle")) %>% 
    filter(Uoplyst=="Vejle")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Vejle_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Vejle_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Vejle_streets, file="Vejle_streets.Rds")}

#number of distinct street lines
format (nrow (Vejle_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Vejle <- weight_streetnet (Vejle_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Ærø
if(file.exists("Ærø")) {readRDS("Ærø_streets.Rds")
}else {
  
  Ærø<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ærø")) %>% 
    filter (Uoplyst=="Ærø")
  
  Ærø_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ærø")) %>% 
    filter(Uoplyst=="Ærø")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Ærø_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Ærø_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Ærø_streets, file="Ærø_streets.Rds")}

#number of distinct street lines
format (nrow (Ærø_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Ærø <- weight_streetnet (Ærø_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Favrskov
if(file.exists("Favrskov")) {readRDS("Favrskov_streets.Rds")
}else {
  
  Favrskov<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Favrskov")) %>% 
    filter (Uoplyst=="Favrskov")
  
  Favrskov_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Favrskov")) %>% 
    filter(Uoplyst=="Favrskov")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Favrskov_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Favrskov_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Favrskov_streets, file="Favrskov_streets.Rds")}

#number of distinct street lines
format (nrow (Favrskov_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Favrskov <- weight_streetnet (Favrskov_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Hedensted
if(file.exists("Hedensted")) {readRDS("Hedensted_streets.Rds")
}else {
  
  Hedensted<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hedensted")) %>% 
    filter (Uoplyst=="Hedensted")
  
  Hedensted_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hedensted")) %>% 
    filter(Uoplyst=="Hedensted")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Hedensted_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Hedensted_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Hedensted_streets, file="Hedensted_streets.Rds")}

#number of distinct street lines
format (nrow (Hedensted_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Hedensted <- weight_streetnet (Hedensted_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Herning
if(file.exists("Herning")) {readRDS("Herning_streets.Rds")
}else {
  
  Herning<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Herning")) %>% 
    filter (Uoplyst=="Herning")
  
  Herning_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Herning")) %>% 
    filter(Uoplyst=="Herning")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Herning_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Herning_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Herning_streets, file="Herning_streets.Rds")}

#number of distinct street lines
format (nrow (Herning_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Herning <- weight_streetnet (Herning_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Holstebro
if(file.exists("Holstebro")) {readRDS("Holstebro_streets.Rds")
}else {
  
  Holstebro<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Holstebro")) %>% 
    filter (Uoplyst=="Holstebro")
  
  Holstebro_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Holstebro")) %>% 
    filter(Uoplyst=="Holstebro")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Holstebro_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Holstebro_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Holstebro_streets, file="Holstebro_streets.Rds")}

#number of distinct street lines
format (nrow (Holstebro_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Holstebro <- weight_streetnet (Holstebro_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Horsens
if(file.exists("Horsens")) {readRDS("Horsens_streets.Rds")
}else {
  
  Horsens<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Horsens")) %>% 
    filter (Uoplyst=="Horsens")
  
  Horsens_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Horsens")) %>% 
    filter(Uoplyst=="Horsens")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Horsens_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Horsens_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Horsens_streets, file="Horsens_streets.Rds")}

#number of distinct street lines
format (nrow (Horsens_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Horsens <- weight_streetnet (Horsens_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Ikast-Brande
if(file.exists("Ikast-Brande")) {readRDS("Ikast-Brande_streets.Rds")
}else {
  
  Ikast-Brande<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ikast-Brande")) %>% 
    filter (Uoplyst=="Ikast-Brande")
  
  Ikast-Brande_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ikast-Brande")) %>% 
    filter(Uoplyst=="Ikast-Brande")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Ikast-Brande_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Ikast-Brande_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Ikast-Brande_streets, file="Ikast-Brande_streets.Rds")}

#number of distinct street lines
format (nrow (Ikast-Brande_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Ikast-Brande <- weight_streetnet (Ikast-Brande_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Lemvig
if(file.exists("Lemvig")) {readRDS("Lemvig_streets.Rds")
}else {
  
  Lemvig<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lemvig")) %>% 
    filter (Uoplyst=="Lemvig")
  
  Lemvig_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Lemvig")) %>% 
    filter(Uoplyst=="Lemvig")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Lemvig_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Lemvig_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Lemvig_streets, file="Lemvig_streets.Rds")}

#number of distinct street lines
format (nrow (Lemvig_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Lemvig <- weight_streetnet (Lemvig_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Norddjurs
if(file.exists("Norddjurs")) {readRDS("Norddjurs_streets.Rds")
}else {
  
  Norddjurs<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Norddjurs")) %>% 
    filter (Uoplyst=="Norddjurs")
  
  Norddjurs_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Norddjurs")) %>% 
    filter(Uoplyst=="Norddjurs")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Norddjurs_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Norddjurs_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Norddjurs_streets, file="Norddjurs_streets.Rds")}

#number of distinct street lines
format (nrow (Norddjurs_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Norddjurs <- weight_streetnet (Norddjurs_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Odder
if(file.exists("Odder")) {readRDS("Odder_streets.Rds")
}else {
  
  Odder<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Odder")) %>% 
    filter (Uoplyst=="Odder")
  
  Odder_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Odder")) %>% 
    filter(Uoplyst=="Odder")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Odder_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Odder_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Odder_streets, file="Odder_streets.Rds")}

#number of distinct street lines
format (nrow (Odder_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Odder <- weight_streetnet (Odder_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Randers
if(file.exists("Randers")) {readRDS("Randers_streets.Rds")
}else {
  
  Randers<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Randers")) %>% 
    filter (Uoplyst=="Randers")
  
  Randers_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Randers")) %>% 
    filter(Uoplyst=="Randers")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Randers_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Randers_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Randers_streets, file="Randers_streets.Rds")}

#number of distinct street lines
format (nrow (Randers_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Randers <- weight_streetnet (Randers_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Ringkøbing-Skjern
if(file.exists("Ringkøbing-Skjern")) {readRDS("Ringkøbing-Skjern_streets.Rds")
}else {
  
  Ringkøbing-Skjern<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ringkøbing-Skjern")) %>% 
    filter (Uoplyst=="Ringkøbing-Skjern")
  
  Ringkøbing-Skjern_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Ringkøbing-Skjern")) %>% 
    filter(Uoplyst=="Ringkøbing-Skjern")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Ringkøbing-Skjern_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Ringkøbing-Skjern_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Ringkøbing-Skjern_streets, file="Ringkøbing-Skjern_streets.Rds")}

#number of distinct street lines
format (nrow (Ringkøbing-Skjern_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Ringkøbing <- weight_streetnet (Ringkøbing-Skjern_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Samsø
if(file.exists("Samsø")) {readRDS("Samsø_streets.Rds")
}else {
  
  Samsø<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Samsø")) %>% 
    filter (Uoplyst=="Samsø")
  
  Samsø_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Samsø")) %>% 
    filter(Uoplyst=="Samsø")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Samsø_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Samsø_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Samsø_streets, file="Samsø_streets.Rds")}

#number of distinct street lines
format (nrow (Samsø_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Samsø <- weight_streetnet (Samsø_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Silkeborg
if(file.exists("Silkeborg")) {readRDS("Silkeborg_streets.Rds")
}else {
  
  Silkeborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Silkeborg")) %>% 
    filter (Uoplyst=="Silkeborg")
  
  Silkeborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Silkeborg")) %>% 
    filter(Uoplyst=="Silkeborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Silkeborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Silkeborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Silkeborg_streets, file="Silkeborg_streets.Rds")}

#number of distinct street lines
format (nrow (Silkeborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Silkeborg <- weight_streetnet (Silkeborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Skanderborg
if(file.exists("Skanderborg")) {readRDS("Skanderborg_streets.Rds")
}else {
  
  Skanderborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Skanderborg")) %>% 
    filter (Uoplyst=="Skanderborg")
  
  Skanderborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Skanderborg")) %>% 
    filter(Uoplyst=="Skanderborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Skanderborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Skanderborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Skanderborg_streets, file="Skanderborg_streets.Rds")}

#number of distinct street lines
format (nrow (Skanderborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Skanderborg <- weight_streetnet (Skanderborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Skive
if(file.exists("Skive")) {readRDS("Skive_streets.Rds")
}else {
  
  Skive<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Skive")) %>% 
    filter (Uoplyst=="Skive")
  
  Skive_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Skive")) %>% 
    filter(Uoplyst=="Skive")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Skive_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Skive_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Skive_streets, file="Skive_streets.Rds")}

#number of distinct street lines
format (nrow (Skive_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Skive <- weight_streetnet (Skive_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Struer
if(file.exists("Struer")) {readRDS("Struer_streets.Rds")
}else {
  
  Struer<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Struer")) %>% 
    filter (Uoplyst=="Struer")
  
  Struer_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Struer")) %>% 
    filter(Uoplyst=="Struer")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Struer_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Struer_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Struer_streets, file="Struer_streets.Rds")}

#number of distinct street lines
format (nrow (Struer_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Struer <- weight_streetnet (Struer_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Syddjurs
if(file.exists("Syddjurs")) {readRDS("Syddjurs_streets.Rds")
}else {
  
  Syddjurs<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Syddjurs")) %>% 
    filter (Uoplyst=="Syddjurs")
  
  Syddjurs_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Syddjurs")) %>% 
    filter(Uoplyst=="Syddjurs")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Syddjurs_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Syddjurs_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Syddjurs_streets, file="Syddjurs_streets.Rds")}

#number of distinct street lines
format (nrow (Syddjurs_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Syddjurs <- weight_streetnet (Syddjurs_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Viborg
if(file.exists("Viborg")) {readRDS("Viborg_streets.Rds")
}else {
  
  Viborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Viborg")) %>% 
    filter (Uoplyst=="Viborg")
  
  Viborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Viborg")) %>% 
    filter(Uoplyst=="Viborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Viborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Viborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Viborg_streets, file="Viborg_streets.Rds")}

#number of distinct street lines
format (nrow (Viborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Viborg <- weight_streetnet (Viborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Aarhus
if(file.exists("Aarhus")) {readRDS("Aarhus_streets.Rds")
}else {
  
  Aarhus<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Aarhus")) %>% 
    filter (Uoplyst=="Aarhus")
  
  Aarhus_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Aarhus")) %>% 
    filter(Uoplyst=="Aarhus")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Aarhus_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Aarhus_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Aarhus_streets, file="Aarhus_streets.Rds")}

#number of distinct street lines
format (nrow (Aarhus_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Aarhus <- weight_streetnet (Aarhus_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Aalborg
if(file.exists("Aalborg")) {readRDS("Aalborg_streets.Rds")
}else {
  
  Aalborg<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Aalborg")) %>% 
    filter (Uoplyst=="Aalborg")
  
  Aalborg_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Aalborg")) %>% 
    filter(Uoplyst=="Aalborg")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Aalborg_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Aalborg_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Aalborg_streets, file="Aalborg_streets.Rds")}

#number of distinct street lines
format (nrow (Aalborg_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Aalborg <- weight_streetnet (Aalborg_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Brønderslev
if(file.exists("Brønderslev")) {readRDS("Brønderslev_streets.Rds")
}else {
  
  Brønderslev<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Brønderslev")) %>% 
    filter (Uoplyst=="Brønderslev")
  
  Brønderslev_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Brønderslev")) %>% 
    filter(Uoplyst=="Brønderslev")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Brønderslev_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Brønderslev_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Brønderslev_streets, file="Brønderslev_streets.Rds")}

#number of distinct street lines
format (nrow (Brønderslev_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Brønderslev <- weight_streetnet (Brønderslev_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Frederikshavn
if(file.exists("Frederikshavn")) {readRDS("Frederikshavn_streets.Rds")
}else {
  
  Frederikshavn<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederikshavn")) %>% 
    filter (Uoplyst=="Frederikshavn")
  
  Frederikshavn_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Frederikshavn")) %>% 
    filter(Uoplyst=="Frederikshavn")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Frederikshavn_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Frederikshavn_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Frederikshavn_streets, file="Frederikshavn_streets.Rds")}

#number of distinct street lines
format (nrow (Frederikshavn_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Frederikshavn <- weight_streetnet (Frederikshavn_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Hjørring
if(file.exists("Hjørring")) {readRDS("Hjørring_streets.Rds")
}else {
  
  Hjørring<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hjørring")) %>% 
    filter (Uoplyst=="Hjørring")
  
  Hjørring_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Hjørring")) %>% 
    filter(Uoplyst=="Hjørring")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Hjørring_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Hjørring_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Hjørring_streets, file="Hjørring_streets.Rds")}

#number of distinct street lines
format (nrow (Hjørring_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Hjørring <- weight_streetnet (Hjørring_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Jammerbugt
if(file.exists("Jammerbugt")) {readRDS("Jammerbugt_streets.Rds")
}else {
  
  Jammerbugt<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Jammerbugt")) %>% 
    filter (Uoplyst=="Jammerbugt")
  
  Jammerbugt_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Jammerbugt")) %>% 
    filter(Uoplyst=="Jammerbugt")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Jammerbugt_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Jammerbugt_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Jammerbugt_streets, file="Jammerbugt_streets.Rds")}

#number of distinct street lines
format (nrow (Jammerbugt_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Jammerburgt <- weight_streetnet (Jammerbugt_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Læsø
if(file.exists("Læsø")) {readRDS("Læsø_streets.Rds")
}else {
  
  Læsø<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Læsø")) %>% 
    filter (Uoplyst=="Læsø")
  
  Læsø_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Læsø")) %>% 
    filter(Uoplyst=="Læsø")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Læsø_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Læsø_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Læsø_streets, file="Læsø_streets.Rds")}

#number of distinct street lines
format (nrow (Læsø_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Læsø <- weight_streetnet (Læsø_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Mariagerfjord
if(file.exists("Mariagerfjord")) {readRDS("Mariagerfjord_streets.Rds")
}else {
  
  Mariagerfjord<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Mariagerfjord")) %>% 
    filter (Uoplyst=="Mariagerfjord")
  
  Mariagerfjord_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Mariagerfjord")) %>% 
    filter(Uoplyst=="Mariagerfjord")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Mariagerfjord_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Mariagerfjord_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Mariagerfjord_streets, file="Mariagerfjord_streets.Rds")}

#number of distinct street lines
format (nrow (Mariagerfjord_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Mariagerfjord <- weight_streetnet (Mariagerfjord_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Morsø
if(file.exists("Morsø")) {readRDS("Morsø_streets.Rds")
}else {
  
  Morsø<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Morsø")) %>% 
    filter (Uoplyst=="Morsø")
  
  Morsø_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Morsø")) %>% 
    filter(Uoplyst=="Morsø")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Morsø_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Morsø_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Morsø_streets, file="Morsø_streets.Rds")}

#number of distinct street lines
format (nrow (Morsø_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Morsø <- weight_streetnet (Morsø_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Rebild
if(file.exists("Rebild")) {readRDS("Rebild_streets.Rds")
}else {
  
  Rebild<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Rebild")) %>% 
    filter (Uoplyst=="Rebild")
  
  Rebild_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Rebild")) %>% 
    filter(Uoplyst=="Rebild")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Rebild_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Rebild_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Rebild_streets, file="Rebild_streets.Rds")}

#number of distinct street lines
format (nrow (Rebild_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Rebild <- weight_streetnet (Rebild_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Thisted
if(file.exists("Thisted")) {readRDS("Thisted_streets.Rds")
}else {
  
  Thisted<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Thisted")) %>% 
    filter (Uoplyst=="Thisted")
  
  Thisted_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Thisted")) %>% 
    filter(Uoplyst=="Thisted")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Thisted_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Thisted_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Thisted_streets, file="Thisted_streets.Rds")}

#number of distinct street lines
format (nrow (Thisted_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Thisted <- weight_streetnet (Thisted_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"

#
#extract streetnetwork for Vesthimmerlands
if(file.exists("Vesthimmerlands")) {readRDS("Vesthimmerlands_streets.Rds")
}else {
  
  Vesthimmerlands<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vesthimmerlands")) %>% 
    filter (Uoplyst=="Vesthimmerlands")
  
  Vesthimmerlands_sf<-DemoStroke_sf%>% filter(str_detect(Uoplyst,"Vesthimmerlands")) %>% 
    filter(Uoplyst=="Vesthimmerlands")
  
  #bounding polygom
  bounding_polygon <- sf::st_transform(Vesthimmerlands_sf,
                                       sf::st_crs(4326)) %>%
    sf::st_union () %>%
    sf::st_coordinates ()
  bounding_polygon <- bounding_polygon [, 1:2]
  
  Vesthimmerlands_streets <- dodgr_streetnet (bounding_polygon, expand = 0, quiet = FALSE)
  
  #save street network
  saveRDS(Vesthimmerlands_streets, file="Vesthimmerlands_streets.Rds")}

#number of distinct street lines
format (nrow (Vesthimmerlands_streets), big.mark = ",")
#[1] "170,940"

#estimate travel time by distance
net_Vesthimmerlands <- weight_streetnet (Vesthimmerlands_streets, wt_profile = "motorcar")
format (nrow (net), big.mark = ",")
#[1] "1,275,125"