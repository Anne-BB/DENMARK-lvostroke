# Helimodel

load("europeRGDK.Rda")

europeRGDK <- mutate(europeRGDK, DirectDistanceToNearestCSC   = pmin(DirectDistanceToAarhus,DirectDistanceToOdense,DirectDistanceToBlegdamsvej  )
)
#CSC
europeRGDK<-mutate(europeRGDK, HeliMothership = (europeRGDK$DirectDistanceToNearestHeli+europeRGDK$DirectDistanceToNearestCSC))
europeRGDK<-mutate(europeRGDK, TimeHeliMothership = HeliMothership*0.25)
europeRGDK$TimeHeliMothership= as.numeric(europeRGDK$TimeHeliMothership)
europeRGDK$TimeHeliMothership=europeRGDK$TimeHeliMothership+20
#PSC
europeRGDK<-mutate(europeRGDK, HeliDripShip =(europeRGDK$DirectDistanceToNearestHeli+europeRGDK$DirectDistanceToNearestHosp))

save(europeRGDK, file="eurpeRGDK.RDA")

library(tmaptools)

library(tmap)
tmap_mode("view")
europeRGDK_sf<-st_sf(europeRGDK)
tm_shape(europeRGDK_sf)+tm_polygons("TimeHeliMothership",palette="inferno", n=20 ,title="time to csc with heli and mothershipmodel")


