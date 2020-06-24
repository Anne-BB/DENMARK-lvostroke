# Helimodel

ems<-5#ems to airborne
speed<-0.25#speed
onsc<-20#onscenetime
dido<-60#DIDO at PSC
ne<-27#dor-needle
gr1<-68#dor-groin mothership
gr2<-41#dor-groin drip and ship

load("europeRGDK.Rda")

europeRGDK <- mutate(europeRGDK, DirectDistanceToNearestCSC   = pmin(DirectDistanceToAarhus,DirectDistanceToOdense,DirectDistanceToBlegdamsvej))

#MODeL MOTHeRSHOP HeLI LVO
europeRGDK<-mutate(europeRGDK, HeliMothershipLVO = (as.numeric(europeRGDK$DirectDistanceToNearestHeli+europeRGDK$DirectDistanceToNearestCSC))*speed+ems+onsc+gr1)
library(tmaptools)

library(tmap)
tmap_mode("view")
europeRGDK_sf<-st_sf(europeRGDK)
HospLocations_sf<-st_sf(HospLocations)
tm_shape(europeRGDK_sf)+tm_polygons("HeliMothershipLVO",palette="inferno", n=o ,title="time to groin at csc with heli and mothershipmodel")


#MODeL MOTHeRSHOP HeLI non-LVO
europeRGDK<-mutate(europeRGDK, HeliMothership_non_LVO = (as.numeric(europeRGDK$DirectDistanceToNearestHeli+europeRGDK$DirectDistanceToNearestCSC))*speed+ems+onsc+ne)
europeRGDK_sf<-st_sf(europeRGDK)
tm_shape(europeRGDK_sf)+tm_polygons("HeliMothership_non_LVO",palette="inferno", n=o ,title="time to IVT at csc with heli and mothershipmodel")

save(europeRGDK, file="eurpeRGDK.RDA")


## DRIP AND SHIP MODELS
europeRGDK<-mutate(europeRGDK, DirectDistanceToNearestPSC =pmin(DirectDistanceToAalborg,DirectDistanceToHolstebro, DirectDistanceToVejle, DirectDistanceToEsbjerg, DirectDistanceToSoenderborg, DirectDistanceToRoskilde, DirectDistanceToGlostrup)) 

#time from nearest psc to nearest csc
#nearest CSC from PSC:
HospLocations <- mutate(HospLocations,
                        DirectDistanceToAarhus = dist_to_loc(geometry,HospLocations["AarhusHospital", ]),
                        DirectDistanceToAalborg     = dist_to_loc(geometry,HospLocations["AalborgHospital", ]),
                        DirectDistanceToHolstebro     = dist_to_loc(geometry,HospLocations["HolstebroHospital", ]),
                       DirectDistanceToVejle     = dist_to_loc(geometry,HospLocations["VejleHospital", ]),
                       DirectDistanceToEsbjerg     = dist_to_loc(geometry,HospLocations["EsbjergHospital", ]),
                       DirectDistanceToSoenderborg = dist_to_loc(geometry,HospLocations["SoenderborgHospital", ]),
                       DirectDistanceToOdense     = dist_to_loc(geometry,HospLocations["OdenseHospital", ]),
                       DirectDistanceToRoskilde     = dist_to_loc(geometry,HospLocations["RoskildeHospital", ]),
                       DirectDistanceToBlegdamsvej     = dist_to_loc(geometry,HospLocations["BlegdamsvejHospital", ]),
                       DirectDistanceToGlostrup     = dist_to_loc(geometry,HospLocations["GlostrupHospital", ]),
                       #
                       DD_from_PSC_to_CSC   = pmin(DirectDistanceToAarhus,
                                                            DirectDistanceToOdense,DirectDistanceToBlegdamsvej)
  )
save(HospLocations, file = "HospLocations.Rda")


hosp<-c("Aarhus","Aalborg","Holstebro","Vejle","Esbjerg","Soenderborg","Odense","Roskilde","Blegdamsvej", "Glostrup")
hosp<-cbind.data.frame(hosp,HospLocations)
save(hosp,file = "hosp.rda")


europeRGDK_centroid <- st_centroid(europeRGDK)
hh<-select(europeRGDK_centroid,DirectDistanceToAarhus,
           DirectDistanceToAalborg,DirectDistanceToHolstebro,
           DirectDistanceToVejle,DirectDistanceToEsbjerg, DirectDistanceToSoenderborg,DirectDistanceToOdense,DirectDistanceToRoskilde,DirectDistanceToBlegdamsvej,DirectDistanceToGlostrup)
hh<-apply(st_drop_geometry(hh),MARGIN=1,which.min)
hh<-c("Aarhus","Aalborg","Holstebro","Vejle","Esbjerg","Soenderborg","Odense","Roskilde","Blegdamsvej", "Glostrup")[hh]
europeRGDK_centroid<-mutate(europeRGDK_centroid, NearestHosp=hh)

DD<-right_join(europeRGDK_centroid,hosp, by=c("NearestHosp"="hosp"))

#MODeL DRIP AND SHIP HeLI LVO
DD$DSHelitimeLVO<-ifelse(DD$DirectDistanceToNearestPSC<DD$DirectDistanceToNearestCSC,
  (ems+as.numeric(DD$DirectDistanceToNearestHeli)*speed
  +onsc
  +as.numeric(DD$DirectDistanceToNearestHosp)*speed+##i think we should try to put in ground here
    dido+
    as.numeric(DD$DD_from_PSC_to_CSC)*speed+
    gr2)
 
    ,DD$HeliMothershipLVO)

DD_sf<-st_sf(DD)
tm_shape(DD_sf)+tm_dots("DSHelitimeLVO",palette="inferno", n=o ,title="time to groin at csc with heli and drip an shipmodel")

  
#MODeL DRIP AND SHIP HELI non-LVO
DD$DSHelitime_non_LVO<-ifelse(DD$DirectDistanceToNearestPSC<DD$DirectDistanceToNearestCSC,ems+as.numeric(DD$DirectDistanceToNearestHeli)*speed
  +onsc
  +as.numeric(DD$DirectDistanceToNearestHosp)*s+##i think we should try to put in ground here
    ne
  
,DD$HeliMothership_non_LVO)

DD_sf<-st_sf(DD)
tm_shape(DD_sf)+tm_dots("DSHelitime_non_LVO",palette="inferno", n=o ,title="time to IVT with heli and drip an shipmodel")



