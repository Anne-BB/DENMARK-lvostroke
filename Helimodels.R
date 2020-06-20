# Helimodel

e<-5#ems to airborne
s<-0.25#speed
o<-20#onscenetime
d<-60#DIDO
n<-27#dor-needle
g1<-68#dor-groin mothership
g2<-41#dor-groin drip and ship

load("europeRGDK.Rda")

europeRGDK <- mutate(europeRGDK, DirectDistanceToNearestCSC   = pmin(DirectDistanceToAarhus,DirectDistanceToOdense,DirectDistanceToBlegdamsvej))

#MODEL MOTHERSHOP HELI LVO
europeRGDK<-mutate(europeRGDK, HeliMothershipLVO = (as.numeric(europeRGDK$DirectDistanceToNearestHeli+europeRGDK$DirectDistanceToNearestCSC))*s+o+g1)
#MODEL MOTHERSHOP HELI non-LVO
europeRGDK<-mutate(europeRGDK, HeliMothership_non_LVO = (as.numeric(europeRGDK$DirectDistanceToNearestHeli+europeRGDK$DirectDistanceToNearestCSC))*s+o+n)

save(europeRGDK, file="eurpeRGDK.RDA")

library(tmaptools)

library(tmap)
tmap_mode("view")
europeRGDK_sf<-st_sf(europeRGDK)
HospLocations_sf<-st_sf(HospLocations)
tm_shape(europeRGDK_sf)+tm_polygons("HeliMothershipLVO",palette="inferno", n=o ,title="time to groin at csc with heli and mothershipmodel")

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

#PROBLEMS PROBLEMS:
europeRGDK<-mutate(europeRGDK,D_PSC_CSC = while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToAarhus){HospLocations[1,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToAalborg){HospLocations[2,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToHolstebro){HospLocations[3,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToVejle){HospLocations[4,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToEsbjerg){HospLocations[5,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToSoenderborg){HospLocations[6,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToOdense){HospLocations[7,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToRoskilde){HospLocations[8,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToBlegdamsvej){HospLocations[9,"DD_from_PSC_to_CSC"]},
                     while(europeRGDK$DirectDistanceToNearestHosp==europeRGDK$DirectDistanceToGlostrup){HospLocations[10,"DD_from_PSC_to_CSC"]})

#MODEL DRIP AND SHIP HELI LVO
DSHelitimeLVO<-if(DirectDistanceToNearestPSC<DirectDistanceToNearestCSC){e+as.numeric(europeRGDK$DirectDistanceToNearestHeli)*s
  +o
  +as.numeric(europeRGDK$DirectDistanceToNearestHosp)*s+##i think we should try to put in ground here
    d+
    as.numeric(D_PSC_CSC)*s+
    g2
 
    }else{HeliMothershipLVO}
  
#MODEL DRIP AND SHIP HELI non-LVO
DSHelitime_non_LVO<-if(DirectDistanceToNearestPSC<DirectDistanceToNearestCSC){e+as.numeric(europeRGDK$DirectDistanceToNearestHeli)*s
  +o
  +as.numeric(europeRGDK$DirectDistanceToNearestHosp)*s+##i think we should try to put in ground here
    n
  
}else{HeliMothership_non_LVO}




