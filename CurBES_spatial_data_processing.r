#Extract spatial data for PPGIS points

require(sf)
require(tidyverse)

wd <- 
setwd(wd)


###load data ----
bb <- st_read("D:/Box Sync/Arctic/Data/Boundaries/Borders/By_country/Norway", "North_municipalities_dissolve")

ppgis <- st_read("D:/Box Sync/Arctic/Data/PPGIS_CultES/Processed/shps", "PPGIS_Markers_north_UTM33N") %>% 
          st_intersection(bb) %>%
          filter(category %in% c("biological", "cabin", "cleanwater", "cultureident", "gathering", 
                                 "hunt/fish", "income", "otherchange", "pasture", "recreation", 
                                 "scenic", "social", "specialplace", "spiritual", "therapuetic", 
                                 "undisturbnature")) #drop the + and - categories
participants <- ""

roads <- st_read("O:/Claire_Big/Arctic/Original/BasemapN500", "n500_roadsl") %>% 
            st_intersection(bb) %>%
            filter(OBJTYPE %in% c("VegSenterlinje", "Bane")) #drop tracks and tractor paths
corrine <- st_read("O:/Claire_Big/Arctic/Original/CORINE2012", "CORINE2012_Norge_ab21a") %>% 
            st_transform(st_crs(ppgis)) %>%
            st_intersection(bb)
elevation <- st_read("O:/Claire_Big/Arctic/Original/BasemapN500", "n500_elevation layersf") %>% 
            st_intersection(bb) %>%
            select(MINHOEYDE) #select the min height column
rivers <- st_read("O:/Claire_Big/Arctic/Original/BasemapN500", "n500_riversl") %>% 
            st_intersection(bb) %>% 
            filter(OBJTYPE=="ElvBekk") #big rivers
lakes <- st_read("O:/Claire_Big/Arctic/Original/N50 Data/Lakes50", "Innsjo_Innsjo") %>% 
            st_intersection(bb) %>% 
            filter(areal_km >= 0.02) #lakes bigger than 2ha
towns <- st_read("O:/Claire_Big/Arctic/Original/SSB/Tettsted2015", "Tettsted2015") %>% 
            st_intersection(bb)

#protected areas, infrastructure?


### Spatial overlap ----
#Euclidean distance from nearest road
distancefromroad <- st_distance(ppgis, roads)
dist2road_m <- apply(distancefromroad, 1, min)
names(dist2road_m) <- "dist2road_m"
ppgis_spatial <- bind_cols(ppgis, dist2road_m)

#Euclidean distance from nearest town
distancefromtown <- st_distance(ppgis, towns)
dist2town_m <- apply(distancefromtown, 1, min)
names(dist2town_m) <- "dist2town_m"
ppgis_spatial <- bind_cols(ppgis_spatial, dist2town_m)

#Any river or lake >2ha within 500m
distancefromriver <- st_distance(ppgis, rivers)
dist2river_m <- apply(distancefromriver, 1, min)
names(dist2river_m) <- "dist2river_m"
ppgis_spatial <- bind_cols(ppgis_spatial, dist2river_m)

distancefromlake <- st_distance(ppgis, lakes)
dist2lake_m <- apply(distancefromlake, 1, min)
names(dist2lake_m) <- "dist2lake_m"
ppgis_spatial <- bind_cols(ppgis_spatial, dist2lake_m)

ppgis_spatial <- ppgis_spatial %>% 
                mutate(waterbodywithin_500m = case_when(dist2lake_m <= 500 | dist2river_m <= 500 ~ TRUE,
                                                  dist2lake_m > 500 | dist2river_m > 500 ~FALSE))

#Elevation (minimum, in 500m intervals)
ppgis_spatial <- st_intersection(ppgis_spatial, elevation)

#PCA of corrine variables ----




#Merge ppgis with participant characteristics ----
ppgis_out <- merge(ppgis_spatial, participants, by="userID", all.x=TRUE)


#Save dataset
ppgis_out %>% st_write("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_info.shp")
ppgis_out %>% st_geometry(NULL) %>% write_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_info.csv")


quantile(d2)
mean(d2)
median(d2)
length(d2[d2<1001])
hist(d2)


