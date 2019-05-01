#Extract spatial data for PPGIS points

require(sf)
require(tidyverse)

#wd <- 
#setwd(wd)


###load data ----
bb <- st_read("D:/Box Sync/Arctic/Data/Boundaries/Borders/By_country/Norway", "North_municipalities_dissolve")
laea_crs <- st_read("D:/Box Sync/Arctic/Data/Boundaries/Borders/By_country/Norway", "AdmBoudariesNorway_lambert") %>%
            st_crs()#for the laea polar crs

ppgis <- st_read("D:/Box Sync/Arctic/Data/PPGIS_CultES/Processed/shps", "PPGIS_Markers_north_UTM33N") %>% 
          st_intersection(bb) %>%
          filter(category %in% c("biological", "cabin", "cleanwater", "cultureident", "gathering", 
                                 "hunt/fish", "income", "otherchange", "pasture", "recreation", 
                                 "scenic", "social", "specialplace", "spiritual", "therapuetic", 
                                 "undisturbnature")) #drop the + and - categories

roads <- st_read("O:/Claire_Big/Arctic/Original/BasemapN500", "n500_roadsl") %>% 
            st_intersection(bb) %>%
            filter(OBJTYPE %in% c("VegSenterlinje", "Bane")) #drop tracks and tractor paths
corrine <- st_read("O:/Claire_Big/Arctic/Original/CORINE2012", "CORINE2012_Norge_ab21a") %>% 
            st_transform(st_crs(ppgis)) %>%
            st_intersection(bb) #%>%
            #st_transform(laea_crs)
elevation <- st_read("O:/Claire_Big/Arctic/Original/BasemapN500", "n500_elevation layersf") %>% 
            st_intersection(bb) %>%
            select(MINHOEYDE) #select the min height column
rivers <- st_read("O:/Claire_Big/Arctic/Original/BasemapN500", "n500_riversl") %>% 
            st_intersection(bb) %>% 
            filter(OBJTYPE=="ElvBekk") #big rivers
lakes <- st_read("O:/Claire_Big/Arctic/Original/N50 Data/Lakes50", "Innsjo_Innsjo") %>% 
            st_intersection(bb) %>% 
            filter(areal_km2 >= 0.02) #lakes bigger than 2ha
towns <- st_read("O:/Claire_Big/Arctic/Original/SSB/Tettsted2015", "Tettsted2015") %>% 
            st_intersection(bb)

#protected areas, infrastructure?


### Spatial overlap ----
#Euclidean distance from nearest road
distancefromroad <- st_distance(ppgis, roads)
dist2road_m <- apply(distancefromroad, 1, min)
names(dist2road_m) <- "dist2road_m"
ppgis_spatial <- cbind(ppgis, dist2road_m)

#Euclidean distance from nearest town
distancefromtown <- st_distance(ppgis, towns)
dist2town_m <- apply(distancefromtown, 1, min)
names(dist2town_m) <- "dist2town_m"
ppgis_spatial <- cbind(ppgis_spatial, dist2town_m)

#Any river or lake >2ha within 500m
distancefromriver <- st_distance(ppgis, rivers)
dist2river_m <- apply(distancefromriver, 1, min)
names(dist2river_m) <- "dist2river_m"
ppgis_spatial <- cbind(ppgis_spatial, dist2river_m)

distancefromlake <- st_distance(ppgis, lakes)
dist2lake_m <- apply(distancefromlake, 1, min)
names(dist2lake_m) <- "dist2lake_m"
ppgis_spatial <- cbind(ppgis_spatial, dist2lake_m)

ppgis_spatial <- ppgis_spatial %>% 
                mutate(waterbodywithin_500m = case_when(dist2lake_m <= 500 | dist2river_m <= 500 ~ TRUE,
                                                  dist2lake_m > 500 | dist2river_m > 500 ~FALSE))

#Elevation (minimum, in 500m intervals)
elev <- st_intersection(ppgis, elevation) %>% 
          st_set_geometry(NULL) %>% 
          select(ID, MINHOEYDE)

ppgis_spatial <- merge(ppgis_spatial, elev, by="ID", all.x=TRUE)
ppgis_spatial$MINHOEYDE[is.na(ppgis_spatial$MINHOEYDE)] <- 0 #replace elevation for points falling in the ocean as elevation=0


#Corrine land use
#make a 1km buffer around each point and calculate it's area. nQuadSegs sets how many straightline segments to dissolve the circle quadrant into (more segments=closer to a true circle).
#not quite as simple as pi*r^2, as the earth is not a perfect sphere, so we let r do it for us.
pointbuff1km <- ppgis %>% 
        #st_transform(st_crs(laea_crs)) %>% #do the calculations in equal area projection
        st_buffer(dist=1000, nQuadSegs=100) %>%
        mutate(circle_area = st_area(.) %>% as.numeric())

#intersect corrine within 1km of each point, calculate the area of each corrine class within each buffered point
lulc <- st_intersection(pointbuff1km, corrine) %>%
        mutate(area = st_area(.) %>% as.numeric()) %>%
        st_set_geometry(NULL) %>%
        mutate(lulc_class = case_when(CLC12_KODE %in% c(311, 313)~"broadleafforest",
                                      CLC12_KODE %in% c(312)~"coniferforest",
                                      CLC12_KODE %in% c(321:324)~"heathshrub",
                                      CLC12_KODE %in% c(331:335)~"sparselyvegetated",
                                      CLC12_KODE %in% c(211, 212, 213, 221, 222, 223, 231, 241:244)~"cropland",
                                      CLC12_KODE %in% c(411, 412, 422, 423)~"wetland",
                                      CLC12_KODE %in% c(112,121, 123, 124, 131, 132, 133)~"developed",#developed=(urban, airports, ports, mines, dumps, construction)
                                      CLC12_KODE %in% c(141, 142)~"urbangreen", 
                                      CLC12_KODE %in% c(511, 512, 521:523)~"water") %>% as.factor) %>%
        group_by(ID, lulc_class) %>%
        summarise(#area_ha_in1km = sum(area/10000),
                  percent_area = sum(area)/mean(circle_area)*100) %>%
        spread(lulc_class, percent_area, fill=0)

ppgis_spatial <- merge(ppgis_spatial, lulc, by="ID", all.x=TRUE)
  
#PCA of landcover variables ----
pca.data <- ppgis_spatial %>% st_set_geometry(NULL) %>% 
            select(c(broadleafforest, cropland, heathshrub, sparselyvegetated, cropland, 
                     wetland, developed, urbangreen, MINHOEYDE))                                                                
pca.envir<-princomp(pca.data, cor=T) # important to remember to normalize the data in PCA, if on different #scales to prevent that certain variables dominate, but the landcoverdata was on same scale as far as I #remember

png("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/PCA_of_landuse.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,2))
  biplot(pca.envir)
  plot(pca.envir)
dev.off()
sink("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/PCA_of_landuse.txt")
loadings(pca.envir)
summary(pca.envir)
sink()
PCA.var<-as.data.frame(pca.envir$scores[,1:3]) #make data frame of PCA variables 
names(PCA.var) <- c("PCA_comp1", "PCA_comp2", "PCA_comp3")

ppgis_spatial <- ppgis_spatial %>% bind_cols(PCA.var)

#Save dataset
ppgis_spatial %>% st_write("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_variables.shp")
ppgis_spatial %>% st_set_geometry(NULL) %>% as.data.frame() %>% write_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_variables.csv")

