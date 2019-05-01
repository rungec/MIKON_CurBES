#Models to split users into pro and anti development groups

require(sf)
require(tidyverse)
require(FactoMineR)
require(factoextra)

setwd("D:/Box Sync/Arctic/MIKON/CurBES/Analysis/ppgis_model")


###load data ----
bb <- st_read("D:/Box Sync/Arctic/Data/Boundaries/Borders/By_country/Norway", "North_municipalities_dissolve")

ppgis <- st_read("D:/Box Sync/Arctic/Data/PPGIS_CultES/Processed/shps", "PPGIS_Markers_north_UTM33N") %>% 
  st_intersection(bb) 

###Correspondence analysis ---- 
#Correspondence analysis of user values, based on the proportion of their mapped values that fall into each category
ca.data <- ppgis %>% st_set_geometry(NULL) %>% 
  filter(category %in% c("biological", "cabin", "cleanwater", "cultureident", "gathering", 
                          "hunt/fish", "income", "pasture", "recreation", 
                          "scenic", "social", "specialplace", "spiritual", "therapuetic", 
                          "undisturbnature")) %>% droplevels() %>%
  group_by(userID) %>% 
  mutate(npointsmapped = n()) %>%
  group_by(category, add=TRUE) %>%
  summarise(npoints = n(),
            npointsmapped=mean(npointsmapped),
            proppoints=npoints/npointsmapped) %>%
  ungroup() %>%
  #spread(key=category, value=npoints, fill=0)
  spread(key=userID, value=proppoints, fill=0) %>%
  select(c(userID, biological:undisturbnature))

ca.values<-CA(ca.data, ncp=5, graph=FALSE) 


png("PCA_of_uservalues.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,2))
fviz_screeplot(ca.values)
#fviz_ca_biplot(ca.values, repel=FALSE)
fviz_ca_col(ca.values, repel=TRUE)
dev.off()
sink("PCA_of_uservalues.txt")
#loadings(ca.values)
summary(ca.values)
sink()

#Correspondence analysis of user preferences
user_devpref <- ppgis %>% st_set_geometry(NULL) %>%
  filter(!category %in% c("biological", "cabin", "cleanwater", "cultureident", "gathering", 
                          "hunt/fish", "income", "otherchange", "pasture", "recreation", 
                          "scenic", "social", "specialplace", "spiritual", "therapuetic", 
                          "undisturbnature")) %>% droplevels() %>% #drop the + and - categories 
  mutate(dev_pref = case_when(category %in% c("+fishing", "+hunting", "+grazing", "+logging")~ "+consumptive",
                              category %in% c("-fishing", "-hunting", "-grazing", "-logging")~ "-consumptive",
                              category %in% c("+boats", "+helicopter", "+snowmobiles", "+roads_atv")~ "+motor",
                              category %in% c("-boats", "-helicopter", "-snowmobiles", "-roads_atv")~ "-motor",
                              category %in% c("+development", "+energy", "+tourism")~ "+development",
                              category %in% c("-development", "-energy", "-tourism")~ "-development",
                              category %in% c("+predator")~ "+predator",
                              category %in% c("-predator")~ "-predator")) %>%
  group_by(userID, dev_pref) %>%
  summarise(npoints = n()) %>%
  spread(key=dev_pref, value=npoints, fill=0)
ca.prefs<-princomp(user_devpref, cor=T) 


png("PCA_of_userpreferences.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,2))
biplot(ca.prefs)
plot(ca.prefs)
dev.off()
sink("PCA_of_userpreferences.txt")
loadings(ca.prefs)
summary(ca.prefs)
sink()

direct cluster
redundancy analysis
