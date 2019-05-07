#Models to split users into pro and anti development groups

require(sf)
require(tidyverse)


setwd("D:/Box Sync/Arctic/MIKON/CurBES/Analysis/ppgis_model")

### load data ----
ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_environment")
ppgis_df <- read_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_environment.csv")

### Add socioeconomic data ----
se_df <- readxl::read_xlsx("D:/Box Sync/Arctic/Data/PPGIS_CultES/Original/Participant_characteristics.xlsx", sheet="Participant_sociodem_North", range="A1:J491", na=c("", "NA"))

#select just the se data we are interested in
se_df <- se_df %>% select(LoginID, gender, age, education, income_NOK, adults, children, liveyears) 

#merge with the ppgis data
ppgis_sf_se <- merge(ppgis_sf, se_df, by.x="LogID", by.y="LoginID", all.x=TRUE) 
  

#Save dataset
ppgis_sf_se %>% st_write("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_environment_socioeconomic.shp")
ppgis_df_se <- ppgis_sf_se %>% st_set_geometry(NULL) %>% as.data.frame() 
names(ppgis_df_se)[5:ncol(ppgis_df)] <- names(ppgis_df)[5:ncol(ppgis_df)]
  write_csv(ppgis_df_se, "D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_environment_socioeconomic.csv")

############################
### Correspondence analysis ---- 
  require(FactoMineR)
  require(factoextra)
  
#Correspondence analysis of user values, based on the proportion of their mapped values that fall into each category
ca.data <- ppgis_sf %>% st_set_geometry(NULL) %>% 
  filter(category %in% c("biological", "cabin", "cleanwater", "cultureident", "gathering", 
                          "hunt/fish", "income", "pasture", "recreation", 
                          "scenic", "social", "specialplace", "spiritual", "therapuetic", 
                          "undisturbnature")) %>% droplevels() %>%
  mutate(activity = case_when(category %in% c("biological", "undisturbnature")~ "nature",
                              category %in% c("income", "pasture")~ "grazing",
                              category %in% c("cabin", "social", "cultureident", "gathering")~ "social",
                              category %in% c("specialplace", "spiritual", "therapuetic")~ "special",
                              category %in% c("scenic")~ "scenic",
                              category %in% c("recreation")~ "recreation",
                              category %in% c("cleanwater")~ "cleanwater",
                              category %in% c("hunt/fish")~ "hunt/fish")) %>%
  group_by(userID) %>% 
  mutate(npointsmapped = n()) %>%
  group_by(activity, add=TRUE) %>%
  summarise(npoints = n(),
            npointsmapped=mean(npointsmapped),
            proppoints=npoints/npointsmapped) %>%
  ungroup() %>%
  select(userID, activity, proppoints) %>% 
  spread(key=activity, value=proppoints, fill=0) 

ca.values<-CA(ca.data[,2:ncol(ca.data)], ncp=5, graph=FALSE) 

png("PCA_of_uservalues.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,2))
fviz_screeplot(ca.values)
#fviz_ca_biplot(ca.values, repel=FALSE)
fviz_ca_col(ca.values, repel=TRUE)
dev.off()
sink("PCA_of_uservalues.txt")
summary(ca.values)
sink()

#Correspondence analysis of user preferences
user_devpref <- ppgis_sf %>% st_set_geometry(NULL) %>%
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

user_devpref %>% as.data.frame() %>% write_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_userdevelopmentpreferences_npoints.csv")

ca.prefs<-CA(user_devpref[, 2:ncol(user_devpref)], ncp=5, graph=FALSE) 


png("PCA_of_userpreferences.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,2))
fviz_screeplot(ca.prefs)
#fviz_ca_biplot(ca.prefs, repel=FALSE)
fviz_ca_col(ca.prefs, repel=TRUE)
dev.off()
sink("PCA_of_userpreferences.txt")
summary(ca.prefs)
sink()

#Correspondence analysis of user values and preferences, based on the proportion of their mapped values that fall into each category
ca.data.comb <- ppgis_sf %>% st_set_geometry(NULL) %>% 
  filter(category!="otherchange") %>%
  mutate(activity = case_when(category %in% c("biological", "undisturbnature")~ "nature",
                              category %in% c("income", "pasture")~ "grazing",
                              category %in% c("cabin", "social", "cultureident", "gathering")~ "social",
                              category %in% c("specialplace", "spiritual", "therapuetic")~ "special",
                              category %in% c("scenic")~ "scenic",
                              category %in% c("recreation")~ "recreation",
                              category %in% c("cleanwater")~ "cleanwater",
                              category %in% c("hunt/fish")~ "hunt/fish", 
                              category %in% c("+fishing", "+hunting", "+grazing", "+logging")~ "+consumptive",
                              category %in% c("-fishing", "-hunting", "-grazing", "-logging")~ "-consumptive",
                              category %in% c("+boats", "+helicopter", "+snowmobiles", "+roads_atv")~ "+motor",
                              category %in% c("-boats", "-helicopter", "-snowmobiles", "-roads_atv")~ "-motor",
                              category %in% c("+development", "+energy", "+tourism")~ "+development",
                              category %in% c("-development", "-energy", "-tourism")~ "-development",
                              category %in% c("+predator")~ "+predator",
                              category %in% c("-predator")~ "-predator")) %>%
  group_by(userID) %>% 
  mutate(npointsmapped = n()) %>%
  group_by(activity, add=TRUE) %>%
  summarise(npoints = n(),
            npointsmapped=mean(npointsmapped),
            proppoints=npoints/npointsmapped) %>%
  ungroup() %>%
  select(userID, activity, npoints) %>% 
  spread(key=activity, value=npoints, fill=0) 

ca.all<-CA(ca.data.comb[,2:ncol(ca.data.comb)], ncp=5, graph=FALSE) 

png("PCA_of_uservaluesandprefs.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,2))
fviz_screeplot(ca.all)
#fviz_ca_biplot(ca.all, repel=FALSE)
fviz_ca_col(ca.all, repel=TRUE)
dev.off()
sink("PCA_of_uservaluesandprefs.txt")
summary(ca.all)
sink()



