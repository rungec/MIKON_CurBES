#Plot and model the distance to road for different ES values

require(sf)
require(tidyverse)
require(ggplot2)

#wd <- 
#setwd(wd)





#Load the dataset of ppgis points plus environmental data
ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_variables")
ppgis_df <- read_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_variables.csv")



#make some plots of the distance to road for the different values
p <- ggplot(ppgis_df, aes(x=dist2road_m) 





#how will we structure the model?

quantile(d2)
mean(d2)
median(d2)
length(d2[d2<1001])
hist(d2)

#check the covariance of the continous variables