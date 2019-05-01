#Plot and model the distance to road for different ES values

#SETUP ----
require(sf)
require(tidyverse)
require(ggplot2)

setwd("D:/Box Sync/Arctic/MIKON/CurBES/Analysis/ppgis_model")

#Load the dataset of ppgis points plus environmental data
#ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_variables")
ppgis_df <- read_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_variables.csv")
#add extra column
ppgis_df$mindist2water_m <- apply(ppgis_df[, c("dist2lake_m", "dist2river_m")], 1, min)

### PLOT THE DATA ----
#how far to water
png("Variable_dist2water_plot.png", width=14, height=7, units="in", res=150)
par(mfrow=c(1,3))
  hist(ppgis_df$dist2lake_m, breaks=100, xlim=c(0,5000))
  hist(ppgis_df$dist2river_m, breaks=100, xlim=c(0,5000))
  hist(ppgis_df$mindist2water_m, breaks=100, xlim=c(0,5000))
dev.off()

#make some plots of the distance to road for the different values
p <- ggplot(ppgis_df, aes(dist2road_m)) +
  geom_freqpoly(binwidth=250) +
  xlim(0,5000) +
  xlab("Distance to road (m)") +
  facet_wrap(vars(category), nrow=4, scales="free_y") +
  theme_minimal()
ggsave("Variable_dist2road_plot_250mbins.png", p)
p <- ggplot(ppgis_df, aes(dist2road_m)) +
  geom_freqpoly(binwidth=100) +
  xlim(0,2000) +
  xlab("Distance to road (m)") +
  facet_wrap(vars(category), nrow=4, scales="free_y") +
  theme_minimal()
ggsave("Variable_dist2road_plot_100mbins.png", p)

#check the covariance of the continous variables
ppgis_cor <- cor(ppgis_df[, c("dist2road_m", "dist2town_m", "dist2river_m", "dist2lake_m", "mindist2water_m", "PCA_comp1", "PCA_comp2", "PCA_comp3")], method="pearson")
png("Variable_correlation_plot.png", width=7, height=7, units="in", res=150)
  corrplot::corrplot.mixed(ppgis_cor, lower.col="black")
dev.off()

### MODEL ----


#So first we need some kind of PCA to split people into groups on whether they support or oppose
#consumptive use, motorised use, development, predator control and how many/what proportion of the different values that they map.
#then we look at the differences in the distance from road that they map different values






#development_preferences ~ dist2road_m + category + userID
#beta regression
#dist2road_m ~ category + ...?
#correspondence aanlysis




