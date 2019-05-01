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

### Group the values categories into different activities to reduce the number of categories
ppgis_df <- ppgis_df %>%  
  filter(category!="otherchange") %>% droplevels() %>%
  mutate(activity = case_when(category %in% c("biological", "undisturbnature")~ "nature",
                              category %in% c("income", "pasture")~ "grazing",
                              category %in% c("cabin", "social", "cultureident", "gathering")~ "social",
                              category %in% c("specialplace", "spiritual", "therapuetic")~ "special",
                              category %in% c("scenic")~ "scenic",
                              category %in% c("recreation")~ "recreation",
                              category %in% c("cleanwater")~ "cleanwater",
                              category %in% c("hunt/fish")~ "hunt/fish"))

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
#Is there any difference in the distance from roads at which points representing different values are mapped? 

#Option 1: split landscape into plots (based on distance from road) and count frequency of points in each plot
#...but we lose information about other env variables or have to work out how to aggregate them
#Set up data
ppgis_freq <- ppgis_df %>% 
              mutate(dist2road_round = round(dist2road_m, digits=-1)) %>% #round to nearest 10m (5m=>0m, 6m=>10m)
              group_by(activity, dist2road_round) %>%
              summarise(frequ = n()) %>%
              ungroup() %>%
              mutate(activity = as.factor(activity))
 
#First lets run a chi-squared test to see if the different activities have different distributions
sink("Model_of_dist2road_byactivity_nls.txt")
print("Chi-sq test: Does frequency change with activity")
chisq.test(ppgis_freq$frequ, ppgis_freq$activity)
sink()
             
#Model 1 glm frequ~exp(a+b*dist2road_round) + activity
mod1 <- glm(log(frequ) ~ log(dist2road_round+1)*activity, data=ppgis_freq)
summary(mod1)
#plot(mod1)
#this didn't represent the data well

#Model 2 nls 
#first run a model without activity to determine the starting parameters a and b
mod2a <- nls(frequ ~ exp(a+b*dist2road_round), data=ppgis_freq, start=list(a=0, b=0))
sink("Model_of_dist2road_byactivity_nls.txt", append=TRUE)
print("NLS model of dist2road ignoring activity")
summary(mod2a)
sink()

#ppgis_freq_sub <- ppgis_freq %>% filter(activity=="cleanwater")
#mod2b <- lm(frequ ~ exp(1.62646+-0.0004147536*dist2road_round), data=ppgis_freq_sub)
#BIC(mod2b)
#BIC(mod3[[1]])

#Model 3 non-linear least-squares 
#https://datascienceplus.com/second-step-with-non-linear-regression-adding-predictors/
#basically this fits separeate nls models on each activity
lf <- formula(frequ ~ exp(a+b*dist2road_round) | activity)
mod3 <- nlme::nlsList(lf, data=ppgis_freq, start=list(a=coef(mod2a)[1], b=coef(mod2a)[2]))
sink("Model_of_dist2road_byactivity_nls.txt", append=TRUE)
print("NLS model of dist2road by activity")
summary(mod3)
sink()
x <-as.data.frame.table(summary(mod3)$coefficients) %>% spread(key = Var2, value = Freq)
names(x)[1:2] <- c("activity", "coef")
write.csv(x, "Model_of_dist2road_byactivity_nls_coefs.csv", row.names=FALSE)

### Plot model 3 ----
ppgis_freq_preds <- ppgis_freq %>% 
                      modelr::add_predictions(mod3) %>% 
                      mutate(pred3 = pred) %>%
                      modelr::add_residuals(mod3) #%>%
                     # modelr::add_predictions(mod1) %>%
                      #mutate(pred1 = pred)


#Plot boxplot of residuals by activity
#plot(mod3, activity ~ resid(.))
p <- ggplot(ppgis_freq_preds) +
  geom_boxplot(aes(activity, resid)) +
  ylab("Model residuals") + xlab("Activity") +
  theme_minimal() 
ggsave("Model_of_dist2road_byactivity_nls_residuals.png", p)

#Plot residuals vs fitted values by activity
#plot(mod3, resid(.) ~ fitted(.) | activity, abline=0)
p <- ggplot(ppgis_freq_preds, aes(pred3, resid)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=0,intercept=0) +
  xlab("Fitted values") + ylab("Model residuals") +
  facet_wrap("activity", nrow=2, scales="free_x") 
ggsave("Model_of_dist2road_byactivity_nls_residuals_vs_fitted.png", p)

#Plot frequency against fitted by activity
#plot(mod3, frequ ~ fitted(.) | activity, abline = c(0,1), col="black")
p <- ggplot(ppgis_freq_preds, aes(pred3, frequ)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=1,intercept=0) +
  xlab("Fitted values") + ylab("Frequency") +
  facet_wrap("activity", nrow=2, scales="free") +
ggsave("Model_of_dist2road_byactivity_nls_frequ_vs_fitted.png", p)

#Plot the model fit
p <- ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round/1000, group=activity))+
  geom_point() +
  geom_line(aes(y=pred3), col="red") +
  #geom_line(aes(y=pred1), col="blue") +
  facet_wrap("activity", scales = "free_y") +
  xlab("Distance to road (km)") + ylab("Frequency of mapped points") +
  theme_minimal()
ggsave("Model_of_dist2road_byactivity_nls_fit.png", p)








#beta regression? but essentially this is presence only data
#dist2road_m ~ activity + ...?

#Are different types of people more likely to map certain values?

#So first we need some kind of PCA to split people into groups on whether they support or oppose
#consumptive use, motorised use, development, predator control and how many/what proportion of the different values that they map.
#then we look at the differences in the distance from road that they map different values

ppgis_freq_preds <- ppgis_freq %>% modelr::add_predictions(mod2a)

ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round, group=activity))+
  geom_point() +
  geom_line(aes(y=pred), col="red") +
  facet_wrap("activity", scales = "free_y") +
  xlim(0,100)

#development_preferences ~ dist2road_m + category + userID
#beta regression
#dist2road_m ~ category + ...?
#correspondence aanlysis




