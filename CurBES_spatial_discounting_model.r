#Plot and model the distance to road for different ES values

#SETUP ----
require(sf)
require(tidyverse)
require(ggplot2)
require(modelr) #add_predictions

setwd("D:/Box Sync/Arctic/MIKON/CurBES/Analysis/ppgis_model")

#Load the dataset of ppgis points plus environmental data
#ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_environment_socioeconomic")
ppgis_df <- read_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_environment_socioeconomic.csv")

#add extra column
#ppgis_df$mindist2water_m <- apply(ppgis_df[, c("dist2lake_m", "dist2river_m")], 1, min)

### Drop preferences
ppgis_df <- ppgis_df %>%  
  filter(category %in% c("biological", "cabin", "cleanwater", "cultureident", "gathering", 
                         "hunt/fish", "recreation", 
                         "scenic", "social", "specialplace", "spiritual", "therapuetic", 
                         "undisturbnature")) %>% droplevels() #dropped income and pasture

### Group the values categories into different activities to reduce the number of categories
ppgis_df <- ppgis_df %>%  
  mutate(activity = case_when(category %in% c("biological")~ "biological",
                              category %in% c("undisturbnature")~ "undisturbnature", 
                              #category %in% c("income", "pasture")~ "grazing",
                              category %in% c("gathering") ~ "gathering",
                              category %in% c("cabin", "social", "cultureident", "specialplace")~ "culture",
                              category %in% c("spiritual", "therapuetic")~ "spiritual and therapeutic",
                              category %in% c("scenic")~ "scenic",
                              category %in% c("recreation")~ "recreation",
                              category %in% c("cleanwater")~ "cleanwater",
                              category %in% c("hunt/fish")~ "hunt/fish"))

#Function to calculate r-squared
rsq <- function (x, y) cor(x, y) ^ 2

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

### MODEL DIST2ROAD by ES CATEGORY ----
#Is there any difference in the distance from roads at which points representing different values are mapped? 

#First we run a model to show that distance from road explains most of the variance in the data
#Set up data
ppgis_freq <- ppgis_df %>% 
  mutate(dist2road_round = round(dist2road_m, digits=-2)) %>% #round to nearest 100m (5m=>0m, 6m=>10m)
  group_by(activity, dist2road_round) %>%
  summarise(frequ = n()) %>%
  ungroup() %>%
  mutate(activity = as.factor(activity))

#model distance to road
mod2 <- nls(frequ ~ exp(a+b*dist2road_round), data=ppgis_freq, start=list(a=0, b=0))
ppgis_freq <- ppgis_freq %>% add_predictions(mod2) %>% add_residuals(mod2)

sink("Model_of_dist2road_byactivity_nls_100m.txt")
print("NLS model of dist2road in 100m blocks ignoring activity")
summary(mod2)
print(paste0("Deviance (RSS): ", deviance(mod2)))
logLik(mod2)
print(paste0("R-squared: ", rsq(ppgis_freq$frequ, ppgis_freq$pred) ))
print(paste0("MSE: ", deviance(mod2)/length(ppgis_freq$resid) ))
print(paste0("RMSE: ", sqrt(mean(ppgis_freq$resid^2)) )) 
sink()

#Plot the model fit
p <- ggplot(ppgis_freq, aes(y=frequ, x=dist2road_round/1000))+
  geom_point() +
  geom_line(aes(y=pred), col="red") +
  xlab("Distance to road (km)") + ylab("Frequency of mapped points") +
  theme_minimal()
ggsave("Model_of_dist2road_nls_100m_fit.png", p)


#Option 1: split landscape into plots (based on distance from road) and count frequency of points in each plot
#We ignore other environmental variables as we know that accessibility is by far the biggest driver - environmental variables only account for a small proportion of the variation
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
#they do.

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
print(paste0("Deviance (RSS): ", deviance(mod2a)))
sink()

#Model 3 non-linear least-squares 
#https://datascienceplus.com/second-step-with-non-linear-regression-adding-predictors/
#basically this fits separate nls models on each activity
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
                      add_predictions(mod3) %>% 
                      mutate(pred3 = pred) %>%
                      add_residuals(mod3) %>%
                      add_predictions(mod2a) %>%
                      mutate(pred1 = pred)

sink("Model_of_dist2road_byactivity_nls.txt", append=TRUE)
print(paste0("R-squared of model 3: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred3) ))
print(paste0("R-squared of model 2a: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred1) ))
sink()

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

#Could also do kolmogorov-smirnov test
subdf <- ppgis_freq_preds %>% filter(activity=="nature")
#is the frequency of nature different from the model2a predictions
ks.test(subdf$frequ, subdf$pred1)
#is the model3 predictions different from the model2a predictions
ks.test(subdf$pred3, subdf$pred1)
#but I'm not sure how this works when we have different numbers of mapped points - can we compare different activities?
#also it doesn't take into account the order of frequency points
subdf2 <- ppgis_freq_preds %>% filter(activity=="cleanwater")
ks.test(subdf$pred3, subdf2$pred3)


### MODEL DIST2ROAD by SOCIOECONOMIC CLASS ----
#Are different types of people more likely to map certain values further from roads?

#We draw on Sigrid Engens paper on what types of people support or oppose development
#(preferences we can choose from are consumptive use, motorised use, development, predator control).
#then we look at the differences in the distance from road that they map different values
#She found that men were significantly higher than women to be in favor of activities
#and that age (favor decreased by age) and education (higher=less in favor) had marginally significant effects

#dist2road_m ~ gender + age...?

#Model 4 non-linear least-squares 
#https://datascienceplus.com/second-step-with-non-linear-regression-adding-predictors/
#basically this fits separate nls models on each gender
#Set up data
ppgis_freq <- ppgis_df %>% 
  mutate(dist2road_round = round(dist2road_m, digits=-1)) %>% #round to nearest 10m (5m=>0m, 6m=>10m)
  group_by(gender, dist2road_round) %>%
  summarise(frequ = n()) %>%
  ungroup() %>%
  mutate(gender = as.factor(gender))

lf <- formula(frequ ~ exp(a+b*dist2road_round) | gender)
mod4 <- nlme::nlsList(lf, data=ppgis_freq, start=list(a=coef(mod2a)[1], b=coef(mod2a)[2]))
sink("Model_of_dist2road_bysocioeconomics_nls.txt", append=TRUE)
print("NLS model of dist2road by gender")
summary(mod4)
sink()
x <-as.data.frame.table(summary(mod4)$coefficients) %>% spread(key = Var2, value = Freq)
names(x)[1:2] <- c("gender", "coef")
write.csv(x, "Model_of_dist2road_bygender_nls_coefs.csv", row.names=FALSE)

### Plot model 4 ----
ppgis_freq_preds <- ppgis_freq %>% 
  add_predictions(mod4) %>% 
  add_residuals(mod4) 

sink("Model_of_dist2road_bygender_nls.txt", append=TRUE)
print(paste0("R-squared of model 4: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred) ))
sink()

#Plot boxplot of residuals by gender
#plot(mod3, activity ~ resid(.))
p <- ggplot(ppgis_freq_preds) +
  geom_boxplot(aes(gender, resid)) +
  ylab("Model residuals") + xlab("Gender") +
  theme_minimal() 
ggsave("Model_of_dist2road_bygender_nls_residuals.png", p)

#Plot residuals vs fitted values by activity
#plot(mod3, resid(.) ~ fitted(.) | activity, abline=0)
p <- ggplot(ppgis_freq_preds, aes(pred, resid)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=0,intercept=0) +
  xlab("Fitted values") + ylab("Model residuals") +
  facet_wrap("gender", scales="free_x") 
ggsave("Model_of_dist2road_bygender_nls_residuals_vs_fitted.png", p)

#Plot frequency against fitted by activity
#plot(mod3, frequ ~ fitted(.) | activity, abline = c(0,1), col="black")
p <- ggplot(ppgis_freq_preds, aes(pred, frequ)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=1,intercept=0) +
  xlab("Fitted values") + ylab("Frequency") +
  facet_wrap("gender", scales="free") +
  ggsave("Model_of_dist2road_bygender_nls_frequ_vs_fitted.png", p)

#Plot the model fit
p <- ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round/1000, group=gender))+
  geom_point() +
  geom_line(aes(y=pred), col="red") +
  facet_wrap("gender", scales = "free_y") +
  xlab("Distance to road (km)") + ylab("Frequency of mapped points") +
  theme_minimal()
ggsave("Model_of_dist2road_bygender_nls_fit.png", p)

