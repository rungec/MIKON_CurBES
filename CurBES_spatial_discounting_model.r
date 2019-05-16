#Plot and model the distance to road for different ES values

#SETUP ----
require(sf)
require(tidyverse)
require(ggplot2)
require(modelr) #add_predictions
#require(mgcv) #gams
require(lme4) #glmer

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
  mutate(activity = case_when(category %in% c("biological")~ "Biological",
                              category %in% c("undisturbnature")~ "Undisturbed nature", 
                              #category %in% c("income", "pasture")~ "grazing",
                              category %in% c("gathering") ~ "Gathering",
                              category %in% c("cabin", "social", "cultureident", "specialplace")~ "Culture",
                              category %in% c("spiritual", "therapuetic")~ "Spiritual and therapeutic",
                              category %in% c("scenic")~ "Scenic",
                              category %in% c("recreation")~ "Recreation",
                              category %in% c("cleanwater")~ "Cleanwater",
                              category %in% c("hunt/fish")~ "Hunting or fishing"),
         income = case_when(income_NOK == "more600" ~ "more600",
                            income_NOK!= "more600" ~ "less600"))


#Function to calculate r-squared
rsq <- function (x, y) cor(x, y) ^ 2
#Function to calculate AICc
AICc<-function(m1) {
      K=summary(m1)$df[1]
      n=summary(m1)$df[2]
      AICc = AIC(m1) + 2*K*(K+1)/(n-K-1)
      return(AICc)
}

###############################
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

#Plot distance by socioeconomics
#by gender
ppgis_sub <- ppgis_df %>% drop_na(gender, age, education, income_NOK) %>% 
  group_by(LogID, activity, gender, age, education, income_NOK) %>%
  summarise(mean_dist = mean(dist2road_m))

ppgis_sub$education <- factor(ppgis_sub$education, levels=c("primary", "secondary", "higher"))
p <- ggplot(ppgis_sub, aes(y=mean_dist/1000, x=activity))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("PPGIS_meandistancetoroad_byactivity_and_gender.png", p)
#by education
p <- ggplot(ppgis_sub, aes(y=mean_dist/1000, x=activity))+
  geom_boxplot(aes(col=education)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("PPGIS_meandistancetoroad_byactivity_and_education.png", p)
#by education and gender
p <- ggplot(ppgis_sub, aes(y=mean_dist/1000, x=education))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("Highest education") +
  theme_minimal() 
ggsave("PPGIS_meandistancetoroad_bygender_and_education.png", p)
#by age
p <- ggplot(ppgis_sub, aes(y=mean_dist/1000, x=age))+
  geom_boxplot(aes(group=age)) + 
  ylab("Mean distance to road (km)") + xlab("Age") +
  facet_wrap("gender") +
  theme_minimal()  
ggsave("PPGIS_meandistancetoroad_byage_and_gender.png", p)

#check the covariance of the continous variables
ppgis_cor <- cor(ppgis_df[, c("dist2road_m", "dist2town_m", "dist2river_m", "dist2lake_m", "mindist2water_m", "PCA_comp1", "PCA_comp2", "PCA_comp3")], method="pearson")
png("Variable_correlation_plot.png", width=7, height=7, units="in", res=150)
  corrplot::corrplot.mixed(ppgis_cor, lower.col="black")
dev.off()

###############################
### MODEL DIST2ROAD by ES CATEGORY ----
#Is there any difference in the distance from roads at which points representing different values are mapped? 

#First we run a model to show that distance from road explains most of the variance in the data
#Set up data
breaktbl <- data.frame(dist2road_round = rep(seq(0, round(max(ppgis_df$dist2road_m), digits=-2), 100), each=length(unique(ppgis_df$activity))),
                        activity=rep(unique(ppgis_df$activity)), stringsAsFactors=FALSE)

ppgis_freq <- ppgis_df %>% 
  mutate(dist2road_round = round(dist2road_m-45, digits=-2)) %>% #round to nearest 100m (9m=>0m, 10m=>10m)
  group_by(activity, dist2road_round) %>%
  summarise(frequ = n()) %>%
  ungroup() %>%
  right_join(breaktbl) %>%
  mutate(frequ = ifelse(is.na(frequ), 0, frequ),
          activity = as.factor(activity))

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
breaktbl <- data.frame(dist2road_round = rep(seq(0, round(max(ppgis_df$dist2road_m), digits=-1), 10), each=length(unique(ppgis_df$activity))),
                       activity=rep(unique(ppgis_df$activity)), stringsAsFactors=FALSE)

ppgis_freq <- ppgis_df %>% 
              mutate(dist2road_round = round(dist2road_m-4.5, digits=-1)) %>% #round to nearest 10m (9m=>0m, 10m=>10m)
              group_by(activity, dist2road_round) %>%
              summarise(frequ = n()) %>%
              ungroup() %>%
              right_join(breaktbl) %>%
              mutate(frequ = ifelse(is.na(frequ), 0, frequ),
                    activity = as.factor(activity))
 
#First lets run a chi-squared test to see if the different activities have different distributions
sink("Model_of_dist2road_byactivity_nls.txt")
print("Chi-sq test: Does frequency change with activity")
chisq.test(ppgis_freq$frequ, ppgis_freq$activity)
sink()
#they do.

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
#This model gives exactly the same results as 
g1 <- glm(frequ ~ activity*dist2road_round - 1, data = ppgis_freq, family = gaussian(link = "log"), start = rep(0, 18))
#but is easier to interpret the coefficients

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
ppgis_freq_preds <- ppgis_freq %>% arrange(activity) %>%
                      add_predictions(mod3) %>% 
                      mutate(pred3 = pred) %>%
                      add_residuals(mod3) %>%
                      add_predictions(mod2a) %>% 
                      mutate(pred2a = pred) 

sink("Model_of_dist2road_byactivity_nls.txt", append=TRUE)
print(paste0("R-squared of model 3: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred3) ))
print(paste0("R-squared of model 2a: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred2a) ))
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
  facet_wrap("activity", nrow=3, scales="free_x") +
  theme_minimal(16)
ggsave("Model_of_dist2road_byactivity_nls_residuals_vs_fitted.png", p)

#Plot frequency against fitted by activity
#plot(mod3, frequ ~ fitted(.) | activity, abline = c(0,1), col="black")
p <- ggplot(ppgis_freq_preds, aes(pred3, frequ)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=1,intercept=0) +
  xlab("Fitted values") + ylab("Frequency") +
  facet_wrap("activity", nrow=3, scales="free") 
ggsave("Model_of_dist2road_byactivity_nls_frequ_vs_fitted.png", p)

#Plot residuals against distance to road by activity
p <- ggplot(ppgis_freq_preds, aes(dist2road_round/1000, resid)) +
  geom_point(col="grey50", pch=1) + #geom_abline(slope=1,intercept=0) +
  xlab("Distance to road (km)") + ylab("Model residuals") +
  facet_wrap("activity", nrow=3, scales="free") +
  theme_minimal(16)
ggsave("Model_of_dist2road_byactivity_nls_dist2road_vs_residuals.png", p,width=11, height=7, units="in")
#Plot mean residuals against distance to road by activity
ppgis_freq_preds_sub <- ppgis_freq_preds %>% 
  filter(activity %in% c("Culture", "Recreation", "Scenic", "Hunting or fishing")) %>%
  mutate(dist2road_100m = round(dist2road_round-45, digits=-2)) %>%
  group_by(activity, dist2road_100m) %>%
  summarise(mean_resid = mean(resid))
p <- ggplot(ppgis_freq_preds_sub, aes(dist2road_100m/1000, mean_resid)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=0,intercept=0) +
  xlab("Distance to road (km)") + ylab("Mean of residuals") +
  facet_wrap("activity", scales="free") +
  theme_minimal(16)
ggsave("Model_of_dist2road_byactivity_nls_dist2road_vs_meanresiduals.png", p,width=9, height=6, units="in")

#Plot the model fit
p <- ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round/1000, group=activity))+
  geom_point(col="grey70") +
  geom_line(aes(y=pred3), col="red") +
  #geom_line(aes(y=pred1), col="blue") +
  facet_wrap("activity", scales = "free_y") +
  xlab("Distance to road (km)") + ylab("Frequency of mapped points") +
  theme_minimal(16) 
ggsave("Model_of_dist2road_byactivity_nls_fit.png", p, width=11, height=7, units="in")
#Plot the model fit zoomed
p <- ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round, group=activity))+
  geom_point(col="grey70") +
  geom_line(aes(y=pred3), col="red") +
  xlim(0,1000) +
  #geom_line(aes(y=pred1), col="blue") +
  facet_wrap("activity", scales = "free_y") +
  xlab("Distance to road (m)") + ylab("Frequency of mapped points") +
  theme_minimal(16) 
ggsave("Model_of_dist2road_byactivity_nls_fit_zoom.png", p, width=11, height=7, units="in")

#Could also do kolmogorov-smirnov test
subdf <- ppgis_freq_preds %>% filter(activity=="recreation")
#is the frequency of nature different from the model2a predictions
ks.test(subdf$frequ, subdf$pred2a)
#is the model3 predictions different from the model2a predictions
ks.test(subdf$pred3, subdf$pred2a)
#but I'm not sure how this works when we have different numbers of mapped points - can we compare different activities?
#also it doesn't take into account the order of frequency points
subdf2 <- ppgis_freq_preds %>% filter(activity=="cleanwater")
ks.test(subdf$pred3, subdf2$pred3)

###############################
### MODEL DIST2ROAD by SOCIOECONOMIC CLASS ----
#Are different types of people more likely to map certain values further from roads?

#We draw on Sigrid Engens paper on what types of people support or oppose development
#(preferences we can choose from are consumptive use, motorised use, development, predator control).
#then we look at the differences in the distance from road that they map different values
#She found that men were significantly higher than women to be in favor of activities
#and that age (favor decreased by age) and education (higher=less in favor) had marginally significant effects

#Model 4 non-linear least-squares - gender only
#https://datascienceplus.com/second-step-with-non-linear-regression-adding-predictors/
#basically this fits separate nls models on each gender
#Set up data
breaktbl <- data.frame(dist2road_round = rep(seq(0, round(max(ppgis_df$dist2road_m), digits=-1), 10), each=length(unique(ppgis_df$gender))),
                       gender=rep(unique(ppgis_df$gender)), stringsAsFactors = FALSE)

ppgis_freq <- ppgis_df %>% 
  mutate(dist2road_round = round(dist2road_m-4.5, digits=-1)) %>% #round to nearest 10m (9m=>0m, 10m=>10m)
  group_by(gender, dist2road_round) %>%
  summarise(frequ = n()) %>%
  ungroup() %>%
  right_join(breaktbl) %>%
  mutate(frequ = ifelse(is.na(frequ), 0, frequ),
         gender = as.factor(gender)) %>%
  filter(!is.na(gender)) 

lf <- formula(frequ ~ exp(a+b*dist2road_round) | gender)
mod4 <- nlme::nlsList(lf, data=ppgis_freq, start=list(a=0, b=0))
sink("Model_of_dist2road_bygender_nls.txt")
print("NLS model of dist2road by gender")
summary(mod4)
sink()
x <-as.data.frame.table(summary(mod4)$coefficients) %>% spread(key = Var2, value = Freq)
names(x)[1:2] <- c("gender", "coef")
write.csv(x, "Model_of_dist2road_bygender_nls_coefs.csv", row.names=FALSE)

### Plot model 4 ----
ppgis_freq_preds <- ppgis_freq %>% 
  arrange(gender) %>%
  add_predictions(mod4) %>% 
  add_residuals(mod4) 

sink("Model_of_dist2road_bygender_nls.txt", append=TRUE)
print(paste0("R-squared of model 4: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred) ))
sink()

#Plot the model fit
p <- ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round/1000, group=gender))+
  geom_point() +
  geom_line(aes(y=pred, col=gender)) +
  xlab("Distance to road (km)") + ylab("Frequency of mapped points") +
  theme_minimal()
ggsave("Model_of_dist2road_bygender_nls_fit.png", p)

#Is there still a difference after we drop primary educated?
ppgis_freq <- ppgis_df %>% filter(education!="primary") %>%
  mutate(dist2road_round = round(dist2road_m-4.5, digits=-1)) %>% #round to nearest 10m (9m=>0m, 10m=>10m)
  group_by(gender, dist2road_round) %>%
  summarise(frequ = n()) %>%
  ungroup() %>%
  right_join(breaktbl) %>%
  mutate(frequ = ifelse(is.na(frequ), 0, frequ),
         gender = as.factor(gender)) %>%
  filter(!is.na(gender))   

lf <- formula(frequ ~ exp(a+b*dist2road_round) | gender)
mod5 <- nlme::nlsList(lf, data=ppgis_freq, start=list(a=0, b=0))
sink("Model_of_dist2road_bygender_nls_noprimaryeducated.txt")
print("NLS model of dist2road by gender")
summary(mod5)
sink()

ppgis_freq_preds <- ppgis_freq %>% 
  arrange(gender) %>%
  add_predictions(mod5) %>% 
  add_residuals(mod5) 

sink("Model_of_dist2road_bygender_nls_noprimaryeducated.txt", append=TRUE)
print(paste0("R-squared of model 5: ", rsq(ppgis_freq_preds$frequ, ppgis_freq_preds$pred) ))
sink()

#Plot the model fit
p <- ggplot(ppgis_freq_preds, aes(y=frequ, x=dist2road_round/1000, group=gender))+
  geom_point() +
  geom_line(aes(y=pred, col=gender)) +
  xlab("Distance to road (km)") + ylab("Frequency of mapped points") +
  theme_minimal()
ggsave("Model_of_dist2road_bygender_nls_noprimaryeducated_fit.png", p)

#############################################
### GLM model of dist2road by socioeconomics ----
#dist2road_m ~ activity*gender*age + education + income
#set up data
ppgis_sub <- ppgis_df %>% drop_na(gender, age, education, income_NOK) %>% 
  mutate_at(vars(LogID, activity, gender, education, income), as.factor) %>%
  group_by(LogID, activity, gender, age, education, income) %>%
  summarise(median_dist = median(dist2road_m),
            mean_dist = mean(dist2road_m),
            logmean_dist = log(mean(dist2road_m)),
            quant_25 = quantile(dist2road_m)[2],
            quant_75 = quantile(dist2road_m)[4], 
            num_points = n())

#summarise the data
ppgis_sub_summary <- ppgis_df %>% drop_na(gender, age, education, income_NOK) %>%
                      group_by(LogID, education, gender, income_NOK) %>%
                      summarise(age=mean(age), 
                             mean_dist=mean(dist2road_m)) %>%
                      ungroup() %>%
                      group_by(education, gender) %>%
                      summarise(n_people=n_distinct(LogID),
                                mean_age=round(mean(age), 1),
                                median_age=median(age),
                                n_points=n(),
                                mean_dist=round(mean(mean_dist), 1))
write.csv(ppgis_sub_summary, "Participant_characteristics_summary.csv", row.names=FALSE)
                                

#full model and backwards stepwise model selection
sink("Model_of_dist2road_bysocioecon_glm.txt")
mod_full <- glm(logmean_dist ~ activity*gender + education + age + income, data = ppgis_sub, family = gaussian)
print(summary(mod_full))
mod_min <- step(mod_full)
print(summary(mod_min))
sink()

#Do my own model selection, keeping interaction

#all people
mod_full <- glm(logmean_dist ~ activity*gender + education + age + income, data = ppgis_sub, family = gaussian)
g2 <- glm(logmean_dist ~ activity*gender + education + age, data = ppgis_sub, family = gaussian)
g3 <- glm(logmean_dist ~ activity*gender + education + income, data = ppgis_sub, family = gaussian)
g4 <- glm(logmean_dist ~ activity*gender + education, data = ppgis_sub, family = gaussian)
g5 <- glm(logmean_dist ~ activity*gender, data = ppgis_sub, family = gaussian)
g6 <- glm(logmean_dist ~ activity + gender, data = ppgis_sub, family = gaussian)
g7 <- glm(logmean_dist ~ activity + gender + education, data = ppgis_sub, family = gaussian)
g8 <- glm(logmean_dist ~ activity + gender + education + age, data = ppgis_sub, family = gaussian)
g9 <- glm(logmean_dist ~ activity + gender + education + income, data = ppgis_sub, family = gaussian)
g10 <- glm(logmean_dist ~ activity + education + income, data = ppgis_sub, family = gaussian)
g11 <- glm(logmean_dist ~ activity + education , data = ppgis_sub, family = gaussian)
g12 <- glm(logmean_dist ~ activity , data = ppgis_sub, family = gaussian)
g13 <- glm(logmean_dist ~ activity*education + gender, data = ppgis_sub, family = gaussian)


a <- do.call(rbind, lapply(list(mod_full, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13), function(x) {
  df <- data.frame(modelform=do.call(paste0, as.list(as.character(formula(x)[c(2,1,3)]))), AIC=round(AIC(x), 1), AICc=round(AICc(x), 1), BIC=round(BIC(x), 1), stringsAsFactors=FALSE)
  return(df)
}))
write.csv(a, "Model_of_dist2road_bysocioecon_glm_manualmodelselection.csv", row.names=FALSE)

sink("Model_of_dist2road_bysocioecon_glm.txt", append=TRUE)
print(summary(g11))
sink()

x <-as.data.frame.table(summary(g11)$coefficients) %>% spread(key = Var2, value = Freq)
write.csv(x, "Model_of_dist2road_bysocioecon_glm_coefs.csv", row.names=FALSE)

#plot the model checks
png("Model_of_dist2road_bysocioecon_glm_check_parsmod.png", width=9, height=7, units="in", res=150)
par(mfrow=c(2,2))
plot(g11)
dev.off()


#plot the predictions from the two models
ppgis_sub_preds <- ppgis_sub %>% 
  add_predictions(mod_full) %>% 
  add_residuals(mod_full) %>%
  mutate(pred_modfull=pred,
         resid_modfull=resid) %>%
  add_predictions(g11) %>% 
  add_residuals(g11) %>%
  mutate(pred_parsmod=pred,
         resid_parsmod=resid)

ggplot(ppgis_sub_preds, aes(y=exp(pred_modfull)/1000, x=activity))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Model_of_dist2road_bysocioecon_glm_fullmodel.png", width=7.77, height=4.34, units="in")
ggplot(ppgis_sub_preds, aes(y=exp(pred_parsmod)/1000, x=activity))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Model_of_dist2road_bysocioecon_glm_parsmodel.png", width=7.77, height=4.34, units="in")


### Drop primary educated people from the model ----
#ppgis_sub2 <- ppgis_sub %>% filter(gender!="Male" | education!="primary")
ppgis_sub2 <- ppgis_sub %>% filter(education!="primary")

#full model and backwards stepwise model selection
sink("Model_of_dist2road_bysocioecon_glm_dropprimaryeducated.txt")
mod_full <- glm(logmean_dist ~ activity*gender + education + age + income, data = ppgis_sub2, family = gaussian)
print(summary(mod_full))
mod_min <- step(mod_full)
print(summary(mod_min))
sink()

#manual model selection, without primary educated people
mod_full <- glm(logmean_dist ~ activity*gender + education + age + income, data = ppgis_sub2, family = gaussian)
g2 <- glm(logmean_dist ~ activity*gender + age + income, data = ppgis_sub2, family = gaussian)
g3 <- glm(logmean_dist ~ activity*gender + income, data = ppgis_sub2, family = gaussian)
g4 <- glm(logmean_dist ~ activity*gender + age, data = ppgis_sub2, family = gaussian)
g5 <- glm(logmean_dist ~ activity*gender, data = ppgis_sub2, family = gaussian)
g6 <- glm(logmean_dist ~ activity + gender, data = ppgis_sub2, family = gaussian)
g7 <- glm(logmean_dist ~ activity + gender + age, data = ppgis_sub2, family = gaussian)
g8 <- glm(logmean_dist ~ activity + gender + income, data = ppgis_sub2, family = gaussian)
g9 <- glm(logmean_dist ~ activity + gender + age + income, data = ppgis_sub2, family = gaussian)
g10 <- glm(logmean_dist ~ activity + age + income, data = ppgis_sub2, family = gaussian)
g11 <- glm(logmean_dist ~ activity + age , data = ppgis_sub2, family = gaussian)
g12 <- glm(logmean_dist ~ activity, data = ppgis_sub2, family = gaussian)
g13 <- glm(logmean_dist ~ activity*gender + education*age + income, data = ppgis_sub2, family = gaussian)


a <- do.call(rbind, lapply(list(mod_full, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12), function(x) {
  df <- data.frame(modelform=do.call(paste0, as.list(as.character(formula(x)[c(2,1,3)]))), AIC=round(AIC(x), 1), AICc=round(AICc(x), 1), BIC=round(BIC(x), 1), stringsAsFactors=FALSE)
  return(df)
}))
write.csv(a, "Model_of_dist2road_bysocioecon_glm_dropprimaryeducated_manualmodelselection.csv", row.names=FALSE)

sink("Model_of_dist2road_bysocioecon_glm_dropprimaryeducated.txt", append=TRUE)
print(summary(g12))
sink()

#x <-as.data.frame.table(summary(mod_min)$coefficients) %>% spread(key = Var2, value = Freq)
#write.csv(x, "Model_of_dist2road_bysocioecon_glm_coefs_dropprimaryeducatedmen.csv", row.names=FALSE)

#plot the model checks
png("Model_of_dist2road_bysocioecon_glm_check_parsmod_dropprimaryeducated.png", width=9, height=7, units="in", res=150)
par(mfrow=c(2,2))
plot(g12)
dev.off()


#plot the predictions from the two models
ppgis_sub_preds <- ppgis_sub2 %>% 
  add_predictions(mod_full) %>% 
  add_residuals(mod_full) %>%
  mutate(pred_modfull=pred,
         resid_modfull=resid) %>%
  add_predictions(g12) %>% 
  add_residuals(g12) %>%
  mutate(pred_parsmod=pred,
         resid_parsmod=resid)

ggplot(ppgis_sub_preds, aes(y=exp(pred_modfull)/1000, x=activity))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Model_of_dist2road_bysocioecon_glm_dropprimaryeducated_fullmodel.png", width=7.77, height=4.34, units="in")
ggplot(ppgis_sub_preds, aes(y=exp(pred_parsmod)/1000, x=activity))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Model_of_dist2road_bysocioecon_glm_dropprimaryeducated_parsmodel.png", width=7.77, height=4.34, units="in")


############################
exp( ({AICc(g6)-AICc(g5)}/2) )
#model g6 is 0.0364 times as probable as mod g5 to minimise the information loss

# #Plot values against fitted by activity
# #plot(mod3, frequ ~ fitted(.) | activity, abline = c(0,1), col="black")
# p <- ggplot(ppgis_sub_preds, aes(pred_modmin, logmean_dist)) +
#   geom_point(col="grey50", pch=1) + geom_abline(slope=1,intercept=0) +
#   xlab("Fitted values") + ylab("Measured values") +
#   facet_wrap("activity", nrow=3, scales="free") 
# ggsave("Model_of_dist2road_bysocioecon_glm_actual_vs_fitted.png", p)
# 
# #Plot residuals against distance to road by activity
# p <- ggplot(ppgis_sub_preds, aes(logmean_dist, resid_modmin)) +
#   geom_point(col="grey50", pch=1) + #geom_abline(slope=1,intercept=0) +
#   xlab("Log mean distance to road") + ylab("Model residuals") +
#   facet_wrap("activity", nrow=3, scales="free") 
# ggsave("Model_of_dist2road_bysocioecon_glm_logmeandist2road_vs_residuals.png", p)
 
ppgis_sub <- ppgis_df %>% drop_na(gender, age, education, income_NOK) %>%
          mutate_at(vars(LogID, activity, gender, education, income), as.factor) %>%
          mutate(rounddist2road = round(dist2road_m+0.5, 0),
                 logdist2road = log(rounddist2road)) 

#becasuse we have grouped distance (to 1m) 
#Trialed a model with logID as random intercept, model would not converge
g1 <- lmer(logdist2road ~ activity*gender + age + income + education + (1|LogID), data = ppgis_sub, family=gaussian(link="identity"))
g2 <- glmer(dist2road_m ~ activity + gender + age + income + education + (1|LogID), data = ppgis_sub, family=gaussian(link="log"), glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
g3 <- glmer(dist2road_m ~ activity + gender + income + education + (1|LogID), data = ppgis_sub, family=Gamma(link="identity"), glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
g4 <- glmer(dist2road_m ~ activity + gender + income + education + (1|LogID), data = ppgis_sub, family=poisson(link="log"))
g5 <- glmer(rounddist2road ~ activity + gender + income + education + (1|LogID), data = ppgis_sub, family=poisson(link="log"))



#################################
##Explicitly model development preference

#ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_environment_socioeconomic")
ppgis_df <- read_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_environment_socioeconomic.csv")

### Drop preferences
ppgis_devpref <- ppgis_df %>%  
  filter(category %in% c("+development", "-development", "+tourism", "-tourism")) %>% droplevels() %>%
  drop_na(gender, age, education, income_NOK)

#463 points, 163 people, 130 with all demographic info
#146 with gender info


