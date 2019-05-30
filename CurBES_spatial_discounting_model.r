#Plot and model the distance to road for different ES values

#SETUP ----
require(sf)
require(tidyverse)
require(ggplot2)
require(modelr) #add_predictions
#require(mgcv) #gams
require(lme4) #lmer
#require(nlme) #gnls
require(DHARMa)
require(piecewiseSEM)
require(sjPlot) #lme plots and diagnostics

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

#First we run a model at 100m to show that distance from road explains most of the variance in the data
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
mod2 <- gnls(frequ ~ exp(a+b*dist2road_round), data=ppgis_freq, start=list(a=0, b=0))
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

#Model at 10m 
#Option 1: split landscape into plots (based on distance from road) and count frequency of points in each plot
#We ignore other environmental variables as we know that accessibility is by far the biggest driver - environmental variables only account for a small proportion of the variation
#Set up data
breaktbl <- data.frame(dist2road_round = rep(seq(10, round(max(ppgis_df$dist2road_m), digits=-1), 10), each=length(unique(ppgis_df$activity))),
                       activity=rep(unique(ppgis_df$activity)), stringsAsFactors=FALSE)

ppgis_freq <- ppgis_df %>% 
              mutate(dist2road_round = round(dist2road_m+5.5, digits=-1)) %>% #round to nearest 10m (9m=>10m, 14m=>20m)
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

#Model 2 nls at 10m
#first run a model without activity to determine the starting parameters a and b
mod2a <- nls(frequ ~ exp(a+b*dist2road_round), data=ppgis_freq, start=list(a=0, b=0))
sink("Model_of_dist2road_byactivity_nls.txt", append=TRUE)
print("NLS model of dist2road ignoring activity")
summary(mod2a)
print(paste0("Deviance (RSS): ", deviance(mod2a)))
sink()

#Model 3 generalised non-linear least-squares (ignoring variance structure)
#https://datascienceplus.com/second-step-with-non-linear-regression-adding-predictors/
#basically this fits separate nls models on each activity
#This model gives exactly the same results as 
#g1 <- glm(frequ ~ activity*dist2road_round - 1, data = ppgis_freq, family = gaussian(link = "log"), start = rep(0, 18))
#but is easier to interpret the coefficients

lf <- formula(frequ ~ exp(a+b*dist2road_round)|activity)
mod3 <- nlsList(lf, data=ppgis_freq, start=coef(mod2a))
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
                      mutate(resid_std = resid(mod3, type="pearson")) %>%
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
#plot(mod3, resid(.) ~ fitted(.) | activity, abline=0)
p <- ggplot(ppgis_freq_preds, aes(pred3, resid_std)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=0,intercept=0) +
  xlab("Fitted values") + ylab("Standardised pearson residuals") +
  facet_wrap("activity", nrow=3, scales="free_x") +
  theme_minimal(16)
ggsave("Model_of_dist2road_byactivity_nls_stdresiduals_vs_fitted_byactivity.png", p, width=8, height=6, units="in")
p <- ggplot(ppgis_freq_preds, aes(pred3, resid_std)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=0,intercept=0) +
  xlab("Fitted values") + ylab("Standardised pearson residuals") +
  #facet_wrap("activity", nrow=3, scales="free_x") +
  theme_minimal(16)
ggsave("Model_of_dist2road_byactivity_nls_stdresiduals_vs_fitted.png", p, width=7, height=6, units="in")

#Plot frequency against fitted by activity
#plot(mod3, frequ ~ fitted(.) | activity, abline = c(0,1), col="black")
p <- ggplot(ppgis_freq_preds, aes(pred3, frequ)) +
  geom_point(col="grey50", pch=1) + geom_abline(slope=1,intercept=0) +
  xlab("Fitted values") + ylab("Frequency") +
  facet_wrap("activity", nrow=3, scales="free") +
  theme_minimal(16)
ggsave("Model_of_dist2road_byactivity_nls_frequ_vs_fitted.png", p, width=8, height=6, units="in")

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

### TRIAL GNLS model accounting for heteroskedascicity ----
#categorical variables cant be included so we run a separate model for each activity
lf <- formula(frequ ~ exp(a+b*dist2road_round))
models <- ppgis_freq %>% group_by(activity) %>%
  do(mod3 = gnls(lf, data=., start=coef(mod2a), control=gnlsControl(maxIter=1000, nlsMaxIter = 100, nlsTol = 0.1)))
do(mod3b = gnls(lf, data=., start=coef(mod2a), control=gnlsControl(maxIter=1000, nlsMaxIter = 100, nlsTol = 0.1), weights=varFixed(~dist2road_round)))
model_coef <- models %>% do(data.frame(activity=.$activity,
                                       var_mod3 = names(coef(.$mod3)),
                                       coef_mod3 = coef(summary(.$mod3)),
                                       var_mod3b = names(coef(.$mod3b)),
                                       coef_mod3b = coef(summary(.$mod3b))))
model_summary <- models %>% do(data.frame(activity=.$activity,
                                          AIC_mod3 = summary(.$mod3)$AIC,
                                          AIC_mod3b = summary(.$mod3b)$AIC,
                                          BIC_mod3 = summary(.$mod3)$BIC,
                                          BIC_mod3b = summary(.$mod3b)$BIC,
                                          logLik_mod3 = summary(.$mod3)$logLik, 
                                          logLik_mod3b = summary(.$mod3b)$logLik))

model_preds <- models %>% do(data.frame(activity=.$activity,
                                        pred_3=fitted(.$mod3),
                                        residuals_3=residuals(.$mod3),  
                                        pred_3b=fitted(.$mod3b),
                                        residuals_3b=residuals(.$mod3b) )) %>% 
  bind_cols(arrange(ppgis_freq, activity) ) %>%
  add_predictions(mod2a) %>% 
  mutate(pred_2a = pred)

model_rsq <- model_preds %>% group_by(activity) %>%
  summarise(rsq_mod2a=rsq(frequ, pred_2a),
            rsq_mod3=rsq(frequ, pred_3),
            rsq_mod3b=rsq(frequ, pred_3b))

compare <- models %>% do(aov = anova(.$mod_3, .$mod_3b)) %>%
  summarise(activity=.$activity,
            p.value = aov$`Pr(>F)`)




###############################
### MODEL DIST2ROAD by SOCIOECONOMIC CLASS ----
#Are different types of people more likely to map certain values further from roads?

#We draw on Sigrid Engens paper on what types of people support or oppose development
#(preferences we can choose from are consumptive use, motorised use, development, predator control).
#then we look at the differences in the distance from road that they map different values
#She found that men were significantly higher than women to be in favor of activities
#and that age (favor decreased by age) and education (higher=less in favor) had marginally significant effects
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

#############################################
### Linear mixed effects model of dist2road by socioeconomics ----
#full model = dist2road_m ~ activity*gender + age + education + income, with LogID as a random effect

ppgis_sub <- ppgis_df %>% drop_na(gender, age, education, income_NOK) %>%
  mutate_at(vars(LogID, activity, gender, education, income), as.factor) %>%
  mutate(rounddist2road = round(dist2road_m+0.5, 0), #so dist starts at 1 not zero thus able to be logged
         logdist2road = log(rounddist2road)) 

#LME with logID as random intercept
#Ben Bolker one of the guys behind the lme4 package says that you should put the variables that you control for first in the model
#and then the variables that are the main focus (activty*gender in this case) should be last
g1 <- lmer(logdist2road~ education + age + income + activity*gender + (1|LogID), data = ppgis_sub)
g1_1 <- lmer(dist2road_m~ education + age + income + activity*gender + (1|LogID), data = ppgis_sub) #just tried without logtransformation

#plot residuals assesses how well the predicted and the observed values fit across predictors. The actual (observed) values have a coloured ???ll, while the predicted values have a solid outline without ???lling.
#https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf
plot_residuals(g1)

# plot random effects 
plot_model(g1, type = "re")

# plot marginal effects 
plot_model(g1, type = "pred", terms = "education")
plot_model(g1, type = "pred", terms = "age")
plot_model(g1, type = "pred", terms = "gender")
plot_model(g1, type = "pred", terms = "income")
plot_model(g1, type = "pred", terms = "activity")

#plot diagnostic plots for lmer For linear (mixed) models, plots for multicollinearity-check (Variance In???ation Factors), 
#QQ-plots, checks for normal distribution of residuals and homoscedasticity (constant variance of residuals) are shown https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf.
p<-plot_model(g1, type = "diag")

p[[1]]
p[[2]]
p[[3]]
p[[4]]

# recommended function to use for rsquared calculations of mixed models in library piecewiseSEM, gives both the variance explained by the fixed effect alone and 
#the fixed and random component together - the whole model https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5970551/ 
rsquared(g1)

#model selection
g1 <- lmer(logdist2road ~ education + age + income + activity*gender + (1|LogID), data = ppgis_sub)
g2 <- lmer(logdist2road ~ education + age + activity*gender + (1|LogID), data = ppgis_sub)
g3 <- lmer(logdist2road ~ education + income + activity*gender + (1|LogID), data = ppgis_sub)
g4 <- lmer(logdist2road ~ education + activity*gender + (1|LogID), data = ppgis_sub)
g5 <- lmer(logdist2road ~ activity*gender + (1|LogID), data = ppgis_sub)
g6 <- lmer(logdist2road ~ activity + gender + (1|LogID), data = ppgis_sub)
g7 <- lmer(logdist2road ~ activity + gender + education + (1|LogID), data = ppgis_sub)
g8 <- lmer(logdist2road ~ activity + gender + education + age + (1|LogID), data = ppgis_sub)
g9 <- lmer(logdist2road ~ activity + gender + education + age + income + (1|LogID), data = ppgis_sub)
g10 <- lmer(logdist2road ~ activity + education + income + (1|LogID), data = ppgis_sub)
g11 <- lmer(logdist2road ~ activity + education + age + (1|LogID), data = ppgis_sub)
g12 <- lmer(logdist2road ~ activity + age + (1|LogID), data = ppgis_sub)
g13 <- lmer(logdist2road ~ activity + (1|LogID), data = ppgis_sub)
g14 <- lmer(logdist2road ~ activity + education*gender + (1|LogID), data = ppgis_sub)

a <- do.call(rbind, lapply(list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14), function(x) {
  df <- data.frame(modelform=do.call(paste0, as.list(as.character(formula(x)))), AIC=round(AIC(x), 1), AICc=round(AICc(x), 1), BIC=round(BIC(x), 1), r_squared=round(rsquared(x), 4), stringsAsFactors=FALSE)
  return(df)
}))
# rsquared() is recommended function to use for rsquared calculations of mixed models in library piecewiseSEM, gives both the variance explained by the fixed effect alone and 
#the fixed and random component together - the whole model https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5970551/ 
exp( ({AICc(g6)-AICc(g5)}/2) )
#e.g. model g6 is 0.0364 times as probable as mod g5 to minimise the information loss

write.csv(a, "Model_of_dist2road_bysocioecon_lme_manualmodelselection.csv", row.names=FALSE)

sink("Model_of_dist2road_bysocioecon_lme.txt", append=TRUE)
print(summary(g13))
sink()

x <-as.data.frame.table(summary(g11)$coefficients) %>% spread(key = Var2, value = Freq)
write.csv(x, "Model_of_dist2road_bysocioecon_lme_coefs.csv", row.names=FALSE)

#plot the model diagnostics
png("Model_of_dist2road_bysocioecon_lme_check_parsmod.png", width=9, height=7, units="in", res=150)
par(mfrow=c(2,2))
#plot diagnostic plots for lmer For linear (mixed) models, plots for multicollinearity-check (Variance In???ation Factors), 
#QQ-plots, checks for normal distribution of residuals and homoscedasticity (constant variance of residuals) are shown https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf.
plot_model(g13, type = "diag")
dev.off()

#plot residuals assesses how well the predicted and the observed values fit across predictors. The actual (observed) values have a coloured ???ll, while the predicted values have a solid outline without ???lling.
#https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf
plot_residuals(g1)

# plot random effects 
plot_model(g1, type = "re")

# plot marginal effects 
plot_model(g1, type = "pred", terms = "education")
plot_model(g1, type = "pred", terms = "age")
plot_model(g1, type = "pred", terms = "gender")
plot_model(g1, type = "pred", terms = "income")
plot_model(g1, type = "pred", terms = "activity")


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
ggsave("Model_of_dist2road_bysocioecon_lme_fullmodel.png", width=7.77, height=4.34, units="in")
ggplot(ppgis_sub_preds, aes(y=exp(pred_parsmod)/1000, x=activity))+
  geom_boxplot(aes(col=gender)) + 
  ylab("Mean distance to road (km)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Model_of_dist2road_bysocioecon_lme_parsmodel.png", width=7.77, height=4.34, units="in")


### Drop primary educated people from the model ----
#ppgis_sub2 <- ppgis_sub %>% filter(gender!="Male" | education!="primary")
ppgis_sub2 <- ppgis_sub %>% filter(education!="primary")

#full model and backwards stepwise model selection
sink("Model_of_dist2road_bysocioecon_lme_dropprimaryeducated.txt")
###
#
#
sink()

#manual model selection, without primary educated people
###
#
#
#
#
write.csv(a, "Model_of_dist2road_bysocioecon_lme_dropprimaryeducated_manualmodelselection.csv", row.names=FALSE)

sink("Model_of_dist2road_bysocioecon_lme_dropprimaryeducated.txt", append=TRUE)
print(summary(g12))
sink()



#plot the model checks
png("Model_of_dist2road_bysocioecon_lme_check_parsmod_dropprimaryeducated.png", width=9, height=7, units="in", res=150)
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
 


#################################
## Explicitly model development preference ----

#ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_environment_socioeconomic")
ppgis_df <- read_csv("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis/Curbes_ppgis_plus_environment_socioeconomic.csv")

### Drop preferences
ppgis_devpref <- ppgis_df %>%  
  filter(category %in% c("+development", "-development", "+tourism", "-tourism")) %>% droplevels() %>%
  drop_na(gender, age, education, income_NOK)

#463 points, 163 people, 130 with all demographic info
#146 with gender info

#dist2stuff ~ proportion of points mapped against development
#expect that people with increasing proportion would map values further from roads

#model how far to where people live include as a covariate/ or as the y value
#model distance to town. can use their postcode and how far it is to their town.
#model distance to industry
#model odds ratio of preferences vs distance to stuff.
lmer()



