#Plot and model the distance to road for different ES values

#SETUP ----

require(tidyverse)
require(lme4) #glmer
require(DHARMa)
require(piecewiseSEM)
require(sjPlot)

setwd("C:/Users/sigrid.engen/OneDrive - NINA/MIne Artikler/Spatial discounting ES Claire/Data")

#Load the dataset of ppgis points plus environmental data
#ppgis_sf <- st_read("D:/Box Sync/Arctic/MIKON/CurBES/Data/ppgis", "Curbes_ppgis_plus_environment_socioeconomic")
ppgis_df <- read.csv("C:/Users/sigrid.engen/OneDrive - NINA/MIne Artikler/Spatial discounting ES Claire/Data/Curbes_ppgis_plus_environment_socioeconomic.csv")

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



#Function to calculate AICc
AICc<-function(m1) {
      K=summary(m1)$df[1]
      n=summary(m1)$df[2]
      AICc = AIC(m1) + 2*K*(K+1)/(n-K-1)
      return(AICc)
}

###############################
### MODEL DIST2ROAD by SOCIOECONOMIC CLASS ----
#Are different types of people more likely to map certain values further from roads?

ppgis_sub <- ppgis_df %>% drop_na(gender, age, education, income_NOK) %>%
          filter(education!="primary")%>%
          mutate_at(vars(LogID, activity, gender, education, income), as.factor) %>%
          mutate(rounddist2road = round(dist2road_m+0.5, 0),
                 logdist2road = log(rounddist2road)) 

#lmer works if you just remove the call to family
#Ben Bolker one of the guys behind the lme4 package says that you should put the variables that you control for first in the model
#and then the variables that are the main focus (activty*gender in this case) should be last
g1 <- lmer(logdist2road~ education + age + income + activity*gender + (1|LogID), data = ppgis_sub)
#g1_1 <- lmer(dist2road_m~ education + age + income + activity*gender + (1|LogID), data = ppgis_sub) #just tried without logtransformation

#the package sjPlot has some nice functions (plot_model, plot_residuals) for diagnostics and plotting lmer

#plot residuals assesses how well the predicted and the observed values fit across predictors. The actual (observed) values have a coloured ﬁll, while the predicted values have a solid outline without ﬁlling.
#https://cran.r-project.org/web/packages/sjPlot/sjPlot.pdf
plot_residuals(g1)

# remove some independent variables from output 
plot_residuals(g1, remove.estimates = c("age"))

# plot random effects 
plot_model(g14, type = "re")

# plot marginal effects 
plot_model(g1, type = "pred", terms = "education")
plot_model(g1, type = "pred", terms = "age")
plot_model(g1, type = "pred", terms = "gender")
plot_model(g1, type = "pred", terms = "income")
plot_model(g1, type = "pred", terms = "activity")

#plot diagnostic plots for lmer For linear (mixed) models, plots for multicollinearity-check (Variance Inﬂation Factors), 
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
g1 <- lmer(logdist2road~ education + age+income+activity*gender + (1|LogID), data = ppgis_sub)
g2 <- lmer(logdist2road~ education + age + activity*gender + (1|LogID), data = ppgis_sub)
g3 <- lmer(logdist2road~ education + income + activity*gender + (1|LogID), data = ppgis_sub)
g4 <- lmer(logdist2road~ education + activity*gender + (1|LogID), data = ppgis_sub)
g5 <- lmer(logdist2road~ activity*gender + (1|LogID), data = ppgis_sub)

g6 <- lmer(logdist2road~ activity+gender + (1|LogID), data = ppgis_sub)
g7 <- lmer(logdist2road~ activity+gender + education + (1|LogID), data = ppgis_sub)
g8 <- lmer(logdist2road~ activity+gender + education + age + (1|LogID), data = ppgis_sub)
g9 <- lmer(logdist2road~ activity+gender + education + age + income + (1|LogID), data = ppgis_sub)
g10 <- lmer(logdist2road~ activity + education + income + (1|LogID), data = ppgis_sub)
g13 <- lmer(logdist2road~ activity + education*gender + (1|LogID), data = ppgis_sub)
g12 <- lmer(logdist2road~ activity + (1|LogID), data = ppgis_sub)

#did something wrong with this 
a <- do.call(rbind, lapply(list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12), function(x) {
  df <- data.frame(modelform=do.call(paste0, as.list(as.character(formula(x)))), AIC=round(AIC(x), 1), AICc=round(AICc(x), 1), BIC=round(BIC(x), 1), stringsAsFactors=FALSE)
  return(df)
}))

#The gamma works when removing some variables  https://stackoverflow.com/questions/38015647/glmer-error-maxstephalfit-pirls-step-halvings-failed-to-reduce-deviance-in-p
g13 <- glmer(rounddist2road ~ gender + (1|LogID), data = ppgis_sub, family=Gamma(link="identity"), glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

#The poisson i still can't get to run
g14 <- glmer(rounddist2road ~ 1 + (1|LogID), data = ppgis_sub, family=poisson(link="log"), glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

#the DHAMRa packages allows an easy way to assess the model fit of (generalzed) mixed models https://theoreticalecology.wordpress.com/2016/08/28/dharma-an-r-package-for-residual-diagnostics-of-glmms/
#but i couldnt get it to work for this lmer model
simulationOutput <- simulateResiduals(fittedModel = g1,re.form=NA, n=1000, refit=F)
simulationOutput <- simulateResiduals(fittedModel = g1, re.form=NULL,n=1000, refit=F)

# plot residuals, quantreg = T is better but costs more time
plotSimulatedResiduals(simulationOutput = simulationOutput, quantreg = TRUE) 
