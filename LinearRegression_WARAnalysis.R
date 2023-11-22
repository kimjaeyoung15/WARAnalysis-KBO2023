#packages required
install.packages('car')
install.packages('ggplot2')
install.packages('GGally')
install.packages('lmtest')

library(car)
library(ggplot2)
library(GGally)
library(lmtest)

#working directory
setwd('C:\\Users\\Finance_Analytics\\WARAnalysis')
Dataset_WARAnalysis_v0.1 <- read.csv('Dataset_WARAnalysis_v0.1.csv')

#prototype models

LM_Prototype_WARAnalysis_v0.1.1 <- lm(war~batting_average+on_base_percentage+slugging_percentage, data=Dataset_WARAnalysis_v0.1)
summary(LM_Prototype_WARAnalysis_v0.1.1)
vif(LM_Prototype_WARAnalysis_v0.1.1)
saveRDS(LM_Prototype_WARAnalysis_v0.1.1, file='C:\\Users\\Finance_Analytics\\WARAnalysis\\LM_Prototype_WARAnalysis_v0.1.1.rds')
#insignificant because of batting_average

LM_Prototype_WARAnalysis_v0.1.2 <- lm(war~hr_ratio+bb_ratio+isop+isod+babip+spd+psn+w_oba+w_rc_27o_pf+w_raa_pf, data=Dataset_WARAnalysis_v0.1)
summary(LM_Prototype_WARAnalysis_v0.1.2)
vif(LM_Prototype_WARAnalysis_v0.1.2)
saveRDS(LM_Prototype_WARAnalysis_v0.1.2, file='C:\\Users\\Finance_Analytics\\WARAnalysis\\LM_Prototype_WARAnalysis_v0.1.2.rds')
#insignificant

LM_Prototype_WARAnalysis_v0.1.3 <- lm(war~isop+isod+babip+spd+psn+w_rcplus, data=Dataset_WARAnalysis_v0.1)
summary(LM_Prototype_WARAnalysis_v0.1.3)
vif(LM_Prototype_WARAnalysis_v0.1.3)
ggpairs(Dataset_WARAnalysis_v0.1, c(29,30,36,37,38,39,40))
saveRDS(LM_Prototype_WARAnalysis_v0.1.3, file='C:\\Users\\Finance_Analytics\\WARAnalysis\\LM_Prototype_WARAnalysis_v0.1.3.rds')
#insignificant

LM_Prototype_WARAnalysis_v0.1.4 <- lm(war~w_oba_pf+isop+isod+babip, data=Dataset_WARAnalysis_v0.1)
summary(LM_Prototype_WARAnalysis_v0.1.4)
vif(LM_Prototype_WARAnalysis_v0.1.4)
saveRDS(LM_Prototype_WARAnalysis_v0.1.4, file='C:\\Users\\Finance_Analytics\\WARAnalysis\\LM_Prototype_WARAnalysis_v0.1.4.rds')
#insignificant

LM_Prototype_WARAnalysis_v0.1.5 <- lm(war~on_base_percentage+slugging_percentage, data=Dataset_WARAnalysis_v0.1)
summary(LM_Prototype_WARAnalysis_v0.1.5)
vif(LM_Prototype_WARAnalysis_v0.1.5)
par(mfrow=c(2,3))
plot(LM_Prototype_WARAnalysis_v0.1.5, which=c(1:6))
saveRDS(LM_Prototype_WARAnalysis_v0.1.5, file='C:\\Users\\Finance_Analytics\\WARAnalysis\\LM_Prototype_WARAnalysis_v0.1.5.rds')
#significant


#cleaned dataset 0.1 
Dataset_Cleaned_WARAnalysis_v0.1 <- read.csv('Dataset_Cleaned_WARAnalysis_v0.1.csv')
LM_Cleaned_WARAnalysis_v0.1.1 <- lm(ln_war~ln_on_base_percentage+ln_slugging_percentage, data=Dataset_Cleaned_WARAnalysis_v0.1)
summary(LM_Cleaned_WARAnalysis_v0.1.1)
par(mfrow=c(2,3))
plot(LM_Cleaned_WARAnalysis_v0.1.1, which=c(1:6))
vif(LM_Cleaned_WARAnalysis_v0.1.1)
par(mfrow=c(1,1))
skewness(LM_Cleaned_WARAnalysis_v0.1.1$residuals)
kurtosis(LM_Cleaned_WARAnalysis_v0.1.1$residuals)
saveRDS(LM_Cleaned_WARAnalysis_v0.1.1, file='C:\\Users\\Finance_Analytics\\WARAnalysis\\LM_Cleaned_WARAnalysis_v0.1.1.rds')
#significant, and residual plot's variance is smaller than LM_Prototype_WARAnalysis_v0.1.5

