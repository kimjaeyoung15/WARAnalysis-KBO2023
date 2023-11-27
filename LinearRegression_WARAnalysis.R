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
setwd('C:\\Users\\esteb\\Documents\\GitHub\\WARAnalysis-KBO2023\\WARAnalysis_Cleaned_v0.2')
Dataset_WARAnalysis_v0.1 <- read.csv('Dataset_WARAnalysis_v0.1.csv')

#prototype models

#model 0.1 series

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

#cleaned dataset 0.2
setwd('C:\\Users\\esteb\\Documents\\GitHub\\WARAnalysis-KBO2023\\WARAnalysis_Cleaned_v0.2')
#model 0.2.1 series(contain all variables)
Dataset_Cleaned_WARAnalysis_v0.2 <- read.csv('Dataset_Cleaned_WARAnalysis_v0.2.csv')
LM_Cleaned_WARAnalysis_v0.2.1 <- lm(war~hits_game+doubles_game+triples_game+homeruns_game+rbi_game
                                    +stolen_game+caught_game+bb_game+hp_game, 
                                    data=Dataset_Cleaned_WARAnalysis_v0.2) #basehits_game is removed because of high multicolinearity
LM_Cleaned_WARAnalysis_v0.2.1l <- lm(ln_war~ln_hits_game+ln_doubles_game+ln_triples_game+ln_homeruns_game
                                     +ln_basehits_game+ln_rbi_game+ln_stolen_game+ln_caught_game+ln_bb_game+ln_hp_game, 
                                     data=Dataset_Cleaned_WARAnalysis_v0.2)
summary(LM_Cleaned_WARAnalysis_v0.2.1)
summary(LM_Cleaned_WARAnalysis_v0.2.1l)
vif(LM_Cleaned_WARAnalysis_v0.2.1)
vif(LM_Cleaned_WARAnalysis_v0.2.1l)
step(LM_Cleaned_WARAnalysis_v0.2.1, direction = 'both')
summary(lm(formula = war ~ hits_game + doubles_game + homeruns_game + 
             stolen_game + bb_game, data = Dataset_Cleaned_WARAnalysis_v0.2))
step(LM_Cleaned_WARAnalysis_v0.2.1l, direction = 'both')
summary(lm(formula = ln_war ~ ln_hits_game + ln_basehits_game + ln_bb_game, 
           data = Dataset_Cleaned_WARAnalysis_v0.2))

saveRDS(LM_Cleaned_WARAnalysis_v0.2.1,file='LM_Cleaned_WARAnalysis_v0.2.1.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.2.1l,file='LM_Cleaned_WARAnalysis_v0.2.1l.rds')

#model 0.2.2 series(selected variables)

#model 0.2.2a - variables are selected by step method
LM_Cleaned_WARAnalysis_v0.2.2a <- lm(formula = war ~ hits_game + homeruns_game + 
                                       stolen_game + bb_game, data = Dataset_Cleaned_WARAnalysis_v0.2)
LM_Cleaned_WARAnalysis_v0.2.2la <- lm(formula = ln_war ~ ln_hits_game + ln_basehits_game + ln_bb_game, 
                                     data = Dataset_Cleaned_WARAnalysis_v0.2)
summary(LM_Cleaned_WARAnalysis_v0.2.2a)
summary(LM_Cleaned_WARAnalysis_v0.2.2la)
par(mfrow=c(2,3))
plot(LM_Cleaned_WARAnalysis_v0.2.2a, which=c(1:6))
plot(LM_Cleaned_WARAnalysis_v0.2.2la, which=c(1:6))
shapiro.test(LM_Cleaned_WARAnalysis_v0.2.2a$residuals)

saveRDS(LM_Cleaned_WARAnalysis_v0.2.2a,file='LM_Cleaned_WARAnalysis_v0.2.2a.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.2.2la,file='LM_Cleaned_WARAnalysis_v0.2.2la.rds')

#model 0.2.2b
par(mfrow=c(2,3))
LM_Cleaned_WARAnalysis_v0.2.3 <- lm(war~., data=Dataset_Cleaned_WARAnalysis_v0.2) #don't use; worthless
step(LM_Cleaned_WARAnalysis_v0.2.3, direction = 'both')
LM_Cleaned_WARAnalysis_v0.2.3a <- lm(formula = war ~ ln_hits_game + ln_triples_game + ln_homeruns_game + ln_basehits_game
                                     + ln_bb_game+ln_stolen_game+ln_caught_game+ln_hp_game, data = Dataset_Cleaned_WARAnalysis_v0.2)
summary(LM_Cleaned_WARAnalysis_v0.2.3a)
summary(lm(war~hits_game+doubles_game+triples_game+homeruns_game, 
           data=Dataset_Cleaned_WARAnalysis_v0.2))
LM_Cleaned_WARAnalysis_v0.2.3la <- lm(ln_war~hits_game+doubles_game+triples_game+homeruns_game+rbi_game
                                      +stolen_game+caught_game+bb_game+hp_game, 
                                      data=Dataset_Cleaned_WARAnalysis_v0.2)
summary(LM_Cleaned_WARAnalysis_v0.2.3la)
step(LM_Cleaned_WARAnalysis_v0.2.3la, direction = 'both')
LM_Cleaned_WARAnalysis_v0.2.3lb <- lm(formula = ln_war ~ hits_game + homeruns_game + 
                                        stolen_game + bb_game, data = Dataset_Cleaned_WARAnalysis_v0.2)
summary(LM_Cleaned_WARAnalysis_v0.2.3lb)
plot(LM_Cleaned_WARAnalysis_v0.2.3lb, which=c(1:6))
shapiro.test(LM_Cleaned_WARAnalysis_v0.2.3lb$residuals)

saveRDS(LM_Cleaned_WARAnalysis_v0.2.3a,file='LM_Cleaned_WARAnalysis_v0.2.3a.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.2.3la,file='LM_Cleaned_WARAnalysis_v0.2.3la.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.2.3lb,file='LM_Cleaned_WARAnalysis_v0.2.3lb.rds')

#cleaned dataset 0.3
setwd('C:\\Users\\esteb\\Documents\\GitHub\\WARAnalysis-KBO2023\\WARAnalysis_Cleaned_v0.3')
Dataset_Cleaned_WARAnalysis_v0.3 <- read.csv('Dataset_Cleaned_WARAnalysis_v0.3.csv')

#model 0.3.1
LM_Cleaned_WARAnalysis_v0.3.1 <- lm(war~hits_plate+doubles_plate+triples_plate+homeruns_plate
                                    +stolen_plate+caught_plate+bb_plate+hp_plate+strikeouts_plate, data=Dataset_Cleaned_WARAnalysis_v0.3)
LM_Cleaned_WARAnalysis_v0.3.1l <- lm(ln_war~hits_plate+doubles_plate+triples_plate+homeruns_plate
                                    +stolen_plate+caught_plate+bb_plate+hp_plate+strikeouts_plate, data=Dataset_Cleaned_WARAnalysis_v0.3)
summary(LM_Cleaned_WARAnalysis_v0.3.1)
step(LM_Cleaned_WARAnalysis_v0.3.1, direction = 'both')
par(mfrow=c(2,3))
plot(LM_Cleaned_WARAnalysis_v0.3.1, which=c(1:6))

saveRDS(LM_Cleaned_WARAnalysis_v0.3.1,file='LM_Cleaned_WARAnalysis_v0.3.1.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.3.1l,file='LM_Cleaned_WARAnalysis_v0.3.1l.rds')

#model 0.3.2
LM_Cleaned_WARAnalysis_v0.3.2 <- lm(war~hits_plate+homeruns_plate
                                    +stolen_plate+bb_plate, data=Dataset_Cleaned_WARAnalysis_v0.3)
LM_Cleaned_WARAnalysis_v0.3.2l <- lm(ln_war~hits_plate+homeruns_plate
                                     +stolen_plate+bb_plate, data=Dataset_Cleaned_WARAnalysis_v0.3) #significant, moderate variance of residuals
summary(LM_Cleaned_WARAnalysis_v0.3.2)
summary(LM_Cleaned_WARAnalysis_v0.3.2l)
plot(LM_Cleaned_WARAnalysis_v0.3.2, which=c(1:6))
plot(LM_Cleaned_WARAnalysis_v0.3.2l, which=c(1:6))
shapiro.test(LM_Cleaned_WARAnalysis_v0.3.2l$residuals)

mockset <- Dataset_Cleaned_WARAnalysis_v0.3[-12,]
mockmodel <- LM_Cleaned_WARAnalysis_v0.3.2l <- lm(ln_war~hits_plate+homeruns_plate
                                                  +stolen_plate+bb_plate, data=mockset)
plot(mockmodel, which = c(1:6))

saveRDS(LM_Cleaned_WARAnalysis_v0.3.2,file='LM_Cleaned_WARAnalysis_v0.3.2.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.3.2l,file='LM_Cleaned_WARAnalysis_v0.3.2l.rds')
