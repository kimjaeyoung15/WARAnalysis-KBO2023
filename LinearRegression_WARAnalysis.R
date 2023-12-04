#packages required
install.packages('car')
install.packages('ggplot2')
install.packages('GGally')
install.packages('lmtest')
install.packages('moments')

library(car)
library(ggplot2)
library(GGally)
library(lmtest)
library(moments)

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

LM_Prototype_WARAnalysis_v0.1.6 <- lm(war~isop, data=Dataset_WARAnalysis_v0.1)
summary(LM_Prototype_WARAnalysis_v0.1.6)
plot(war~isop, data=Dataset_WARAnalysis_v0.1)
abline(LM_Prototype_WARAnalysis_v0.1.6)

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
LM_Cleaned_WARAnalysis_v0.3.1 <- lm(war~singles_plate+doubles_plate+triples_plate+homeruns_plate
                                    +stolen_plate+caught_plate+bb_plate+hp_plate+strikeouts_plate, data=Dataset_Cleaned_WARAnalysis_v0.3)
LM_Cleaned_WARAnalysis_v0.3.1l <- lm(ln_war~singles_plate+doubles_plate+triples_plate+homeruns_plate
                                    +stolen_plate+caught_plate+bb_plate+hp_plate+strikeouts_plate, data=Dataset_Cleaned_WARAnalysis_v0.3)
summary(LM_Cleaned_WARAnalysis_v0.3.1)
summary(LM_Cleaned_WARAnalysis_v0.3.1l)
step(LM_Cleaned_WARAnalysis_v0.3.1l, direction = 'both')
step(LM_Cleaned_WARAnalysis_v0.3.1, direction = 'both')
par(mfrow=c(2,3))
plot(LM_Cleaned_WARAnalysis_v0.3.1, which=c(1:6))

saveRDS(LM_Cleaned_WARAnalysis_v0.3.1,file='LM_Cleaned_WARAnalysis_v0.3.1.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.3.1l,file='LM_Cleaned_WARAnalysis_v0.3.1l.rds')

#model 0.3.2
LM_Cleaned_WARAnalysis_v0.3.2 <- lm(war~singles_plate+homeruns_plate
                                    +stolen_plate+bb_plate, data=Dataset_Cleaned_WARAnalysis_v0.3)
LM_Cleaned_WARAnalysis_v0.3.2l <- lm(ln_war~singles_plate+homeruns_plate
                                     +stolen_plate+bb_plate, data=Dataset_Cleaned_WARAnalysis_v0.3) #significant, moderate variance of residuals
summary(LM_Cleaned_WARAnalysis_v0.3.2)
summary(LM_Cleaned_WARAnalysis_v0.3.2l)
plot(LM_Cleaned_WARAnalysis_v0.3.2, which=c(1:6))
plot(LM_Cleaned_WARAnalysis_v0.3.2l, which=c(1:6))
shapiro.test(LM_Cleaned_WARAnalysis_v0.3.2l$residuals)

saveRDS(LM_Cleaned_WARAnalysis_v0.3.2,file='LM_Cleaned_WARAnalysis_v0.3.2.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.3.2l,file='LM_Cleaned_WARAnalysis_v0.3.2l.rds')

#model 0.3.3 - stolen base cannot be calculated per plate
par(mfrow=c(2,3))
LM_Cleaned_WARAnalysis_v0.3.3 <- lm(formula = war ~ singles_plate + doubles_plate + triples_plate + 
                                      homeruns_plate + bb_plate+stolen_plate, 
                                    data = Dataset_Cleaned_WARAnalysis_v0.3)
LM_Cleaned_WARAnalysis_v0.3.3l <- lm(formula = ln_war ~ singles_plate + doubles_plate + triples_plate + 
                                       homeruns_plate + bb_plate+stolen_plate, 
                                     data = Dataset_Cleaned_WARAnalysis_v0.3)
summary(LM_Cleaned_WARAnalysis_v0.3.3)
summary(LM_Cleaned_WARAnalysis_v0.3.3l)

vif(LM_Cleaned_WARAnalysis_v0.3.3)
vif(LM_Cleaned_WARAnalysis_v0.3.3l)

plot(LM_Cleaned_WARAnalysis_v0.3.3, which = c(1:6))
plot(LM_Cleaned_WARAnalysis_v0.3.3l,which=c(1:6))
shapiro.test(LM_Cleaned_WARAnalysis_v0.3.3l$residuals)
jarque.test(LM_Cleaned_WARAnalysis_v0.3.3l$residuals)

saveRDS(LM_Cleaned_WARAnalysis_v0.3.3,file='LM_Cleaned_WARAnalysis_v0.3.3.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.3.3l,file='LM_Cleaned_WARAnalysis_v0.3.3l.rds')

mockset <- Dataset_Cleaned_WARAnalysis_v0.3[-c(4,12,21),]
mockmodel <- lm(formula = ln_war ~ singles_plate + doubles_plate + triples_plate + 
                  homeruns_plate + bb_plate, 
                data = mockset)
plot(mockmodel, which=c(1:6))
shapiro.test(mockmodel$residuals)

#model 0.3.4
LM_Cleaned_WARAnalysis_v0.3.4 <- lm(formula = war ~ singles_plate + doubles_plate + triples_plate + 
                                      homeruns_plate + bb_plate, 
                                    data = Dataset_Cleaned_WARAnalysis_v0.3)
LM_Cleaned_WARAnalysis_v0.3.4l <- lm(formula = ln_war ~ singles_plate + doubles_plate + triples_plate + 
                                       homeruns_plate + bb_plate,
                                     data = Dataset_Cleaned_WARAnalysis_v0.3)
summary(LM_Cleaned_WARAnalysis_v0.3.4)
summary(LM_Cleaned_WARAnalysis_v0.3.4l)

vif(LM_Cleaned_WARAnalysis_v0.3.4)
vif(LM_Cleaned_WARAnalysis_v0.3.4l)

plot(LM_Cleaned_WARAnalysis_v0.3.4, which = c(1:6))
plot(LM_Cleaned_WARAnalysis_v0.3.4l,which=c(1:6))

saveRDS(LM_Cleaned_WARAnalysis_v0.3.4,file='LM_Cleaned_WARAnalysis_v0.3.4.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.3.4l,file='LM_Cleaned_WARAnalysis_v0.3.4l.rds')

#model 0.5
setwd('C:\\Users\\esteb\\Documents\\GitHub\\WARAnalysis-KBO2023\\WARAnalysis_Cleaned_v0.5')
Dataset_Cleaned_WARAnalysis_v0.5 <- read.csv('Dataset_Cleaned_WARAnalysis_v0.5.csv')
saveRDS(Dataset_Cleaned_WARAnalysis_v0.5,file='Dataset_Cleaned_WARAnalysis_v0.5.rds')

#model 0.5.1
LM_Cleaned_WARAnalysis_v0.5.1la <- lm(ln_war~ln_batdir_left+ln_batdir_center+ln_batdir_right
                                     +ln_batdir_pull+ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.1lb <- lm(ln_war~ln_batdir_left
                                     +ln_batdir_pull+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.1lc <- lm(ln_war~ln_batdir_center
                                     +ln_batdir_pull+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.1ld <- lm(ln_war~ln_batdir_right
                                     +ln_batdir_pull+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.1le <- lm(ln_war~ln_batdir_left
                                     +ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.1lf <- lm(ln_war~ln_batdir_center
                                     +ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.1lg <- lm(ln_war~ln_batdir_right
                                     +ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                     +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
summary(LM_Cleaned_WARAnalysis_v0.5.1la)
summary(LM_Cleaned_WARAnalysis_v0.5.1lb)
summary(LM_Cleaned_WARAnalysis_v0.5.1lc)
summary(LM_Cleaned_WARAnalysis_v0.5.1ld)
summary(LM_Cleaned_WARAnalysis_v0.5.1le)
summary(LM_Cleaned_WARAnalysis_v0.5.1lf)
summary(LM_Cleaned_WARAnalysis_v0.5.1lg)

vif(LM_Cleaned_WARAnalysis_v0.5.1la)
vif(LM_Cleaned_WARAnalysis_v0.5.1lb)
vif(LM_Cleaned_WARAnalysis_v0.5.1lc)
vif(LM_Cleaned_WARAnalysis_v0.5.1ld)
vif(LM_Cleaned_WARAnalysis_v0.5.1le)
vif(LM_Cleaned_WARAnalysis_v0.5.1lf)
vif(LM_Cleaned_WARAnalysis_v0.5.1lg)

saveRDS(LM_Cleaned_WARAnalysis_v0.5.1la,file='LM_Cleaned_WARAnalysis_v0.5.1la.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.1lb,file='LM_Cleaned_WARAnalysis_v0.5.1lb.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.1lc,file='LM_Cleaned_WARAnalysis_v0.5.1lc.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.1ld,file='LM_Cleaned_WARAnalysis_v0.5.1ld.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.1le,file='LM_Cleaned_WARAnalysis_v0.5.1le.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.1lf,file='LM_Cleaned_WARAnalysis_v0.5.1lf.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.1lg,file='LM_Cleaned_WARAnalysis_v0.5.1lg.rds')


#model 0.5.2
LM_Cleaned_WARAnalysis_v0.5.2la <- lm(ln_batting_average~ln_batdir_left
                                      +ln_batdir_pull+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.2lb <- lm(ln_batting_average~ln_batdir_center
                                      +ln_batdir_pull+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.2lc <- lm(ln_batting_average~ln_batdir_right
                                      +ln_batdir_pull+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.2ld <- lm(ln_batting_average~ln_batdir_left
                                      +ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.2le <- lm(ln_batting_average~ln_batdir_center
                                      +ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.2lf <- lm(ln_batting_average~ln_batdir_right
                                      +ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
summary(LM_Cleaned_WARAnalysis_v0.5.2la)
summary(LM_Cleaned_WARAnalysis_v0.5.2lb)
summary(LM_Cleaned_WARAnalysis_v0.5.2lc)
summary(LM_Cleaned_WARAnalysis_v0.5.2ld)
summary(LM_Cleaned_WARAnalysis_v0.5.2le)
summary(LM_Cleaned_WARAnalysis_v0.5.2lf)

vif(LM_Cleaned_WARAnalysis_v0.5.2la)
vif(LM_Cleaned_WARAnalysis_v0.5.2lb)
vif(LM_Cleaned_WARAnalysis_v0.5.2lc)
vif(LM_Cleaned_WARAnalysis_v0.5.2ld)
vif(LM_Cleaned_WARAnalysis_v0.5.2le)
vif(LM_Cleaned_WARAnalysis_v0.5.2lf)

step(LM_Cleaned_WARAnalysis_v0.5.2la, direction='both')
step(LM_Cleaned_WARAnalysis_v0.5.2lb, direction='both')
step(LM_Cleaned_WARAnalysis_v0.5.2lc, direction='both')
step(LM_Cleaned_WARAnalysis_v0.5.2ld, direction='both')
step(LM_Cleaned_WARAnalysis_v0.5.2le, direction='both')
step(LM_Cleaned_WARAnalysis_v0.5.2lf, direction='both')

saveRDS(LM_Cleaned_WARAnalysis_v0.5.2la,file='LM_Cleaned_WARAnalysis_v0.5.2la.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.2lb,file='LM_Cleaned_WARAnalysis_v0.5.2lb.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.2lc,file='LM_Cleaned_WARAnalysis_v0.5.2lc.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.2ld,file='LM_Cleaned_WARAnalysis_v0.5.2ld.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.2le,file='LM_Cleaned_WARAnalysis_v0.5.2le.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.2lf,file='LM_Cleaned_WARAnalysis_v0.5.2lf.rds')


#model 0.5.3
LM_Cleaned_WARAnalysis_v0.5.3la <- lm(formula = ln_batting_average ~ ln_batdir_left + ln_bat_at_first, 
                                      data = Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.3lb <- lm(formula = ln_batting_average ~ ln_batdir_center, data = Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.3lc <- lm(formula = ln_batting_average ~ ln_batdir_pull + ln_bat_at_first, 
                                      data = Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.3ld <- lm(formula = ln_batting_average ~ ln_batdir_left + ln_batdir_push + 
                                        ln_bat_at_first, data = Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.3le <- lm(formula = ln_batting_average ~ ln_batdir_center + ln_batdir_push + 
                                        ln_bat_at_first, data = Dataset_Cleaned_WARAnalysis_v0.5)
LM_Cleaned_WARAnalysis_v0.5.3lf <- lm(formula = ln_batting_average ~ ln_batdir_push + ln_bat_at_first, 
                                      data = Dataset_Cleaned_WARAnalysis_v0.5)

summary(LM_Cleaned_WARAnalysis_v0.5.3la)
summary(LM_Cleaned_WARAnalysis_v0.5.3lb)
summary(LM_Cleaned_WARAnalysis_v0.5.3lc)
summary(LM_Cleaned_WARAnalysis_v0.5.3ld)
summary(LM_Cleaned_WARAnalysis_v0.5.3le)
summary(LM_Cleaned_WARAnalysis_v0.5.3lf)

saveRDS(LM_Cleaned_WARAnalysis_v0.5.3la,file='LM_Cleaned_WARAnalysis_v0.5.3la.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.3lb,file='LM_Cleaned_WARAnalysis_v0.5.3lb.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.3lc,file='LM_Cleaned_WARAnalysis_v0.5.3lc.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.3ld,file='LM_Cleaned_WARAnalysis_v0.5.3ld.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.3le,file='LM_Cleaned_WARAnalysis_v0.5.3le.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.3lf,file='LM_Cleaned_WARAnalysis_v0.5.3lf.rds')


#model 0.5.4
LM_Cleaned_WARAnalysis_v0.5.4la <- lm(ln_war~ln_batdir_pull+ln_batdir_push+ln_bat_in_pitch+ln_bat_at_first
                                      +ln_swing_in_chance_ratio,data=Dataset_Cleaned_WARAnalysis_v0.5)
summary(LM_Cleaned_WARAnalysis_v0.5.4la)
vif(LM_Cleaned_WARAnalysis_v0.5.4la)
step(LM_Cleaned_WARAnalysis_v0.5.4la, direction = 'both')
LM_Cleaned_WARAnalysis_v0.5.4lb <- lm(formula = ln_batting_average ~ ln_batdir_pull + ln_batdir_push + ln_bat_at_first, 
   data = Dataset_Cleaned_WARAnalysis_v0.5)
summary(LM_Cleaned_WARAnalysis_v0.5.4lb)

saveRDS(LM_Cleaned_WARAnalysis_v0.5.4la,file='LM_Cleaned_WARAnalysis_v0.5.4la.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.5.4lb,file='LM_Cleaned_WARAnalysis_v0.5.4lb.rds')


#model 0.6
setwd('C:\\Users\\esteb\\Documents\\GitHub\\WARAnalysis-KBO2023\\WARAnalysis_Cleaned_v0.6')
Dataset_Cleaned_WARAnalysis_v0.6 <- read.csv('Dataset_Cleaned_WARAnalysis_v0.6.csv')
saveRDS(Dataset_Cleaned_WARAnalysis_v0.6,file='Dataset_Cleaned_WARAnalysis_v0.6.rds')

LM_Cleaned_WARAnalysis_v0.6.1 <- lm(batting_average~swing_in_chance_ratio+bat_in_pitch+bat_at_first, data=Dataset_Cleaned_WARAnalysis_v0.6)
summary(LM_Cleaned_WARAnalysis_v0.6.1)
vif(LM_Cleaned_WARAnalysis_v0.6.1)
names(Dataset_Cleaned_WARAnalysis_v0.6)
saveRDS(LM_Cleaned_WARAnalysis_v0.6.1,file='LM_Cleaned_WARAnalysis_v0.6.1.rds')


#model 0.7
setwd('C:\\Users\\esteb\\Documents\\GitHub\\WARAnalysis-KBO2023\\WARAnalysis_Cleaned_v0.7')
Dataset_Cleaned_WARAnalysis_v0.7 <- read.csv('Dataset_Cleaned_WARAnalysis_v0.7.csv')
saveRDS(Dataset_Cleaned_WARAnalysis_v0.7, 'Dataset_Cleaned_WARAnalysis_v0.7.rds')

LM_Cleaned_WARAnalysis_v0.7.1a <- lm(war~w_fastball_value+w_slider_value+w_curve_value+w_changeup_value+w_spliter_value+w_sinker_value, data=Dataset_Cleaned_WARAnalysis_v0.7)

summary(LM_Cleaned_WARAnalysis_v0.7.1a)
step(LM_Cleaned_WARAnalysis_v0.7.1a, direction = 'both')
LM_Cleaned_WARAnalysis_v0.7.1b <- lm(formula = war ~ w_fastball_value + w_slider_value + w_curve_value + 
                                       w_changeup_value, data = Dataset_Cleaned_WARAnalysis_v0.7)
summary(LM_Cleaned_WARAnalysis_v0.7.1b)
vif(LM_Cleaned_WARAnalysis_v0.7.1a)
LM_Cleaned_WARAnalysis_v0.7.1lb <- lm(formula = ln_war ~ w_fastball_value + w_slider_value + w_curve_value + 
                                       w_changeup_value, data = Dataset_Cleaned_WARAnalysis_v0.7)
summary(LM_Cleaned_WARAnalysis_v0.7.1lb)
par(mfrow=c(2,3))
plot(LM_Cleaned_WARAnalysis_v0.7.1b, which=c(1:6))
plot(LM_Cleaned_WARAnalysis_v0.7.1lb, which=c(1:6))
shapiro.test(LM_Cleaned_WARAnalysis_v0.7.1lb$residuals)
vif()

LM_Cleaned_WARAnalysis_v0.7.1z <- lm(formula = war ~ z_fastball_value + z_slider_value + z_curve_value + 
                                       z_changeup_value, data = Dataset_Cleaned_WARAnalysis_v0.7)
summary(LM_Cleaned_WARAnalysis_v0.7.1z)
LM_Cleaned_WARAnalysis_v0.7.1lz <- lm(formula = ln_war ~ z_fastball_value + z_slider_value + z_curve_value + 
                                        z_changeup_value, data = Dataset_Cleaned_WARAnalysis_v0.7)
summary(LM_Cleaned_WARAnalysis_v0.7.1lz)

saveRDS(LM_Cleaned_WARAnalysis_v0.7.1a,file='LM_Cleaned_WARAnalysis_v0.7.1a.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.7.1b,file='LM_Cleaned_WARAnalysis_v0.7.1b.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.7.1lb,file='LM_Cleaned_WARAnalysis_v0.7.1lb.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.7.1z,file='LM_Cleaned_WARAnalysis_v0.7.1z.rds')
saveRDS(LM_Cleaned_WARAnalysis_v0.7.1lz,file='LM_Cleaned_WARAnalysis_v0.7.1lz.rds')

