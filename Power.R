
rm(list= ls())

library(qs2)
library(here)
library(simr)
library(brms)
library(lmerTest)

library(readr)
dat <- read_csv("Prolific/data/target_word_frequency_dat.csv")

dat$Freq<- as.factor(dat$Freq)
contrasts(dat$Freq)<- c(0.5, -0.5)

FFD<- lmer(log(FFD)~ Freq +(Freq|sub) +(Freq|item), data= dat)
#summary(FFD)
SFD<- lmer(log(SFD)~ Freq +(Freq|sub) +(Freq|item), data= dat)
GD<- lmer(log(GD)~ Freq +(Freq|sub) +(Freq|item), data= dat)
TVT<- lmer(log(TVT)~ Freq +(Freq|sub) +(Freq|item), data= dat)


# model parameters:

# FFD:
b_FFD <- coef(summary(FFD))[,1] # fixed intercept and slopes
RE_FFD <- VarCorr(FFD) # random effects
s_FFD <- sigma(FFD) # residual sd

# SFD:
b_SFD <- coef(summary(SFD))[,1] # fixed intercept and slopes
RE_SFD <- VarCorr(SFD) # random effects
s_SFD <- sigma(SFD) # residual sd

# GD:
b_GD <- coef(summary(TVT))[,1] # fixed intercept and slopes
RE_GD <- VarCorr(TVT) # random effects
s_GD <- sigma(TVT) # residual sd

# TVT:
b_TVT <- coef(summary(FFD))[,1] # fixed intercept and slopes
RE_TVT <- VarCorr(FFD) # random effects
s_TVT <- sigma(FFD) # residual sd


power<- NULL

##### Simulations:

NSim= 200 # number of simulations per cell 
nsub = seq(6, 120, 6)  # number of subjects
nitems = seq(8, 160, 8) 
data_loss<- 0.1 # percentage of data points to randomly remove


for(i in 1:length(nsub)){ # for each subject..
  
  for(j in 1:length(nitems)){ # for each item..
    
    # high- low freq order (first half of subjects):
    sim_data <- data.frame(Freq= rep(c("high", "low"), length.out = (nsub[i]/2) * nitems[j]))
    sim_data$Subject <- rep(seq(1, nsub[i]/2, 1), each= nitems[j])
    sim_data$item <- rep(1:nitems[j], times = nsub[i]/2)
    sim_data$FFD<- rnorm(n= nrow(sim_data))
    sim_data$SFD<- rnorm(n= nrow(sim_data))
    sim_data$GD<- rnorm(n= nrow(sim_data))
    sim_data$TVT<- rnorm(n= nrow(sim_data))
    
    # low- high freq order (second half of subjects):
    
    sim_data2 <- data.frame(Freq= rep(c("low", "high"), length.out = (nsub[i]/2) * nitems[j]))
    sim_data2$Subject <- rep(seq(1, nsub[i]/2, 1), each= nitems[j])
    # increment so that subject IDs are not the same:
    sim_data2$Subject<- sim_data2$Subject + max(sim_data$Subject)
    sim_data2$item <- rep(1:nitems[j], times = nsub[i]/2)
    sim_data2$FFD<- rnorm(n= nrow(sim_data))
    sim_data2$SFD<- rnorm(n= nrow(sim_data))
    sim_data2$GD<- rnorm(n= nrow(sim_data))
    sim_data2$TVT<- rnorm(n= nrow(sim_data))
    
    sim_data3<- rbind(sim_data, sim_data2)
    
    # check counter-balancing:
    table(sim_data3$item, sim_data3$Freq)
    
    # simulate data loss:
    sim_data3<- sim_data3[-sample(nrow(sim_data3),
                                  round(data_loss*nrow(sim_data3))), ]
    
    
    sim_data3$Freq<-as.factor(sim_data3$Freq)
    contrasts(sim_data3$Freq)<- c(0.5, -0.5)
    
    # make fake models using real variance/covariance matrix:
    
    # FFD:
    model_FFD <- makeLmer(FFD~ Freq+ 
                            (Freq|Subject)+(Freq|item),
                          fixef=b_FFD, VarCorr= RE_FFD, sigma=s_FFD, data=sim_data3)
    #summary(model_FFD)
    
    # SFD:
    model_SFD <- makeLmer(SFD~ Freq+ 
                            (Freq|Subject)+(Freq|item),
                          fixef=b_SFD, VarCorr= RE_SFD, sigma=s_SFD, data=sim_data3)
    #summary(model_SFD)
    
    # GD:
    model_GD <- makeLmer(GD~ Freq+ 
                           (Freq|Subject)+(Freq|item),
                         fixef=b_GD, VarCorr= RE_GD, sigma=s_GD, data=sim_data3)
    #summary(model_GD)
    
    # TVT:
    model_TVT <- makeLmer(TVT~ Freq+ 
                            (Freq|Subject)+(Freq|item),
                          fixef=b_TVT, VarCorr= RE_TVT, sigma=s_TVT, data=sim_data3)
    #summary(model_TVT)
    
    
    ### Power:
    
    # FFD:
    p_FFD<- powerSim(model_FFD, nsim= NSim,
                     test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
    s_FFD<- summary(p_FFD)
    
    # SFD:
    p_SFD<- powerSim(model_SFD, nsim= NSim,
                     test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
    s_SFD<- summary(p_SFD)
    
    # GD:
    p_GD<- powerSim(model_GD, nsim= NSim,
                    test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
    s_GD<- summary(p_GD)
    
    # TVT:
    p_TVT<- powerSim(model_TVT, nsim= NSim,
                     test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
    s_TVT<- summary(p_TVT)
    
    # save result:
    t<- data.frame('Num_subjects'= nsub[i],
                   'Num_items'= nitems[j],
                   power_FFD= s_FFD$mean,
                   power_FFD_upper= s_FFD$upper,
                   power_FFD_lower= s_FFD$lower,
                   power_SFD= s_SFD$mean,
                   power_SFD_upper= s_SFD$upper,
                   power_SFD_lower= s_SFD$lower,
                   power_GD= s_GD$mean,
                   power_GD_upper= s_GD$upper,
                   power_GD_lower= s_GD$lower,
                   power_TVT= s_TVT$mean,
                   power_TVT_upper= s_TVT$upper,
                   power_TVT_lower= s_TVT$lower

                   
                   )
    
    power<- rbind(power, t)
    
  }
  
}


# # high- low freq order (first half of subjects):
# sim_data <- data.frame(Freq= rep(c("high", "low"), length.out = (nsub/2) * nitems))
# sim_data$Subject <- rep(seq(1, nsub/2, 1), each= nitems)
# sim_data$item <- rep(1:nitems, times = nsub/2)
# sim_data$FFD<- rnorm(n= nrow(sim_data))
# sim_data$SFD<- rnorm(n= nrow(sim_data))
# sim_data$GD<- rnorm(n= nrow(sim_data))
# sim_data$TVT<- rnorm(n= nrow(sim_data))
# 
# # low- high freq order (second half of subjects):
# 
# sim_data2 <- data.frame(Freq= rep(c("low", "high"), length.out = (nsub/2) * nitems))
# sim_data2$Subject <- rep(seq(1, nsub/2, 1), each= nitems)
# sim_data2$item <- rep(1:nitems, times = nsub/2)
# sim_data2$FFD<- rnorm(n= nrow(sim_data))
# sim_data2$SFD<- rnorm(n= nrow(sim_data))
# sim_data2$GD<- rnorm(n= nrow(sim_data))
# sim_data2$TVT<- rnorm(n= nrow(sim_data))
# 
# sim_data3<- rbind(sim_data, sim_data2)
# 
# # check counter-balancing:
# table(sim_data3$item, sim_data3$Freq)
# 
# # simulate data loss:
# sim_data3<- sim_data3[-sample(nrow(sim_data3),
#                     round(data_loss*nrow(sim_data3))), ]
# 
# 
# # model parameters:
# 
# # FFD:
# b_FFD <- coef(summary(FFD))[,1] # fixed intercept and slopes
# RE_FFD <- VarCorr(FFD) # random effects
# s_FFD <- sigma(FFD) # residual sd
# 
# # SFD:
# b_SFD <- coef(summary(SFD))[,1] # fixed intercept and slopes
# RE_SFD <- VarCorr(SFD) # random effects
# s_SFD <- sigma(SFD) # residual sd
# 
# # GD:
# b_GD <- coef(summary(TVT))[,1] # fixed intercept and slopes
# RE_GD <- VarCorr(TVT) # random effects
# s_GD <- sigma(TVT) # residual sd
# 
# # TVT:
# b_TVT <- coef(summary(FFD))[,1] # fixed intercept and slopes
# RE_TVT <- VarCorr(FFD) # random effects
# s_TVT <- sigma(FFD) # residual sd
# 
# sim_data3$Freq<-as.factor(sim_data3$Freq)
# contrasts(sim_data3$Freq)<- c(0.5, -0.5)
# 
# # make fake models using real variance/covariance matrix:
# 
# # FFD:
# model_FFD <- makeLmer(FFD~ Freq+ 
#                      (Freq|Subject)+(Freq|item),
#                    fixef=b_FFD, VarCorr= RE_FFD, sigma=s_FFD, data=sim_data3)
# #summary(model_FFD)
# 
# # SFD:
# model_SFD <- makeLmer(SFD~ Freq+ 
#                         (Freq|Subject)+(Freq|item),
#                       fixef=b_SFD, VarCorr= RE_SFD, sigma=s_SFD, data=sim_data3)
# #summary(model_SFD)
# 
# # GD:
# model_GD <- makeLmer(GD~ Freq+ 
#                         (Freq|Subject)+(Freq|item),
#                       fixef=b_GD, VarCorr= RE_GD, sigma=s_GD, data=sim_data3)
# #summary(model_GD)
# 
# # TVT:
# model_TVT <- makeLmer(TVT~ Freq+ 
#                        (Freq|Subject)+(Freq|item),
#                      fixef=b_TVT, VarCorr= RE_TVT, sigma=s_TVT, data=sim_data3)
# #summary(model_TVT)
# 
# 
# ### Power:
# 
# # FFD:
# p_FFD<- powerSim(model_FFD, nsim= NSim,
#                  test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
# 
# # SFD:
# p_SFD<- powerSim(model_SFD, nsim= NSim,
#                  test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
# 
# # GD:
# p_GD<- powerSim(model_GD, nsim= NSim,
#                  test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
# 
# # TVT:
# p_TVT<- powerSim(model_TVT, nsim= NSim,
#                 test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05)
# 
# 
