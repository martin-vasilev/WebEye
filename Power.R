
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

NSim= 100 # number of simulations per cell 
nsub = seq(20, 110, 10)  # number of subjects
nitems = seq(20, 110, 10) 
data_loss<- 0.1 # percentage of data points to randomly remove


for(i in 1:length(nsub)){ # for each subject..
  
  
  for(j in 1:length(nitems)){ # for each item..
    
    cat(sprintf("subject %g, item %g\n", nsub[i], nitems[j]))
    
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
    suppressMessages(
    model_FFD <- makeLmer(FFD~ Freq+ 
                            (Freq|Subject)+(Freq|item),
                          fixef=b_FFD, VarCorr= RE_FFD, sigma=s_FFD, data=sim_data3)
    )
    #summary(model_FFD)
    
    # SFD:
    suppressMessages(
    model_SFD <- makeLmer(SFD~ Freq+ 
                            (Freq|Subject)+(Freq|item),
                          fixef=b_SFD, VarCorr= RE_SFD, sigma=s_SFD, data=sim_data3)
    )
    #summary(model_SFD)
    
    # GD:
    suppressMessages(
    model_GD <- makeLmer(GD~ Freq+ 
                           (Freq|Subject)+(Freq|item),
                         fixef=b_GD, VarCorr= RE_GD, sigma=s_GD, data=sim_data3)
    )
    #summary(model_GD)
    
    # TVT:
    suppressMessages(
    model_TVT <- makeLmer(TVT~ Freq+ 
                            (Freq|Subject)+(Freq|item),
                          fixef=b_TVT, VarCorr= RE_TVT, sigma=s_TVT, data=sim_data3)
    )
    #summary(model_TVT)
    
    
    ### Power:
    
    # FFD:
    suppressMessages(
    p_FFD<- powerSim(model_FFD, nsim= NSim,
                     test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05, progress= F)
    )
    sum_FFD<- summary(p_FFD)
    
    # SFD:
    suppressMessages(
    p_SFD<- powerSim(model_SFD, nsim= NSim,
                     test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05, progress= F)
    )
    sum_SFD<- summary(p_SFD)
    
    # GD:
    suppressMessages(
    p_GD<- powerSim(model_GD, nsim= NSim,
                    test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05, progress= F)
    )
    sum_GD<- summary(p_GD)
    
    # TVT:
    suppressMessages(
    p_TVT<- powerSim(model_TVT, nsim= NSim,
                     test = simr::fixed(xname = 'Freq1', method = "z"), alpha =.05, progress= F)
    )
    sum_TVT<- summary(p_TVT)
    
    # save result:
    t<- data.frame('Num_subjects'= nsub[i],
                   'Num_items'= nitems[j],
                   power_FFD= sum_FFD$mean,
                   power_FFD_upper= sum_FFD$upper,
                   power_FFD_lower= sum_FFD$lower,
                   power_SFD= sum_SFD$mean,
                   power_SFD_upper= sum_SFD$upper,
                   power_SFD_lower= sum_SFD$lower,
                   power_GD= sum_GD$mean,
                   power_GD_upper= sum_GD$upper,
                   power_GD_lower= sum_GD$lower,
                   power_TVT= sum_TVT$mean,
                   power_TVT_upper= sum_TVT$upper,
                   power_TVT_lower= sum_TVT$lower

                   
                   )
    
    power<- rbind(power, t)
    
  }
  
}

save(power, file= "power_results.Rda")


power_long<- power %>% pivot_longer(cols = c('power_FFD', 'power_SFD',
                                   'power_GD', 'power_TVT'), 
                          names_to = 'measure', values_to = 'power')

power_long$measure[which(power_long$measure=='power_FFD')]<- "FFD"
power_long$measure[which(power_long$measure=='power_SFD')]<- "SFD"
power_long$measure[which(power_long$measure=='power_GD')]<- "GD"
power_long$measure[which(power_long$measure=='power_TVT')]<- "TVT"


library(ggplot2)

GGP_FFD<- ggplot(power_long, aes(x = Num_subjects, y = power)) +
  geom_area(fill = "steelblue", alpha = 0.5, colour = "black") +
  geom_hline(yintercept = .8, color= 'darkred')+
  facet_wrap(
    ~ measure*Num_items, ncol= 5,
    labeller = labeller(
      Num_items = function(x) paste("Num. of items =", x)
    )
  ) +
  labs(
    x = "Number of subjects",
    y = "Power"
  ) +
  theme_minimal(24)+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = 'Prolific/Plots/Power_FFD.png', plot = GGP_FFD, 
       width =  16, height = 20, units = 'in')

