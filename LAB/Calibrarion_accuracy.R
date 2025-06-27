
rm(list= ls())

options(scipen = 999)

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


if('EMreading' %in% rownames(installed.packages())==FALSE){
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
}else{
  library(EMreading)
}



cal= Calibr(data_list = 'C:/Data/WebEye')

write.csv(x = cal, 'LAB/data/eyelink_calibration.csv')


library(tidyverse)

el_acc<- cal %>% 
  group_by(subject) %>%
  summarise(M= mean(offset_deg, na.rm= T),
            Med= median(offset_deg, na.rm= T))


# generate a plot of 
P_cal<- el_acc %>% 
  ggplot(aes(y= M))+
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.3,
    point_colour = NA, fill= pallete1[1], alpha= 1)+
  geom_boxplot(
    width = .25,
    outlier.shape = NA, fill= pallete1[5], alpha= 0.3
  )+
  labs(y= "Avg. calibration accuracy [Eyelink]",
      x= 'Density')+
  theme_classic(16)
  
ggsave(filename = 'LAB/Plots/Eyelink_calibration_accuracy.png', plot = P_cal,
       width = 1600, height = 1600, units = 'px')


