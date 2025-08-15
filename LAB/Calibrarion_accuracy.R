
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
library(readr)
cal <- read_csv("LAB/data/eyelink_calibration.csv")

el_acc<- cal %>% 
  group_by(flnm) %>%
  summarise(M= mean(offset_deg, na.rm= T),
            Med= median(offset_deg, na.rm= T))

mean(el_acc$M)
sd(el_acc$M)
range(el_acc$M)

# generate a plot of 
P_cal<- el_acc %>% 
  ggplot(aes(y= M))+
  ylim(0,0.75)+
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
  labs(y= "Avgerage calibration accuracy\nin deg. [Eyelink]",
      x= 'Density')+
  theme_classic(20)
  
ggsave(filename = 'LAB/Plots/Eyelink_calibration_accuracy.png', plot = P_cal,
       width = 1600, height = 1600, units = 'px')


####### Webcam reported calibration accuracy:

get_num <- function(string) {
  as.numeric(gsub("[^0-9]", "", string))
}

folders<- list.dirs('LAB/lab_raw_data')
folders<- folders[2:length(folders)]

web_acc<- NULL

for(i in 1:length(folders)){
  
  # open files:
  folder_dir<- folders[i]
  
  sub= unlist(strsplit(folder_dir, '/'))[3]
  sub= get_num(sub)
  
  list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  ts<- suppressMessages(read_csv(paste(folder_dir, '/timeseries.csv', sep='')))
  ts$value<- as.numeric(ts$value)
  ce<- subset(ts, is.element(variable_name, c('error_calibration_x', 'error_calibration_y')) & !is.na(value))
  
  ce<- ce%>% 
    group_by(variable_name) %>% 
    distinct(value)
  
  ce$sub<- sub
  
  web_acc<- rbind(web_acc, ce)
  
  unique(ce$value)
  
}

web_acc$value[which(web_acc$variable_name== 'error_calibration_x')]<- web_acc$value[which(web_acc$variable_name== 'error_calibration_x')]*0.0187
web_acc$value[which(web_acc$variable_name== 'error_calibration_y')]<- web_acc$value[which(web_acc$variable_name== 'error_calibration_y')]*0.0192

web_acc<- web_acc %>%
  group_by(sub, variable_name) %>%
  summarise(mean_value = mean(value), .groups = "drop")%>%
  pivot_wider(id_cols = sub,
                  names_from = variable_name,
                  values_from = mean_value)

# x position:
mean(web_acc$error_calibration_x)
sd(web_acc$error_calibration_x)

# y position:
mean(web_acc$error_calibration_y)
sd(web_acc$error_calibration_y)

write.csv(x = web_acc, file = 'LAB/data/webcam_calibration.csv')


