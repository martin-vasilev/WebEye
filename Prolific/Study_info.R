
rm(list= ls())

options(scipen=999)

library(readr)
library(tidyverse)

# load data folders:
folders<- list.dirs('Prolific/prolific_raw_data')
folders<- folders[2:length(folders)]

dat<- NULL

for(i in 1:length(folders)){ # for each subject folder
  
  # open files:
  folder_dir<- folders[i]
  
  # get study list
  list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  # info:
  info<- read_csv(paste(folder_dir, '/sessions.csv', sep=''))
  trials<- read_csv(paste(folder_dir, '/trials.csv', sep=''))
  
  #info$Crowdsourcing_SubjId<- NULL
  info$Crowdsourcing_Code<- NULL
  #info$End_Time_Local<- NULL
  info$Experiment_Version<- NULL
  info$Group_Name<- NULL
  info$Group_Nr<- NULL
  info$Session_Name<- NULL
  info$Session_Nr<- NULL
  info$Subject_Code<- NULL
  info$Subject_Nr<- NULL
  info$Session_Token<- NULL
  info$Subject_Nr_Per_Group<- NULL
  #info$Start_Time_Local<- NULL
  info$Completed<- NULL
  
  info$Distance_To_Screen_In_CM<- NULL
  info$Pixel_Density_PerMM<- NULL
  
  if(!'Distance_To_Screen_In_CM'%in% colnames(info)){
    info$Distance_To_Screen_In_CM<- NA
  }
  # 
  if(!'Pixel_Density_PerMM'%in% colnames(info)){
    info$Pixel_Density_PerMM<- NA
  }
  
  dist<- which(!is.na(trials$screen_dist))
  
  if(length(dist)>0){
    info$screen_dist<- trials$screen_dist[dist]
  }else{
    info$screen_dist<- NA
  }
  
  pixel_density<- which(!is.na(trials$pixel_density))
  if(length(dist)>0){
    info$pixel_density<- trials$pixel_density[pixel_density]
  }else{
    info$pixel_density<- NA
  }
  
  
  # Sort columns alphabetically
  info <- info[, order(names(info))]
  
  info$list= list
  
  # info$screen_distance<- trials$screen_dist[which(!is.na(trials$screen_dist))]
  # 
  # if(!'screen_distance'%in% colnames(info)){
  #   info$screen_distance<- NULL
  # }
  
  dat<- rbind(dat, info)
  
}

### add visual angle:
PixelsToAngle <- function(
    pixel_distance,
    pixel_density,      # pixels per mm
    viewing_distance_cm = 60
){
  
  viewing_distance_mm <- viewing_distance_cm * 10
  
  angle_deg <- (
    pixel_distance /
      (pixel_density * viewing_distance_mm)
  ) * 57.3
  
  return(angle_deg)
}

# pixels per letter (in Labvanced screen):
ppl<- 33

# We can get the true letter width by taking the ration of differences between screen size:
dat$letter_width<- ppl*(dat$Window_Width_In_Pixels/800)

dat$letter_deg<- PixelsToAngle(pixel_density = dat$pixel_density,
                               pixel_distance = dat$letter_width, viewing_distance_cm = dat$screen_dist)

write.csv(dat, 'Prolific/data/Study_info_data.csv')



#dat_sorted <- dat[order(dat$Start_Time), ]

### Some stats to report for paper:
table(dat$Browser_Spec)

library(tidyverse)
library(ggpubr)

P_browser <- dat %>% 
  count(Browser_Spec) %>%
  rename(`Browser Type`= Browser_Spec, N= n)%>%
  arrange(desc(N))%>%
  
  ggtexttable(rows = NULL, 
                        theme = ttheme("mBlue"))


P_system <- dat %>% 
  count(System_Spec) %>%
  rename(`System`= System_Spec, N= n)%>%
  arrange(desc(N))%>%
  
  ggtexttable(rows = NULL, 
              theme = ttheme("mOrange"))


dat<- dat %>% mutate(resolution= paste(Screen_Width_In_Pixels,
                                       'x', Screen_Height_In_Pixels))

P_resolution<- dat %>% 
  count(resolution) %>%
  rename(`Screen resolution`= resolution, N= n)%>%
  arrange(desc(N))%>%
  
  ggtexttable(rows = NULL, 
              theme = ttheme("mGreen"))



# combine the two plots:
library(ggpubr)


figure1 <- ggarrange(P_browser, P_system,
                         ncol = 1, nrow = 2) #widths = c(1.8, 1))

figure2 <- ggarrange(P_resolution, figure1,
                     ncol = 2, nrow = 1)

ggsave(filename = 'Prolific/Plots/computer_setup.png',
       plot = figure2, height = 8, width = 4,units = 'in')
