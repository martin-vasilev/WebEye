
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
  
  # if(!'Distance_To_Screen_In_CM'%in% colnames(info)){
  #   info$Distance_To_Screen_In_CM<- NA
  # }
  # 
  # if(!'Pixel_Density_PerMM'%in% colnames(info)){
  #   info$Pixel_Density_PerMM<- NA
  # }
  
  # Sort columns alphabetically
  info <- info[, order(names(info))]
  
  info$list= list
  
  dat<- rbind(dat, info)
  
}

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





