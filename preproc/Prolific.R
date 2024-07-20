
rm(list= ls())

options(scipen=999)

library(readr)

folders<- list.dirs('preproc/prolific/')
folders<- folders[2:length(folders)]

for(i in 1:length(folders)){
  
  # open files:
  folder_dir<- folders[i]
  trials <- read_csv(paste(folder_dir, '/trials.csv', sep=''))
  trials<- subset(trials, Task_Name== 'sentence'|Task_Name== 'sentence_DC' )
  
  # info:
  info<- read_csv(paste(folder_dir, '/sessions.csv', sep=''))
  
  ## screen settings<- 
  scr_width<- info$Screen_Width_In_Pixels
  scr_height<- info$Screen_Height_In_Pixels
  
  
  
  # timeseries:
  ts<- read_csv(paste(folder_dir, '/timeseries.csv', sep=''))
  
}



