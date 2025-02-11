
rm(list= ls())

options(scipen=999)

library(readr)
library(tidyverse)

folders<- list.dirs('preproc/lab/')
folders<- folders[2:length(folders)]

eye_data<- NULL
trial_data<- NULL

for(i in 1:length(folders)){

  # open files:
  folder_dir<- folders[i]
  
  list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  trials <- read_csv(paste(folder_dir, '/trials.csv', sep=''))
  trials<- subset(trials, Task_Name== 'sentence'|Task_Name== 'sentence_DC' )
  trials$sub<- list
  
  # remove empty columns:
  trials<- trials[colSums(!is.na(trials)) > 0]
  
  trials$Block_Name<- NULL
  trials$Block_Nr<- NULL
  trials$Task_Nr<- NULL
  
  trial_data<- rbind(trial_data, trials) # add to df
  
  
  ## eye-movement data files:
  
  # timeseries:
  ts<- read_csv(paste(folder_dir, '/timeseries.csv', sep=''))
  ts<- subset(ts, Task_Name== 'sentence'|Task_Name== 'sentence_DC')
  
  ts$sub<- list
  
  
  ## load eyelink data:
  dataF<- readLines(paste(folder_dir, '/', list,  '.asc', sep='')) # load asc file;
  
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
  start_loc<- which(grepl('UNIX', dataF))
  start_stamp<- dataF[start_loc[1]]
  start<- as.numeric(unlist(strsplit(start_stamp, ' '))[3])
  el_start_time<- get_num(unlist(strsplit(start_stamp, ' '))[1])
  
  # extract fixations:
  sacc_samples<- dataF[start_loc[1]+1:length(dataF)]
  
  # remove flags from samples data:
  sacc_samples<- sacc_samples[!grepl("SFIX", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("EFIX", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("ESACC", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("SSACC", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("SBLINK", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("EBLINK", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("MSG", sacc_samples)]
  
  
  
  sacc_samples <-  as.data.frame(do.call( rbind, strsplit( sacc_samples, '\t' ) )) # V2 is xpos
  sacc_samples$V1<- as.numeric(sacc_samples$V1)
  sacc_samples$V2<- as.numeric(sacc_samples$V2)
  sacc_samples$V3<- as.numeric(sacc_samples$V3)
  
  sacc_samples$start_time<- sacc_samples$V1- el_start_time
  sacc_samples$unix_time<- sacc_samples$start_time+start
  
  
  
  for(j in 1:nrow(trials)){
    
    start_time<- trials$trial_start[j]
    end_time<- trials$trial_end[j]
    
    trial_ts<- subset(ts, Trial_Id== trials$Trial_Id[j] & variable_name== 'gaze_data') 
    
    trial_ts$x<- NA
    trial_ts$y<- NA
    trial_ts$time<- NA
    trial_ts$conf<- NA
    
    trial_ts$el_x<- NA
    trial_ts$el_y<- NA
    trial_ts$el_pupil<- NA
    trial_ts$el_time<- NA
    
    for(l in 1:nrow(trial_ts)){
      
      string<- as.numeric(unlist(strsplit(trial_ts$value[l], ',')))
      
      if(length(string)==1){
        next
      }
      
      trial_ts$x[l]<- string[1]#*width_multiplier # change to native width
      trial_ts$y[l]<- string[2]#*height_multiplier # change to native height
      trial_ts$time[l]<- string[3]
      trial_ts$conf[l]<- string[4]
      
    }
    
    # remove variables we don't need:
    trial_ts$value<- NULL
    trial_ts$variable_name<- NULL
    trial_ts$Block_Name <- NULL
    trial_ts$Block_Nr<- NULL
    trial_ts$Task_Nr<- NULL
    trial_ts$Session_Nr<- NULL
    trial_ts$Exp_Subject_Id<- NULL
    trial_ts$Rec_Session_Id<- NULL
    
    trial_ts<- subset(trial_ts, time>= start_time & time<end_time)
    
    # standardise timestamps relative to screen start time:
    trial_ts$time_start<- trial_ts$time- start_time
    
    # calculate time difference from previous sample:
    trial_ts<- trial_ts %>%
      mutate(time_diff = time_start - lag(time_start))
    
    
    for(l in 1:nrow(trial_ts)){
      loc<- which(sacc_samples$unix_time== round(trial_ts$time[l])) 
      
      if(length(loc)>0){
        trial_ts$el_x[l]<- sacc_samples$V2[loc]
        trial_ts$el_y[l]<- sacc_samples$V3[loc]
        trial_ts$el_pupil[l]<- sacc_samples$V4[loc]
        trial_ts$el_time[l]<- sacc_samples$V1[loc]
      }
    }
    
    
    
    eye_data<- rbind(eye_data, trial_ts)
    
    
  }
  



}

eye_data$el_x_std<- eye_data$el_x/2.4
eye_data$el_y_std<- eye_data$el_y/2.4

write.csv(eye_data, 'preproc/lab/raw_data.csv')

cor.test(eye_data$x, eye_data$el_x_std)
cor.test(eye_data$y, eye_data$el_y_std)
