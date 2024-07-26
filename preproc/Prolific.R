
rm(list= ls())

options(scipen=999)

library(readr)
library(tidyverse)

folders<- list.dirs('preproc/prolific/')
folders<- folders[2:length(folders)]

eye_data<- NULL

for(i in 1:length(folders)){
  
  # open files:
  folder_dir<- folders[i]
  
  list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  trials <- read_csv(paste(folder_dir, '/trials.csv', sep=''))
  trials<- subset(trials, Task_Name== 'sentence'|Task_Name== 'sentence_DC' )
  
  # info:
  info<- read_csv(paste(folder_dir, '/sessions.csv', sep=''))
  
  ## screen settings:
  scr_width<- info$Screen_Width_In_Pixels
  scr_height<- info$Screen_Height_In_Pixels
  
  scr_width/scr_height
  
  native_width<- 800
  native_height<- 450
  
  width_multiplier<- scr_width/native_width
  height_multiplier<- scr_height/native_height
  
  # timeseries:
  ts<- read_csv(paste(folder_dir, '/timeseries.csv', sep=''))
  ts<- subset(ts, Task_Name== 'sentence'|Task_Name== 'sentence_DC')
  
  ts$list<- list
  
  tasks<- unique(trials$Task_Name)
  
  for(j in 1:length(tasks)){
    
    task_trials<- subset(trials, Task_Name== tasks[j])
    task_ts<- subset(ts, Task_Name== tasks[j])
    
    for(k in 1:nrow(task_trials)){
      
      start_time<- task_trials$trial_start[k]
      end_time<- task_trials$trial_end[k]
      
      trial_ts<- subset(task_ts, Trial_Id== task_trials$Trial_Id[k] & variable_name== 'gaze_data') 
      
      
      if(list=='A'){
        trial_ts$Frequency<- task_trials$Frequency[k]
        trial_ts$Preview<- task_trials$Preview[k]
      }else{
        trial_ts$Frequency<- ifelse(task_trials$Frequency[k]=='low', 'high', ifelse(task_trials$Frequency[k]=='high', 'low', NA))
        trial_ts$Preview<- ifelse(task_trials$Preview[k]== 'valid', 'invalid', ifelse(task_trials$Preview[k]== 'invalid', 'valid', NA))
      }
      


      trial_ts$x<- NA
      trial_ts$y<- NA
      trial_ts$time<- NA
      trial_ts$conf<- NA
    
      
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
      
      ## take only data when the sentence was read:
      trial_ts<- subset(trial_ts, time>= start_time & time<end_time)
      
      # standardise timestamps relative to screen start time:
      trial_ts$time<- trial_ts$time- start_time
      
      # calculate time difference from previous sample:
      trial_ts<- trial_ts %>%
        mutate(time_diff = time - lag(time))
      
      eye_data<- rbind(eye_data, trial_ts)
      
      
    }
    
    
  }
  
}


write.csv(eye_data, 'preproc/prolific/eye_data.csv')



sent<- subset(eye_data, Task_Name== 'sentence')

library(readr)
Corpus_fq <- read_csv("preproc/prolific/Corpus_fq.csv")


library(saccades)


subs<- unique(DC$Exp_Subject_Id)
fix<- NULL

for(i in 1:length(subs)){
  
  n<- subset(DC, Exp_Subject_Id==subs[i])
  
  a<- n[, c('time', 'x', 'y', 'Trial_Id')]
  colnames(a)<- c('time', 'x', 'y', 'trial', 'sub')
  
  b<- detect.fixations(a)
  
  

    
}

a<- subset(eye_data, Task_Name== 'sentence'  &Trial_Id< 31)

source('preproc/functions/get_coords.R')
dat<- NULL

nitems<- sort(unique(a$Trial_Id))

for(i in 1:length(nitems)){
  b<- subset(a, Trial_Id== nitems[i])
  
  sent<-Corpus_fq$line_breaks[which(Corpus_fq$Study_ID== nitems[i])[1]]
  coords<- get_coords(sent)
  
  b$wordID<- NA
  b$char<- NA
  b$char_num<- NA
  
  for(i in 1:nrow(b)){
    
    loc<- which(coords$x1<= b$x[i] & coords$x2>= b$x[i] & coords$y1<= b$y[i] & coords$y2>= b$y[i])
    
    if(length(loc)>0){
      b$wordID[i]<- coords$wordID[loc]
      b$char[i]<- coords$char[loc]
      b$char_num[i]<- coords$char_num[loc] 
    }
    
  }
  
  dat<- rbind(dat, b)
  
}

colnames(b)<- c("seq", "trial", "Task_Name", "sub", "Rec_Session_Id",
                "timestamp", "list", "Frequency", "Preview", "x",             
                "y", "time", "conf", "time_diff", "wordID",        
                "char", "char_num", "word_length")

b$wordID<- gsub(" ", "", b$wordID, fixed = TRUE)

tab<- b %>% group_by(sub, wordID) %>% summarise(TVT= sum(time_diff))
tab$word_length<- nchar(tab$wordID)

freq<- Frequency(tab)
tab$lexical_freq<- freq$zipf

plot(tab$word_length, tab$TVT)

tab$lexical_freq_c= scale(tab$lexical_freq, center = T, scale = F)
tab$word_length_c= scale(tab$word_length, center = T, scale = F)


summary(M1<- lm(TVT ~ word_length_c*lexical_freq_c, data= tab))

plot(effect('lexical_freq_c', M1))

