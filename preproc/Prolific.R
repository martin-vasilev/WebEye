
rm(list= ls())

options(scipen=999)

library(readr)
library(tidyverse)

# load data folders:
folders<- list.dirs('preproc/prolific/')
folders<- folders[2:length(folders)]

eye_data<- NULL

for(i in 1:length(folders)){ # for each subject folder
  
  # open files:
  folder_dir<- folders[i]
  
  # get study list
  list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  # trial info data:
  trials <- read_csv(paste(folder_dir, '/trials.csv', sep=''))
  trials<- subset(trials, Task_Name== 'sentence')#|Task_Name== 'sentence_DC' )
  
  # info:
  info<- read_csv(paste(folder_dir, '/sessions.csv', sep=''))
  
  ## screen settings for subject:
  scr_width<- info$Screen_Width_In_Pixels
  scr_height<- info$Screen_Height_In_Pixels
  
  #scr_width/scr_height
  
  native_width<- 800
  native_height<- 450
  
  width_multiplier<- scr_width/native_width
  height_multiplier<- scr_height/native_height
  
  # timeseries (i.e., eye-tracking data):
  ts<- read_csv(paste(folder_dir, '/timeseries.csv', sep=''))
  ts<- subset(ts, Task_Name== 'sentence')#|Task_Name== 'sentence_DC')
  
  ts$list<- list
  
  tasks<- unique(trials$Task_Name)
  
  for(j in 1:length(tasks)){ # for each task
    
    task_trials<- subset(trials, Task_Name== tasks[j])
    task_ts<- subset(ts, Task_Name== tasks[j])
    
    for(k in 1:nrow(task_trials)){ # for each trial...
      
      start_time<- task_trials$trial_start[k]
      end_time<- task_trials$trial_end[k]
      
      trial_ts<- subset(task_ts, Trial_Id== task_trials$Trial_Id[k] & variable_name== 'gaze_data') 
      
      
      if(list=='A'){
        trial_ts$Frequency<- task_trials$Frequency[k]
        trial_ts$Preview<- task_trials$Preview[k]
        
      }else{ # frequency labels are reversed for list B (difficult to change on website)
        trial_ts$Frequency<- ifelse(task_trials$Frequency[k]=='low', 'high', ifelse(task_trials$Frequency[k]=='high', 'low', NA))
        trial_ts$Preview<- ifelse(task_trials$Preview[k]== 'valid', 'invalid', ifelse(task_trials$Preview[k]== 'invalid', 'valid', NA))
      }
      

      trial_ts$x<- NA
      trial_ts$y<- NA
      trial_ts$time<- NA
      trial_ts$conf<- NA
    
      
      for(l in 1:nrow(trial_ts)){
        
        # take eye-tracking data as string
        string<- as.numeric(unlist(strsplit(trial_ts$value[l], ',')))
        
        if(length(string)==1){ # if no data, go to next sample
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
      trial_ts$Preview<- NULL
      trial_ts$Rec_Session_Id<- NULL
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


write.csv(eye_data, 'preproc/prolific/eye_data_raw.csv')



sent<- subset(eye_data, Task_Name== 'sentence')

library(readr)
Corpus_fq <- read_csv("preproc/prolific/Corpus_fq.csv")
Corpus_fq<- Corpus_fq[1:120,]

#library(saccades)

# subs<- unique(DC$Exp_Subject_Id)
# fix<- NULL

# for(i in 1:length(subs)){
#   
#   n<- subset(DC, Exp_Subject_Id==subs[i])
#   
#   a<- n[, c('time', 'x', 'y', 'Trial_Id')]
#   colnames(a)<- c('time', 'x', 'y', 'trial', 'sub')
#   
#   b<- detect.fixations(a)
#   
#   
# 
#     
# }

#a<- subset(eye_data, Task_Name== 'sentence'  &Trial_Id< 31)

source('preproc/functions/get_coords.R')
dat<- NULL

eye_data$wordID<- NA
eye_data$char<- NA
eye_data$char_num<- NA
eye_data$target_word<- NA

library(stringr)

nsubs<- unique(eye_data$Exp_Subject_Id)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(eye_data, Exp_Subject_Id== nsubs[k])
  
  nitems<- unique(eye_data$Trial_Id)
  
  for(i in 1:length(nitems)){ # for each item...
    
    b<- subset(a, Trial_Id== nitems[i])
    
    freq<- ifelse(b$Frequency[1]== 'low', 'LF', 'HF')
    
    sent<-Corpus_fq$line_breaks[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
    coords<- get_coords(sent)
    
    target<- Corpus_fq$`Target (N)`[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
    
    
    for(j in 1:nrow(b)){ # for each fixation
      
      loc<- which(coords$x1<= b$x[j] & coords$x2>= b$x[j] & coords$y1<= b$y[j] & coords$y2>= b$y[j])
      
      if(length(loc)>0){
        b$wordID[j]<- str_trim(coords$wordID[loc])
        b$char[j]<- coords$char[loc]
        b$char_num[j]<- coords$char_num[loc] 
        
        if(!is.na(coords$wordID[loc])){
          if(b$wordID[j]== target){
            b$target_word[j]<- "Yes"
          }else{
            b$target_word[j]<- "No"
          }
        }

      }
      
    }
    
    dat<- rbind(dat, b)
    
    
  }
  
  cat(k); cat(' ')
  
}




colnames(dat)<- c("seq", "item", "Task_Name", "sub",
                "timestamp", "list", "Frequency", "x",             
                "y", "time", "conf", "time_diff", "wordID",        
                "char", "char_num", "target_word")

#b$wordID<- gsub(" ", "", b$wordID, fixed = TRUE)

dat$word_length<- nchar(dat$wordID)


tab<- dat %>% group_by(sub, item, wordID) %>% summarise(TVT= sum(time_diff, na.rm = T))
tab$word_length<- nchar(tab$wordID)

library(EMreading)
freq<- Frequency(tab)
tab$lexical_freq<- freq$zipf

plot(tab$word_length, tab$TVT)

tab$lexical_freq_c= scale(tab$lexical_freq, center = T, scale = F)
tab$word_length_c= scale(tab$word_length, center = T, scale = F)

library(lmerTest)

summary(M1<- lmer(TVT ~ word_length_c*lexical_freq_c +(1|sub), data= tab))

library(ggeffects)
plot(ggeffect(M1, terms = c('lexical_freq_c', 'word_length_c' )))


target_data<- subset(dat, target_word== 'Yes')

target_data<- target_data %>% 
  group_by(sub, item, Frequency, wordID) %>%
  summarise(TVT= sum(time_diff, na.rm = T))%>%
  filter(TVT<5000)

summary(M2<- lmer(TVT ~ Frequency +(1|sub)+ (1|item), data= target_data))

plot(ggeffect(M2, terms = 'Frequency'))

