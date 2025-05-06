
rm(list= ls())

options(scipen=999)

library(readr)
library(tidyverse)

# load data folders:
folders<- list.dirs('preproc/Provo/')
folders<- folders[2:length(folders)]

eye_data<- NULL

for(i in 1:length(folders)){ # for each subject folder
  
  # open files:
  folder_dir<- folders[i]
  
  # get study list
  #list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  # trial info data:
  trials <- read_csv(paste(folder_dir, '/trials.csv', sep=''))
  trials<- subset(trials, Task_Name!= 'instructions' & Task_Name!= 'Questions')#|Task_Name== 'sentence_DC' )
  
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
  ts<- subset(ts, Task_Name!= 'instructions' & Task_Name!= 'Questions')#|Task_Name== 'sentence_DC')
  
  #ts$list<- list
  
  tasks<- unique(trials$Task_Name)
  
  for(j in 1:length(tasks)){ # for each task
    
    task_trials<- subset(trials, Task_Name== tasks[j])
    task_ts<- subset(ts, Task_Name== tasks[j])
    
    for(k in 1:nrow(task_trials)){ # for each trial...
      
      start_time<- task_trials$P1_start[k]
      end_time<- max(task_trials$P4_end[k], task_trials$P5_end[k], na.rm = T) 
      
      trial_ts<- subset(task_ts, variable_name== 'gaze_data') 
      
      
      trial_ts$x<- NA
      trial_ts$y<- NA
      trial_ts$time<- NA
      trial_ts$conf<- NA
      trial_ts$Page<- NA
      
      
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
        
        # map samples to pages:
        
        # Page 1:
        if(!is.na(task_trials$P1_start[k])& !is.na(task_trials$P1_end[k])){
          
          if(trial_ts$time[l]>= task_trials$P1_start[k] & trial_ts$time[l]<= task_trials$P1_end[k]){
            trial_ts$Page[l]<- 1
          }
          
        }
        
        # Page 2:
        if(!is.na(task_trials$P2_start[k])& !is.na(task_trials$P2_end[k])){
          
          if(trial_ts$time[l]>= task_trials$P2_start[k] & trial_ts$time[l]<= task_trials$P2_end[k]){
            trial_ts$Page[l]<- 2
          }
          
        }
        
        # Page 3:
        if(!is.na(task_trials$P3_start[k])& !is.na(task_trials$P3_end[k])){
          
          if(trial_ts$time[l]>= task_trials$P3_start[k] & trial_ts$time[l]<= task_trials$P3_end[k]){
            trial_ts$Page[l]<- 3
          }
          
        }
        
        
        # Page 4:
        if(!is.na(task_trials$P4_start[k])& !is.na(task_trials$P4_end[k])){
          
          if(trial_ts$time[l]>= task_trials$P4_start[k] & trial_ts$time[l]<= task_trials$P4_end[k]){
            trial_ts$Page[l]<- 4
          }
          
        }
        
        
        # Page 5:
        if(!is.na(task_trials$P5_start[k])& !is.na(task_trials$P5_end[k])){
          
          if(trial_ts$time[l]>= task_trials$P4_end[k] & trial_ts$time[l]<= task_trials$P5_end[k]){
            trial_ts$Page[l]<- 5
          }
          
        }
        
      } # end of l (fixation) loop
      
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


write.csv(eye_data, 'preproc/Provo/Provo_eye_data_raw.csv')


library(readr)
Corpus <- read_csv("preproc/Provo_stimuli.csv")

source('preproc/functions/get_coords.R')
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}


dat<- NULL

eye_data$Task_Name<- get_num(eye_data$Task_Name)

eye_data$wordID<- NA
eye_data$char<- NA
eye_data$char_num<- NA
eye_data$target_word<- NA
eye_data$word_num<- NA

library(stringr)

nsubs<- unique(eye_data$Exp_Subject_Id)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(eye_data, Exp_Subject_Id== nsubs[k])
  
  nitems<- unique(eye_data$Task_Name) 
  
  for(l in 1:length(nitems)){
    c<- subset(a, Task_Name== nitems[l])
    
    npages<-  unique(eye_data$Page)
    npages<- npages[which(!is.na(npages))]
    
    for(i in 1:length(nitems)){ # for each item...
      
      b<- subset(c, Page== npages[i])
      
      
      if(nrow(b)<1){
        next;
      }
      
      
      sent<- Corpus$Text[which(Corpus$Item== b$Task_Name[i] & Corpus$Page== b$Page[1])]
      coords<- get_coords(sent)
      
      
      for(j in 1:nrow(b)){ # for each fixation
        
        loc<- which(coords$x1<= b$x[j] & coords$x2>= b$x[j] & coords$y1<= b$y[j] & coords$y2>= b$y[j])
        
        if(length(loc)>0){
          b$wordID[j]<- str_trim(coords$wordID[loc])
          b$char[j]<- coords$char[loc]
          b$char_num[j]<- coords$char_num[loc]
          b$word_num[j]<- coords$word_num[loc]
          
          # if(!is.na(coords$wordID[loc])){
          #   if(b$wordID[j]== target){
          #     b$target_word[j]<- "Yes"
          #   }else{
          #     b$target_word[j]<- "No"
          #   }
          # }
          
        }
        
      }
      
      dat<- rbind(dat, b)
      
      
    }
    
  }
  
    

  

  
  cat(k); cat(' ')
  
}


dat$Trial_Nr<- NULL
dat$Trial_Id<- NULL

colnames(dat)<- c("Provo_item_number", "sub",
                  "timestamp", "x",             
                  "y", "time", "conf", "Page", "time_diff", "wordID",        
                  "char", "char_num", "target_word", "word_number")

#b$wordID<- gsub(" ", "", b$wordID, fixed = TRUE)

dat$target_word<- NULL
dat$word_length<- nchar(dat$wordID)

library(EMreading)
dat<- Frequency(dat)
#dat$lexical_freq<- dat$zipf

write.csv(dat, file= "preproc/Provo_data.csv")


Corpus <- read_csv("preproc/Provo_stimuli.csv")


nsubs<- unique(dat$sub)

new<- NULL

for (i in 1:length(nsubs)){
  
  sub<- subset(dat, sub== nsubs[i])
  
  nitems<- unique(sub$Provo_item_number)
  
  for(j in 1:length(nitems)){
    
    item<- subset(sub, Provo_item_number== nitems[j])
    
    npages<- unique(item$Page)
    
    for(k in 1:length(npages)){
      
      page<- subset(item, Page== npages[k])
      
      unique_ids<- unique(page$word_number)
      
      t<- data.frame('sub'= nsubs[i], 'item'= nitems[j], 'Page'= npages[k])
      
      # get unique words in the page:
      
      loc<- which(Corpus$Item== page$Provo_item_number[1] & Corpus$Page== page$Page[1])
      
      string<- Corpus$Text[loc]
      string= paste(unlist(strsplit(string, "@")), collapse= ' ')
      words<- unlist(strsplit(string, ' '))
      
      t<- t %>% slice(rep(row_number(), length(words)))
      t$wordID<- words
      t$word_number<- 1:length(words)
      
      t$skipped<- NA
      
      for(l in 1:nrow(t)){
        
        t$skipped[l]<- ifelse(is.element(t$word_number[l], unique_ids), 0, 1 )
        
      }
      
      new<- rbind(new, t)
      
      
      }
    
  }
  
  
}

write.csv(new, file = 'preproc/Provo_skipping.csv')


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
  summarise(TVT= sum(time_diff, na.rm = T))

target_data<- target_data%>%
  filter(TVT>0& TVT<4000)

summary(M2<- lmer(log(TVT) ~ Frequency +(Frequency|sub)+ (1|item), data= target_data))

plot(ggeffect(M2, terms = 'Frequency'))

