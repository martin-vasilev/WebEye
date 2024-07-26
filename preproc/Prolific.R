
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
  
  tasks<- unique(trials$Task_Name)
  
  for(j in 1:length(tasks)){
    
    task_trials<- subset(trials, Task_Name== tasks[j])
    task_ts<- subset(ts, Task_Name== tasks[j])
    
    for(k in 1:nrow(task_trials)){
      
      start_time<- task_trials$trial_start[k]
      end_time<- task_trials$trial_end[k]
      
      trial_ts<- subset(task_ts, Trial_Id== task_trials$Trial_Id[k] & variable_name== 'gaze_data') 
      
      
      trial_ts$Frequency<- task_trials$Frequency[k]
      trial_ts$Preview<- task_trials$Preview[k]

      trial_ts$x<- NA
      trial_ts$y<- NA
      trial_ts$time<- NA
      trial_ts$conf<- NA
    
      
      for(l in 1:nrow(trial_ts)){
        
        string<- as.numeric(unlist(strsplit(trial_ts$value[l], ',')))
        
        if(length(string)==1){
          next
        }
        
        trial_ts$x[l]<- string[1]*width_multiplier # change to native width
        trial_ts$y[l]<- string[2]*height_multiplier # change to native height
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
        mutate(time_diff = time - lag(time, default = first(time)))
      
      eye_data<- rbind(eye_data, trial_ts)
      
      
    }
    
    
  }
  
}


write.csv(eye_data, 'preproc/prolific/eye_data.csv')



DC<- subset(eye_data, Task_Name== 'sentence_DC')

library(readxl)
corpus_dc <- read_excel("preproc/prolific/Corpus_fq.xlsx", 
                               sheet = "DC")


library(saccades)


subs<- unique(DC$Exp_Subject_Id)
fix<- NULL

for(i in 1:length(subs)){
  
  n<- subset(DC, Exp_Subject_Id==subs[i])
  
  a<- n[, c('time', 'x', 'y', 'Trial_Id')]
  colnames(a)<- c('time', 'x', 'y', 'trial', 'sub')
  
  b<- detect.fixations(a)
  
  

    
}

a<- subset(eye_data, Task_Name== 'sentence'  &Trial_Id== 2 & Exp_Subject_Id==953681)

sent<- c('The house\nwas recognisable by\nits green fence and\nbig windows.')

x_offset<- 125
y_offset<- 112    
ppl<- 78
linespan<- 233

lines<- unlist(strsplit(sent, '\n'))

coords<- NULL

for(i in 1:length(lines)){
  
  if(i==1){ # if first line
    
    for(j in 1:nchar(lines[i])){
      
      if(j==1){
        x1<- x_offset
        x2<- x_offset + ppl
        y1<- y_offset
        y2<- y_offset+ linespan
        char<- substr(lines[i], j, j)

      }else{
        x1<- coords$x2[j-1] +1
        x2<- x1+ ppl
        y1<- y_offset
        y2<- y_offset+ linespan
        char<- substr(lines[i], j, j)
      }
      t<- data.frame('x1'=x1, 'x2'=x2, 'y1'=y1, 'y2'=y2, 'char'= char)
      coords<- rbind(coords, t)
      
    }
    
  }else{
    
    for(j in 1:nchar(lines[i])){
      
      if(j==1){
        x1<- x_offset
        x2<- x1+ ppl
        y1<- coords[nrow(coords), 'y2' ]+1
        y2<- y1+ linespan
        char<- substr(lines[i], j, j)
      }else{
        x1<- coords[nrow(coords), 'x2' ]+1
        x2<- x1+ ppl
        y1<- coords[nrow(coords), 'y1' ]
        y2<- coords[nrow(coords), 'y2' ]
        char<- substr(lines[i], j, j)
      }
      
      t<- data.frame('x1'=x1, 'x2'=x2, 'y1'=y1, 'y2'=y2, 'char'= char)
      coords<- rbind(coords, t)
      
    }
    
  }
  
}




## map words to dataframe:

coords$wordID<- NA
coords$char_num<- NA

line_breaks<- which(diff(coords$x1)<0)
new_coords<-NULL

for(i in 1:(length(line_breaks)+1)){
  
  if(i==1){
    t<- coords[1:line_breaks[i],]
  }else{
    
    if(i== length(line_breaks)+1){
      t<- coords[(line_breaks[i-1]+1): nrow(coords),]
    }else{
      t<- coords[(line_breaks[i-1]+1):line_breaks[i],]
    }
    
  }
  
  empty_spaces<- c(which(t$char== ' '), nrow(t))
  
  for(j in 1:length(empty_spaces)){
    if(j==1){
      t[1:(empty_spaces[j]-1), 'wordID']<- paste(t[1:(empty_spaces[j]-1), 'char'], collapse = '')
      t[1:(empty_spaces[j]-1), 'char_num']<- 1:(empty_spaces[j]-1)
    }else{
      
      t[empty_spaces[j-1] : empty_spaces[j], 'wordID']<- paste(t[empty_spaces[j-1] : empty_spaces[j], 'char'], collapse='')
      t[empty_spaces[j-1] : empty_spaces[j], 'char_num']<- 0:(empty_spaces[j]-empty_spaces[j-1])
      
      
    }
  }
  
  new_coords<- rbind(new_coords, t)

}




for(i in 1:length(empty_spaces)){
  
  if(i==1){
    coords[1:(empty_spaces[i]-1), 'wordID']<- paste(coords[1:(empty_spaces[i]-1), 'char'], collapse = '')
    coords[1:(empty_spaces[i]-1), 'char_num']<- 1:(empty_spaces[i]-1)
    
  }else{
    
    if(!is.na(coords[empty_spaces[i-1], 'wordID'])){
      
      coords[(empty_spaces[i-1]+1) : (empty_spaces[i]-1), 'wordID']<- paste(coords[(empty_spaces[i-1]+1) : (empty_spaces[i]-1), 'char' ], collapse = '')
      

      
    }
    
    coords[empty_spaces[i-1] : empty_spaces[i], 'wordID']<- paste(coords[empty_spaces[i-1] : empty_spaces[i], 'char'], collapse='')
    coords[empty_spaces[i-1] : empty_spaces[i], 'char_num']<- 0:(empty_spaces[i]-empty_spaces[i-1])
  }
  
}

## revert back to original frame size:
coords$x1<- coords$x1/2.4
coords$x2<- coords$x2/2.4
coords$y1<- coords$y1/2.4
coords$y2<- coords$y2/2.4

