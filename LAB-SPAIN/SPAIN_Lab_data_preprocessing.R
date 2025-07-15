
rm(list= ls())

options(scipen=999)

source('preproc/functions/all_functions.R')

library(readr)
library(tidyverse)

folders<- list.dirs('LAB-SPAIN/raw_data')
folders<- folders[2:length(folders)]

eye_data<- NULL
trial_data<- NULL
el_fix_data<- NULL
el_raw<- NULL


q<- NULL

for(i in 1:length(folders)){ # for each subject

  # open files:
  folder_dir<- folders[i]
  
  sub= unlist(strsplit(folder_dir, '/'))[3]
  sub= get_num(sub)
  
  list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
  
  trials <- suppressMessages(read_csv(paste(folder_dir, '/trials.csv', sep='')))
  trials<- subset(trials, Task_Name== 'experimental_task_A'|Task_Name== 'experimental_task_B' |Task_Name== 'experimentaltask_B' | Task_Name== 'experimentaltask_A')
  trials$list<- list
  
  # remove empty columns:
  trials<- trials[colSums(!is.na(trials)) > 0]
  
  trials$subject<- sub
  
  ## add missing columns
  
  if(!"frec"%in% colnames(trials)){
    trials$frec<-NA
  }
  
  
  if(!"answer_value"%in% colnames(trials)){
    trials$answer_value<-NA
  }
  
  if(!"has_question_value"%in% colnames(trials)){
    trials$has_question_value<-NA
  }
  
  if(!"index_value"%in% colnames(trials)){
    trials$index_value<-NA
  }
  
  if(!"question_value...36"%in% colnames(trials)){
    trials$question_value...36<-NA
  }
  
  if(!"question_value...37"%in% colnames(trials)){
    trials$question_value...37<-NA
  }
  
  if(!"sentence_line1_value"%in% colnames(trials)){
    trials$sentence_line1_value<-NA
  }
  
  if(!"sentence_line2_value"%in% colnames(trials)){
    trials$sentence_line2_value<-NA
  }
  
  
  
  trial_data<- rbind(trial_data, trials) # add to df
  
  
  ## eye-movement data files:
  
  # timeseries:
  ts<- suppressMessages(read_csv(paste(folder_dir, '/timeseries.csv', sep='')))
  ts<- subset(ts, Task_Name== 'experimental_task_B' |Task_Name=='experimental_task_A'|Task_Name== 'experimentaltask_B' | Task_Name== 'experimentaltask_B')
  
  ts$subject<- sub
  
  
  ## load eyelink data:
  dataF<- readLines(paste(folder_dir, '/', sub,  '.asc', sep='')) # load asc file;
  
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
  sacc_samples<- sacc_samples[!grepl("END", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("START", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("PRESCALER", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("VPRESCALER", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("PUPIL", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("EVENTS", sacc_samples)]
  sacc_samples<- sacc_samples[!grepl("SAMPLES", sacc_samples)]
  
  
  
  
  
  
  sacc_samples <-  as.data.frame(do.call( rbind, strsplit( sacc_samples, '\t' ) )) # V2 is xpos
  sacc_samples$V1<- as.numeric(sacc_samples$V1)
  sacc_samples$V2<- as.numeric(sacc_samples$V2)
  sacc_samples$V3<- as.numeric(sacc_samples$V3)
  sacc_samples$V4<- as.numeric(sacc_samples$V4)
  sacc_samples$V5<- as.numeric(sacc_samples$V5)
  sacc_samples$V6<- as.numeric(sacc_samples$V6)
  sacc_samples$V7<- as.numeric(sacc_samples$V7)
  
  sacc_samples$start_time<- sacc_samples$V1- el_start_time
  sacc_samples$unix_time<- sacc_samples$start_time+start
  
  cat(sprintf('\n\nSubject %g, Item: ',sub ))
  
  ntasks<- unique(ts$Task_Name)
  
  for(t in 1:length(ntasks)){
    
    n<- subset(trials, Task_Name==ntasks[t])
    
    
    for(j in 1:nrow(n)){
      
      cat(sprintf('%g ', j))
      
      start_time<- n$trial_start[j]
      end_time<- n$trial_end[j]
      
      trial_ts<- subset(ts, Trial_Id== n$Trial_Id[j] & variable_name== 'gaze_Frame_2' & Task_Name== n$Task_Name[j]) 
      
      if(is.na(start_time)){
        start_time<- trial_ts$timestamp[2]
      }
      
      trial_ts$x<- NA
      trial_ts$y<- NA
      trial_ts$time<- NA
      trial_ts$conf<- NA
      
      trial_ts$el_x<- NA
      trial_ts$el_y<- NA
      trial_ts$el_pupil_left<- NA
      trial_ts$el_pupil_right<- NA
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
          trial_ts$el_x[l]<- (sacc_samples$V2[loc] + sacc_samples$V5[loc])/2
          trial_ts$el_y[l]<- (sacc_samples$V3[loc] + sacc_samples$V6[loc])/2
          trial_ts$el_pupil_left[l]<- sacc_samples$V4[loc]
          trial_ts$el_pupil_right[l]<- sacc_samples$V7[loc]
          trial_ts$el_time[l]<- sacc_samples$V1[loc]
        }
      }
      
      ## add frequency information:
      trial_ts$Freq<- n$frec[j]
      
      
      if(nrow(trial_ts)>0){
        try(eye_data<- rbind(eye_data, trial_ts))
      }
      
      ### Save raw Eyelink samples for trial:
      
      el_s<- sacc_samples[which(sacc_samples$unix_time== start_time):which(sacc_samples$unix_time== end_time),]
      
      el_s$V8<- NULL
      el_s$V9<- NULL
      el_s$start_time<- NULL
      
      colnames(el_s)<- c('el_time', 'left_x', 'left_y', 'left_pupil', 'right_x',
                         'right_y', 'right_pupil', 'Unix_time')
      
      el_s$subject<- n$sub[j]
      el_s$item<- n$Trial_Id[j]
      el_s$task<- n$Task_Name[j]
      
      el_raw<- rbind(el_raw, el_s)
      
      ### save eyelink data for the whole trial:
      
      start_time_el<- sacc_samples$V1[which(sacc_samples$unix_time== start_time)]
      
      end_time_el<- sacc_samples$V1[which(sacc_samples$unix_time== end_time)]
      
      trial_file_el<- dataF[which(grepl(as.character(start_time_el), dataF)):which(grepl(as.character(end_time_el), dataF))]
      
      
      ## get all fixation stamps for extraction and processing:
      # get position of fixation stamps:
      SFIX_stamps<- which(grepl('SFIX R', trial_file_el))
      EFIX_stamps<- which(grepl('EFIX R', trial_file_el))
      if(length(SFIX_stamps)==0 | length(EFIX_stamps)==0){
        # means that there was no complete fixation on this trial (i.e,
        # participant likely pressed end button by mistake)
        message(sprintf("No fixations in subject %d Trial %d: excluded",i, j))
        next;
        
      }
      
      if(length(EFIX_stamps)>0 & length(SFIX_stamps[1])>0){
        
        if(EFIX_stamps[1]<SFIX_stamps[1]){ # removes fixation that triggered gaze box
          EFIX_stamps<- EFIX_stamps[-1]
          
          if(length(EFIX_stamps)==0){
            next # no fixations left, skip trial
          }
        }
        
        if(length(EFIX_stamps[length(EFIX_stamps)])>0 & length(SFIX_stamps[length(SFIX_stamps)])>0){
          
          if(EFIX_stamps[length(EFIX_stamps)]< SFIX_stamps[length(SFIX_stamps)]){
            SFIX_stamps<- SFIX_stamps[-length(SFIX_stamps)]
          } # fixation was not terminated before the end of trial
          
        }
        
      }
      

      
      parse_sacc<- function(string){a<- unlist(strsplit(string, "\t")); return(as.numeric(a[3]))}
      
      esacc_flag<- trial_file_el[SFIX_stamps-1]
      
      saccDur<- NULL
      for(k in 1:length(esacc_flag)){
        saccDur[k]<- parse_sacc(esacc_flag[k])
      }
      
      
      # get start and end time of fixations:
      s_time<- get_num(trial_file_el[SFIX_stamps])  # start time of fixation
      e_time<- NA
      
      for(s in 1:length(EFIX_stamps)){
        e_time[s]<-  trial_file_el[EFIX_stamps]
      }
      
      e_time<- get_FIX_stamp(trial_file_el[EFIX_stamps]) # end time of fixation
      
      # calculate fixation durations:
      fixDur<- e_time- s_time
      
      # get x pixel position:
      x<- get_x_pixel(trial_file_el[EFIX_stamps])
      
      # get y pixel position:
      y<- get_y_pixel(trial_file_el[EFIX_stamps])
      
      
      if(length(saccDur)<length(fixDur)){
        
        saccDur[(length(saccDur)+1):length(fixDur)]<- NA
      }
      
      
      fix<- data.frame(sub= sub , item= trials$Trial_Id[j], cond= trials$frec[j],
                       task= trials$Task_Name[j], s_time, e_time, fixDur, saccDur, x, y,
                       blink_before = NA,blink_after = NA,fix_num = NA) #blink, prev_blink, after_blink)
      
      
      fix$fix_num<- 1:nrow(fix)
      
      if(nrow(fix)>500){
        stop('too many fixations!')
      }
      
      for(k in 1:nrow(fix)){
        
        if(!is.na(fix$sub[k+1])){
          fix_between <- trial_file_el[SFIX_stamps[k]:SFIX_stamps[k+1]]
          blink_stamp_s <- fix_between[which(grepl('SBLINK',fix_between))]
          blink_stamp_e <- fix_between[which(grepl('EBLINK',fix_between))]
          
          if(length(blink_stamp_s)!=0 | length(blink_stamp_e)!=0){
            fix$blink_after[k] <- 1
            fix$blink_before[k+1] <-1
          }
        }
        
      }
      
      el_fix_data<- rbind(el_fix_data, fix)
      
      
      
      
      
      
    } # end of trial loop
    
    
  } # end of task loop
  
  


} # end of subject loop

#eye_data$el_x_std<- eye_data$el_x/2.4
#eye_data$el_y_std<- eye_data$el_y/2.4

#eye_data$Task_Name[which(eye_data$Task_Name=='sentence')]<-'Freq_sentences'
#eye_data$Task_Name[which(eye_data$Task_Name=='sentence_DC')]<-'Single_line_sentences'


# webcam data:
write.csv(eye_data, 'LAB-SPAIN/data/SPAIN_webcam_raw_data.csv')
write.csv(trial_data, 'LAB-SPAIN/data/SPAIN_trial_data.csv')

# eye-link fix data:
write.csv(el_fix_data, 'LAB-SPAIN/data/SPAIN_eyelink_fix_data.csv')

# eyelink raw samples for each trial:
write.csv(el_raw, 'LAB-SPAIN/data/SPAIN_eyelink_raw_samples.csv')
#save(el_raw, file = 'LAB/data/eyelink_raw_samples.Rda')

cor.test(eye_data$x, eye_data$el_x)
cor.test(eye_data$y, eye_data$el_y)


##### Map words to eye-tracking data:

rm(list= ls())

library(readr)
el <- read_csv("LAB-SPAIN/data/SPAIN_eyelink_fix_data.csv")


Corpus_A <- read_csv("LAB-SPAIN/sentences_fullA.csv")
Corpus_B <- read_csv("LAB-SPAIN/sentences_fullB_final.csv")

Corpus_B$index<- Corpus_B$index -100# bring to same id as list A 


source('LAB-SPAIN/get_coords.R')
dat<- NULL

el$wordID<- NA
el$char<- NA
el$char_num<- NA
el$target_word<- NA
el$word_num<- NA

library(stringr)

nsubs<- unique(el$sub)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(el, sub== nsubs[k])
  
  ntasks<- unique(a$task)
  
  for(l in 1:length(ntasks)){
    n<- subset(a, task== ntasks[l])
    
    nitems<- unique(n$item)
    
    for(i in 1:length(nitems)){ # for each item...
      
      b<- subset(n, item== nitems[i]& task== ntasks[l])
      
      # find list:
      list= substr(b$task[1], nchar(b$task[1]), nchar(b$task[1]))
    
      # get sentence coordinates and target word:
      if(list=="A"){
        
         which_row<- which(Corpus_A$index== b$item[1])
         freq<- Corpus_A$frecA[which_row]
         target<- ifelse(freq== 'low', Corpus_A$word_low[which_row], Corpus_A$word_high[which_row])
         
         sent<- paste(Corpus_A$sentence1A[which_row],
                      Corpus_A$sentence2A[which_row], sep= '@' )

             
      }else{
        which_row<- which(Corpus_B$index== b$item[1])
        freq<- Corpus_B$frecB[which_row]
        target<- ifelse(freq== 'low', Corpus_B$word_low[which_row], Corpus_B$word_high[which_row])
        
        
        
        sent<- paste(Corpus_B$sentece1B[which_row],
                     Corpus_B$sentence2B[which_row], sep= '@' )
        
      }
      
      coords<- get_coords(sent, revert = T)
      
      
      for(j in 1:nrow(b)){ # for each fixation
        
        loc<- which(coords$x1<= b$x[j] & coords$x2>= b$x[j] & coords$y1<= b$y[j] & coords$y2>= b$y[j])
        
        if(length(loc)>0){
          b$wordID[j]<- str_trim(coords$wordID[loc])
          b$char[j]<- coords$char[loc]
          b$char_num[j]<- coords$char_num[loc]
          b$word_num[j]<- coords$word_num[loc]
          
          
          
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
    
  }
  

  

  
  cat(k); cat(' ')
  
}

# eye-link fix data:
write.csv(dat, 'LAB/data/eyelink_fix_data.csv')



### Pre-process webcam data:

rm(list= ls())


library(readr)
eye_data <- read_csv("LAB-SPAIN/data/SPAIN_webcam_raw_data.csv")

Corpus_A <- read_csv("LAB-SPAIN/sentences_fullA.csv")
Corpus_B <- read_csv("LAB-SPAIN/sentences_fullB_final.csv")

Corpus_B$index<- Corpus_B$index -100# bring to same id as list A 

source('LAB-SPAIN/get_coords.R')
dat<- NULL

eye_data$web_wordID<- NA
eye_data$web_char<- NA
eye_data$web_char_num<- NA
eye_data$web_target_word<- NA
eye_data$web_word_num<- NA

eye_data$el_wordID<- NA
eye_data$el_char<- NA
eye_data$el_char_num<- NA
eye_data$el_target_word<- NA
eye_data$el_word_num<- NA


library(stringr)

nsubs<- unique(eye_data$subject)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(eye_data, subject== nsubs[k])
  
  ntasks<- unique(a$Task_Name)
  
  for (l in 1:length(ntasks)){ # for each task...
    b<- subset(a, Task_Name==ntasks[l])
    
    nitems<- unique(b$Trial_Id)
    
    for(i in 1:length(nitems)){ # for each item...
      
      c<- subset(b, Trial_Id== nitems[i])
      
      
      # find list:
      list= substr(c$Task_Name[1], nchar(c$Task_Name[1]), nchar(c$Task_Name[1]))
      
      # get sentence coordinates and target word:
      if(list=="A"){
        
        which_row<- which(Corpus_A$index== c$Trial_Id[1])
        freq<- Corpus_A$frecA[which_row]
        target<- ifelse(freq== 'low', Corpus_A$word_low[which_row], Corpus_A$word_high[which_row])
        
        sent<- paste(Corpus_A$sentence1A[which_row],
                     Corpus_A$sentence2A[which_row], sep= '@' )
        
        
      }else{
        which_row<- which(Corpus_B$index== c$Trial_Id[1])
        freq<- Corpus_B$frecB[which_row]
        target<- ifelse(freq== 'low', Corpus_B$word_low[which_row], Corpus_B$word_high[which_row])
        
        
        sent<- paste(Corpus_B$sentece1B[which_row],
                     Corpus_B$sentence2B[which_row], sep= '@' )
        
      }
      
      coords<- get_coords(sent, revert = T)
      
      
      c$x<- c$x*2
      c$y<- c$y*2

      for(j in 1:nrow(c)){ # for each fixation
        
        
        ### map webcam samples:
        
        loc<- which(coords$x1<= c$x[j] & coords$x2>= c$x[j] & coords$y1<= c$y[j] & coords$y2>= c$y[j])
        
        if(length(loc)>0){
          c$web_wordID[j]<- str_trim(coords$wordID[loc])
          c$web_char[j]<- coords$char[loc]
          c$web_char_num[j]<- coords$char_num[loc]
          c$web_word_num[j]<- coords$word_num[loc]
          
          if(!is.na(coords$wordID[loc])){
            if(c$web_wordID[j]== target){
              c$web_target_word[j]<- "Yes"
            }else{
              c$web_target_word[j]<- "No"
            }
          }

          
          
        }
        
        #### map Eyelink samples:
        loc2<- which(coords$x1<= c$el_x[j] & coords$x2>= c$el_x[j] & coords$y1<= c$el_y[j] & coords$y2>= c$el_y[j])
        
        if(length(loc2)>0){
          c$el_wordID[j]<- str_trim(coords$wordID[loc2])
          c$el_char[j]<- coords$char[loc2]
          c$el_char_num[j]<- coords$char_num[loc2]
          c$el_word_num[j]<- coords$word_num[loc2]
          
          
          if(!is.na(coords$wordID[loc2])){
            if(c$el_wordID[j]== target){
              c$el_target_word[j]<- "Yes"
            }else{
              c$el_target_word[j]<- "No"
            }
          }
          
          
        }
        
        
        
      }
      
      dat<- rbind(dat, c)
      
      
    }
    
    
    
  }
  
  
  

  

  cat(k); cat(' ')
  
}

dat$...1<- NULL
#dat$timestamp<- NULL

# library(EMreading)
# dat<- Frequency(dat)

write.csv(dat, 'LAB-SPAIN/data/SPAIN_webcam_data.csv')


