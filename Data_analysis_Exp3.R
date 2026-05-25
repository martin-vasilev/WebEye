
rm(list= ls())

options(scipen=999)

library(readr)
library(tidyverse)
library(readxl)

freq <- read_excel("corpus/Corpus_fq.xlsx", sheet = 'Frequency')
freq<- freq[1:120,]
single_sent <- read_excel("corpus/Corpus_fq.xlsx", sheet = 'Single sentences')


# load data folders:
folders<- list.dirs('Prolific/prolific_raw_data/')
folders<- folders[2:length(folders)]

if(!file.exists('Prolific/data/eye_data_raw.csv')){
  
  eye_data<- NULL
  trial_data<- NULL
  
  for(i in 1:length(folders)){ # for each subject folder
    
    # open files:
    folder_dir<- folders[i]
    
    # get study list
    list<- substr(folder_dir, nchar(folder_dir), nchar(folder_dir))
    
    # trial info data:
    trials <- suppressMessages(read_csv(paste(folder_dir, '/trials.csv', sep='')))
    trials<- subset(trials, Task_Name== 'sentence'|Task_Name== 'sentence_DC' )
    
    # info:
    info<- suppressMessages(read_csv(paste(folder_dir, '/sessions.csv', sep='')))
    
    year<- info$Start_Time
    year<- as.numeric(substr(year, 1, 4))
    
    ## screen settings for subject:
    scr_width<- info$Screen_Width_In_Pixels
    scr_height<- info$Screen_Height_In_Pixels
    
    #scr_width/scr_height
    
    native_width<- 800
    native_height<- 450
    
    width_multiplier<- scr_width/native_width
    height_multiplier<- scr_height/native_height
    
    # timeseries (i.e., eye-tracking data):
    ts<- suppressWarnings(suppressMessages(read_csv(paste(folder_dir, '/timeseries.csv', sep=''))))
    ts<- subset(ts, Task_Name== 'sentence' |Task_Name== 'sentence_DC')
    
    ts$list<- list
    
    tasks<- unique(trials$Task_Name)
    
    
    for(j in 1:length(tasks)){ # for each task
      
      task_trials<- subset(trials, Task_Name== tasks[j])
      task_ts<- subset(ts, Task_Name== tasks[j])
      
      task_trials$question_accuracy<-NA
      
      if(tasks[j]== "sentence_DC" & year== 2024){
        cat(sprintf('Subject %g task %s skipped\n', 
                        info$Exp_Subject_Id, tasks[j]))
        next

      }
      
      for(k in 1:nrow(task_trials)){ # for each trial...
        
        start_time<- task_trials$trial_start[k]
        end_time<- task_trials$trial_end[k]
        
        if(is.na(start_time) | is.na(end_time)){
          stop('Missing trial times')
        }
        
        trial_ts<- subset(task_ts, Trial_Id== task_trials$Trial_Id[k] & variable_name== 'gaze_data') 
        
        
        if(list=='A' | list== 'C'){
          trial_ts$Frequency<- task_trials$Frequency[k]
          
        }else{ # frequency labels are reversed for list B (difficult to change on website)
          trial_ts$Frequency<- ifelse(task_trials$Frequency[k]=='low', 'high', ifelse(task_trials$Frequency[k]=='high', 'low', NA))
        }
        
        
        # calculate accuracy
        if(task_trials$Task_Name[k]== 'sentence'){ # frequency corpus
          loc<- which(freq$Study_ID== task_trials$Trial_Id[k])[1]
          item<- freq[loc, ]
          
          if(task_trials$question_answer[k]== item$Question_answer){
            task_trials$question_accuracy[k]<- 1
          }else{
            task_trials$question_accuracy[k]<- 0
          }
          
        }else{ # single line corpus:
          loc<- which(single_sent$Study_ID== task_trials$Trial_Id[k])
          item<- single_sent[loc, ]
          
          if(task_trials$question_answer[k]== item$Question_answ){
            task_trials$question_accuracy[k]<- 1
          }else{
            task_trials$question_accuracy[k]<- 0
          }
          
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
          mutate(time_diff = time - dplyr::lag(time))
        
        # save data:
        eye_data<- rbind(eye_data, trial_ts)
        
        
      } # end of k (each trial)
        
      # save trial data:
      task_trials<- task_trials[, c('Rec_Session_Id', 'Trial_Nr',
                    'Trial_Id', 'Task_Name', 'question_accuracy',
                    'trial_start', 'trial_end')]
      trial_data<- rbind(trial_data, task_trials)
      
      } # end of j (task)
      
    cat(sprintf('Subject %g \n', 
                i))
      
    } # end of i (subject)

  write.csv(eye_data, 'Prolific/data/eye_data_raw.csv')
  write.csv(trial_data, 'Prolific/data/trial_data.csv')
  } else{
  eye_data <- read.csv("Prolific/data/eye_data_raw.csv")
  trial_data<- read.csv("Prolific/data/trial_data.csv")
}


## average accuracy
library(tidyverse)

mean(trial_data$question_accuracy)*100
sd(trial_data$question_accuracy)*100

# accuracy per subject:
sub_means<- trial_data %>% 
  group_by(Rec_Session_Id)%>%
  summarise(Mean= mean(question_accuracy))

# range:
range(sub_means$Mean)*100

# trial times:
trial_data$trial_time<- trial_data$trial_end - trial_data$trial_start

sub_time<- trial_data %>% group_by(Rec_Session_Id)%>%
  summarise(M= mean(trial_time))

mean(sub_time$M)
sd(sub_time$M)


# remove samples within blinks:
n_full<- nrow(eye_data)

eye_data<- eye_data %>% filter(conf>0)

# samples removed due to blinks:
perc_blinks= (1- nrow(eye_data)/n_full)*100
perc_blinks

n_blinks<- nrow(eye_data)

# remove samples outside of screen bounds:
eye_data<- eye_data %>% 
  filter(x>0 & x<=1920 & y>0 & y<1080)

(1- nrow(eye_data)/n_full)*100 - perc_blinks


## calculate effective sampling rate:
hz= eye_data %>%
  filter(!is.na(time_diff))%>%
  mutate(valid_diffs= time_diff/1000)%>%
  mutate(hz= 1/valid_diffs)

# subject level sampling rate:
sub<- hz%>%
  group_by(Exp_Subject_Id)%>%
  summarise(M= mean(hz), 
            SD= sd(hz))

mean(sub$M)
sd(sub$M)
range(sub$M)



#### Parse samples into saccades:
library(saccades)

# Frequency corpus:
webfq<- eye_data #subset(eye_data, Task_Name== "sentence")

nsubs<- unique(webfq$Exp_Subject_Id)

parsed_web<- NULL


for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  s<- subset(webfq, Exp_Subject_Id== nsubs[i])
  ntasks<- unique(s$Task_Name)
  
  for (j in 1:length(ntasks)){
    
    t<- subset(s, Task_Name== ntasks[j])
    
    if(ntasks[j]=='sentence'){
      freq<- t %>%
        distinct(Trial_Id, .keep_all = TRUE)%>%
        select(Trial_Id, Frequency)
      
      colnames(freq)<- c('trial', 'Freq')
      
      t<- t[, c('x', 'y', 'Trial_Id', 'time')]
      colnames(t)<- c("x", "y", "trial", "time")
      ke.result<- detect.fixations(t, smooth.coordinates = T,
                                   smooth.saccades = F, lambda = 3)
      ke.result<- subset(ke.result, event=='fixation')
      ke.result$sub<- nsubs[i]
      ke.result$event<- NULL
      
      ke.result<- ke.result %>% inner_join(freq, by= 'trial')
      ke.result$corpus= "Frequency corpus"
      parsed_web<- rbind(parsed_web, ke.result)
      
    }else{
      
      t<- t[, c('x', 'y', 'Trial_Id', 'time')]
      colnames(t)<- c("x", "y", "trial", "time")
      ke.result<- detect.fixations(t, smooth.coordinates = T,
                                   smooth.saccades = F, lambda = 3)
      ke.result<- subset(ke.result, event=='fixation')
      ke.result$sub<- nsubs[i]
      ke.result$event<- NULL
      ke.result$Freq= NA
      ke.result$corpus= "Single-line corpus"
      parsed_web<- rbind(parsed_web, ke.result)
      
    }
    

  }

}

median(parsed_web$dur)

median(log(parsed_web$dur))
hist(log(parsed_web$dur))

## save raw fixations (before pre-processing) for visualisation:
write.csv(x = parsed_web, file = 'Prolific/data/preproc/EK_webcam.csv')



library(readr)
Corpus_fq <- read_csv("LAB/Corpus_fq.csv")
Corpus_fq<- Corpus_fq[1:120,]
Corpus_sent <- read_csv("corpus/Corpus_sent.csv")

source('preproc/functions/get_coords.R')

web_fix<- NULL
parsed_web$wordID<- NA
parsed_web$char<- NA
parsed_web$char_num<- NA
parsed_web$target_word<- NA
parsed_web$word_num<- NA
parsed_web$line<- NA
parsed_web$char_line<- NA

library(stringr)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(parsed_web, sub== nsubs[k])
  ntasks<- unique(a$corpus)
  
  for(l in 1:length(ntasks)){
    n<- subset(a, corpus== ntasks[l])
    nitems<- unique(n$trial) 
    
    for(i in 1:length(nitems)){ # for each item...
      
      c<- subset(n, trial== nitems[i])
      
      if(c$corpus[1]== "Frequency corpus"){
        
        freq<- ifelse(c$Freq[1]== 'low', 'LF', 'HF')
        
        sent<-Corpus_fq$line_breaks[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
        coords<- get_coords(sent, revert = F)
        
        target<- Corpus_fq$`Target (N)`[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
        
      }else{ # single line sentence corpus
        
        sent<-Corpus_sent$Sentence[which(Corpus_sent$Study_ID== nitems[i])]
        
        coords<- get_coords(sent, revert = F)
        
        # sentence is on a single line so y dimension is not important
        # we set the y limits of the text box to the screen size
        coords$y1<- 1
        coords$y2<- 450
        
      }
      
            
      for(j in 1:nrow(c)){ # for each fixation
        
        ### map webcam samples:
        
        loc<- which(coords$x1<= c$x[j] & coords$x2>= c$x[j] & coords$y1<= c$y[j] & coords$y2>= c$y[j])
        
        if(length(loc)>0){
          c$wordID[j]<- str_trim(coords$wordID[loc])
          c$char[j]<- coords$char[loc]
          c$char_num[j]<- coords$char_num[loc]
          c$word_num[j]<- coords$word_num[loc]
          c$line[j]<- coords$line[loc]
          c$char_line[j]<- coords$char_line[loc]
          
          if(!is.na(coords$wordID[loc])){
            if(c$wordID[j]== target){
              c$target_word[j]<- "Yes"
            }else{
              c$target_word[j]<- "No"
            }
          }
          
          
        }
        
      } # end of j (fixations)
      
      web_fix<- rbind(web_fix, c)
      
    } # end of i (items)
    
    
    
  } # end of task...
  
  
  #nitems<- unique(a$trial)
  
  cat(k); cat(' ')
  
} # end of k (subjects)



# Parse trial data to compute fixation duration metrics:
nsubs<- unique(web_fix$sub)

dat<- NULL

for(i in 1:length(nsubs)){ # for each subject
  a<- subset(web_fix, sub== nsubs[i])
  
  ntasks<- unique(a$corpus)
  
  for(l in 1:length(ntasks)){ #for each task
    c<- subset(a, corpus== ntasks[l])
    nitems<- unique(c$trial)
    
    for(j in 1:length(nitems)){ # for each item in task
      
      b<- subset(c, trial== nitems[j])
      
      b$regress<- NA
      
      max_word<- 1
      
      max_fixated<- max(b$word_num, na.rm=T)
      
      if(max_fixated<0){
        dat<- rbind(dat, b)
        next
      }
      
      terminated<- rep(0, max_fixated)
      
      for(k in 1:nrow(b)){
        
        if(!is.na(b$word_num[k])){
          if(b$word_num[k]>= max_word & terminated[b$word_num[k]]==0){
            b$regress[k]<- 0
          }else{
            b$regress[k]<- 1
          }
          # 
          # # check for cases where readers return to word before progressing:
          # if(b$word_num[k]== max_word){
          #  which(b[1:k,]$regress) 
          # }
          
          
          if(b$word_num[k]> max_word){
            max_word<- b$word_num[k]
            terminated[1:(b$word_num[k]-1)]<- 1
          }
          
          if(b$word_num[k]< max_word){
            terminated[max_word]<- 1
          }
          
        }
        
        
      }
      
      dat<- rbind(dat, b)
      
    } # end of item (j)
    
    
  } # end of task (l)
  
  
}  #end of subject (i)



### merge adjacent fixations under 80 ms that are next to each other (1 char)
dat_new<- NULL

nsubs<- unique(dat$sub)

for(i in 1:length(nsubs)){
  a<- subset(dat, sub== nsubs[i])
  ntasks<- unique(a$corpus)
  
  for(l in 1:length(ntasks)){
    c<- subset(a, corpus== ntasks[l])
    nitems<- unique(c$trial)
    
    
    for(j in 1:length(nitems)){
      b<- subset(c, trial==nitems[j])
      b$remove<- NA
      
      for(k in 1:nrow(b)){
        
        if(b$dur[k]<80){
          
          if(!is.na(b$char_line[k])){
            
            # check if within 1 character of prev fixation
            if(k>1){
              if(!is.na(b$char_line[k-1])){
                
                if(abs(b$char_line[k]-b$char_line[k-1])<=1 & b$line[k]== b$line[k-1]){
                  b$remove[k]<- T
                  
                  # merge fixation with previous one:
                  cat(sprintf("Subject %g item %g: merging fixation %g (%g ms) with fixation %g (%g ms). New fixation %g is %g ms.\n\n",
                              nsubs[i], nitems[j], k, b$dur[k], k-1, b$dur[k-1], k-1, b$dur[k]+b$dur[k-1]))
                  
                  b$dur[k-1]<- b$dur[k-1]+ b$dur[k] 
                }
                
              }
              
              # check if within 1 character of next fixation
              
              if(k<nrow(b) & is.na(b$remove[k])){
                
                if(!is.na(b$char_line[k+1])){
                  
                  if(abs(b$char_line[k+1]-b$char_line[k])<=1 & b$line[k+1]== b$line[k]){
                    
                    b$remove[k]<- T
                    
                    # merge fixation with the next one:
                    cat(sprintf("Subject %g item %g: merging fixation %g (%g ms) with fixation %g (%g ms). New fixation %g is %g ms.\n\n",
                                nsubs[i], nitems[j], k, b$dur[k], k+1, b$dur[k+1], k+1, b$dur[k]+b$dur[k+1]))
                    b$dur[k+1]<- b$dur[k+1]+ b$dur[k] 
                  }
                  
                  
                }
                
              }
              
            }
            
          }
          
          
        }
      }
      dat_new<- rbind(dat_new, b)
      
    } # end of item loop
    
    
  }
  


}

table(dat_new$remove)
dat<- dat_new %>% filter(is.na(remove))

# percentage of < 80 ms fixations merged
(1-nrow(dat)/ nrow(dat_new))*100


out_L80<- which(dat$dur<80)

# percentage of < 80ms fixations left unmerged:
(length(out_L80)/nrow(dat_new))*100

dat<- dat[-out_L80,]


out<- which(dat$dur> 1000)

# percentage of >1000 ms fixations discarded
length(out)/nrow(dat_new)*100
dat<- dat[-out,]



words_web<- NULL
nsubs<- unique(dat$sub)

for(i in 1:length(nsubs)){
  
  a<- subset(dat, sub== nsubs[i])
  ntasks<- unique(a$corpus)
  
  for(l in 1:length(ntasks)){
    
    b<- subset(a, corpus== ntasks[l])
    nitems<- unique(b$trial)
    
    for(j in 1:length(nitems)){
      
      c<- subset(b, trial== nitems[j])
      
      nwords<- sort(unique(c$word_num))
      
      if(length(nwords)==0){
        next
      }
      
      # get fixated words
      fixated_words<- unique(c$word_num)
      fixated_words<- fixated_words[which(!is.na(fixated_words))]
      
      ## find non-fixated words in the sentence:
      freq<- ifelse(c$Freq[1]== 'low', 'LF', 'HF')
      
      if(ntasks[l]== "Frequency corpus"){
        sent<- Corpus_fq$line_breaks[which(Corpus_fq$Study_ID== c$trial[1] & Corpus_fq$`Frequency type`== freq)[1]]
      }else{
        sent<- Corpus_sent$Sentence[which(Corpus_sent$Study_ID== c$trial[1])[1]]
      }
      
      words <- unlist(strsplit(gsub("@", " ", sent), "\\s+"))
      words[fixated_words]<- NA
        
        if(length(which(!is.na(words)))==0){
          # all words were fixated, skip non-fixated loop:
          words<- NULL
        }else{
          
          # set-up data frame for non-fixated words:
          words<- as.data.frame(words)
          colnames(words)<- 'wordID'
          words$word_num<- 1:nrow(words)
          words$sub<- c$sub[1]
          words$item<- c$trial[1]
          words$Freq<- c$Freq[1]
          words<- words[,c(3, 4, 5, 2, 1)]
          words<- words%>% drop_na(wordID) # remove fixated words
          
          # find target word and add to data frame:
          target<- Corpus_fq$`Target (N)`[which(Corpus_fq$Study_ID== c$trial[1] & Corpus_fq$`Frequency type`== freq)[1]]
          
          is_target<- which(words$wordID==target)
          words$target<- "No"
          
          if(length(is_target)>0){
            words$target[is_target]<- 'Yes'
          }
          
          # Add dependent measures to data frame:
          words$FFD<- NA
          words$SFD<- NA
          words$GD<- NA
          words$TVT<- NA
          words$nfixAll<- 0
          words$nfix1<- 0
          words$nfix2<- 0
          
          # corpus:
          words$corpus<-  ntasks[l]
          
        }
      
      for(k in 1:length(nwords)){
        
        d<- subset(c, word_num== nwords[k])
        
        TVT<- NA
        FFD<- NA
        SFD<- NA
        GD<- NA
        
        p1<- subset(d, regress==0)
        p2<- subset(d, regress==1)
        
        TVT<- sum(d$dur)
        
        if(nrow(p1)>0){
          GD<- sum(p1$dur)
          FFD<- p1$dur[1]
          
          if(nrow(p1)==1){
            SFD<- FFD
          }
          
        }
        
        t<- data.frame('sub'= c$sub[1], 'item'= c$trial[1], 'Freq'= c$Freq[1],
                       'word_num'= nwords[k], 'wordID'= d$wordID[1],
                       'target'= d$target_word[1], 'FFD'= FFD,
                       'SFD'= SFD, 'GD'= GD, 'TVT'= TVT,
                       nfixAll= nrow(d), nfix1= nrow(p1),
                       nfix2= nrow(p2),'corpus'= d$corpus[1])
        
        words<- rbind(words, t)
        #words_web<- rbind(words_web, t)
        
      } # end of word
      
      words <- words[order(words$word_num), ]
      words$skip_first_pass<- ifelse(words$nfix1==0, 1, 0)
      words$skip_total<- ifelse(words$nfixAll==0, 1, 0)
      
      if(ntasks[l]=="Single-line corpus"){
        # reset target word column for single line sentences:
        words$target<- NA
      }
      
      # save data for this trial:
      words_web<- rbind(words_web, words)
      
    } # end of item
    
    
  }# end of task
  
  
}

words_web_t<- words_web%>% filter(target== 'Yes' & corpus== "Frequency corpus")

outall<- which(words_web_t$FFD>1000|words_web_t$SFD>1000 | words_web_t$GD>2000 | words_web_t$TVT> 3000)

# percentange of words removed as outliers in reading measures:
length(outall)/nrow(words_web_t)*100

words_web_t<- words_web_t[-outall,]

# save dataset:
write.csv(words_web_t, 'Prolific/data/target_word_frequency_dat.csv')


## single line corpus data:
words_web_c<- words_web#%>% filter(target== 'No')

outall<- which(words_web_c$FFD>1000|words_web_c$SFD>1000 | words_web_c$GD>2000 | words_web_c$TVT> 3000)

# percentange of words removed as outliers in reading measures:
length(outall)/nrow(words_web_c)*100

if(length(outall)>0){
  words_web_c<- words_web_c[-outall,]
}

library(EMreading)
words_web_c<- Frequency(words_web_c)
words_web_c$word_length<- nchar(words_web_c$wordID)

# increment single-line corpus items, so they don't overlap:
words_web_c$item[which(words_web_c$corpus== 'Single-line corpus')]<- 100+words_web_c$item[which(words_web_c$corpus== 'Single-line corpus')]



# save dataset:
write.csv(words_web_c, 'Prolific/data/corpus_fixation_data.csv')




words_web_t %>%  group_by(Freq)%>%
  summarise(FFD= mean(FFD, na.rm=T),
            SFD= mean(SFD, na.rm=T),
            GD= mean(GD, na.rm=T),
            TVT= mean(TVT, na.rm=T))

words_web_t %>%
  group_by(Freq) %>%
  summarise(FFD_sd = sd(FFD, na.rm = TRUE),
            SFD_sd = sd(SFD, na.rm = TRUE),
            GD_sd = sd(GD, na.rm = TRUE),
            TVT_sd = sd(TVT, na.rm = TRUE))



# fit lmer models:
words_web_t$Freq<- as.factor(words_web_t$Freq)
contrasts(words_web_t$Freq)<- c(-1, 1)

library(lmerTest)


summary(M1<- lmer(log(FFD)~ Freq +(Freq|sub)+(1|item), data= words_web_t))

summary(M2<- lmer(log(SFD)~ Freq +(Freq|sub)+(1|item), data= words_web_t))

summary(M3<- lmer(log(GD)~ Freq +(Freq|sub)+(1|item), data= words_web_t))

summary(M4<- lmer(log(TVT)~ Freq +(Freq|sub)+(1|item), data= words_web_t))



### plot frequency by sampling rate:

colnames(sub)<- c("sub", "Hz", "SD")
  
fq<- read.csv('Prolific/data/target_word_frequency_dat.csv')

sub_means<- fq %>%
  group_by(sub, Freq)%>%
  summarise(M= mean(GD, na.rm=T))%>%
  pivot_wider(names_from = Freq, values_from = M)%>%
  mutate(ES= low - high)%>%
  inner_join(sub, by = 'sub')

sub_means %>%
  ggplot(aes(x= Hz, y=ES))+
  geom_point()+
  geom_smooth(method= 'lm')


# check visual angle:
study_dat <- read.csv("~/R/WebEye/Prolific/data/Study_info_data.csv")

study_dat<- study_dat %>% select(letter_deg,Exp_Subject_Id )
colnames(study_dat)<- c('deg','sub')  

sub_means<- inner_join(sub_means, study_dat, by = 'sub')  

sub_means %>%
  filter(SD<40)%>%
  ggplot(aes(x= Hz, y=SD))+
  geom_point()+
  geom_smooth(method= 'lm')
