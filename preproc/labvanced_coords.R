

ppl= 67
x_offset= 163
y_offset= 370


sent= 'Mrs. Clark is a\nsocial person who\ngets along with\neverybody.'
lines= unlist(strsplit(sent, '\n'))

library(tidyverse)


options(scipen=999)
trials <- read.csv("~/R/WebEye/preproc/trials.csv")
dat <- read.csv("preproc/timeseries.csv")
dat<- subset(dat, variable_name== 'gaze_data')

#dat2<- dat %>% pivot_wider(names_from = variable_name, values_from = value)
#dat2$timestamp<- format(dat2$timestamp, scientific = F)

dat2<- dat
dat2$t<- format(dat2$t, scientific = F)

dat2$question_answer<- NULL
dat2$gaze_data<- NULL
dat2$el_x<- NULL
dat2$gaze_conf<- NULL

dat2$timestamp<- as.numeric(dat2$timestamp)
dat2$gaze_x<- as.numeric(dat2$gaze_x)
dat2$gaze_y<- as.numeric(dat2$gaze_y)

# standardise to screen size:
dat2$gaze_x<- dat2$gaze_x*2.4
dat2$gaze_y<- dat2$gaze_y*2.4

dat2<- arrange(dat2, timestamp)

dat2<- dat2 %>%
  mutate(time_diff = timestamp - lag(timestamp, default = first(timestamp)))


### Extract Eyelink data:
get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}

dataF<- readLines('preproc/100_2024_06_06_17_43.asc') # load asc file;

start_loc<- which(grepl('UNIX', dataF))
start_stamp<- dataF[start_loc]
start<- as.numeric(unlist(strsplit(start_stamp, ' '))[3])
el_start_time<- get_num(unlist(strsplit(start_stamp, ' '))[1])

# extract fixations:
sacc_samples<- dataF[start_loc+1:length(dataF)]

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


### merge webcam and eyelink data:
el<- dat2

for(i in 1:nrow(el)){
  a<- which(sacc_samples$unix_time==el$timestamp[i])
  if(length(a)>0){
    el$gaze_x[i]<- sacc_samples$V2[a]
    el$gaze_y[i]<- sacc_samples$V3[a]
  }else{
    el$gaze_x[i]<- NA
    el$gaze_y[i]<- NA
  }

}

el$tracker= 'Eyelink'
dat2$tracker= "Webcam"

df<- rbind(dat2, el)

s<- subset(df, Trial_Nr==3)

library(ggpubr)
library(jpeg)

img <- readJPEG("preproc/Trial5.jpg")

P1= ggplot(s, aes(x=gaze_x, y= gaze_y, color= tracker))+
  scale_y_reverse()+
  background_image(img)+
  geom_point()+
  theme_classic(22)+
  ylim(1080,1 )+ xlim(1, 1920)+
  labs(x= 'Gaze pos. x', y='Gaze pos. y')
ggsave(filename = 'preproc/Trial_5plot.jpg', plot = P1, width = 10, height = 6.5)

cor.test(el$gaze_x, dat2$gaze_x, na.rm=T)
cor.test(el$gaze_y, dat2$gaze_y, na.rm=T)


df2 <- df%>% pivot_wider(names_from = tracker, values_from = c(gaze_x, gaze_y))

P2= ggplot(df2, aes(x=gaze_x_Eyelink, y= gaze_x_Webcam))+
  geom_point()+
  theme_minimal(16)+
  facet_wrap(~Trial_Nr, nrow = 4)+
  labs(x= 'Eyelink gaze x', y='Webcam gaze x')

ggsave(filename = 'preproc/gaze_x_corr.jpg', plot = P2, width = 10, height = 10)


P3= ggplot(df2, aes(x=gaze_y_Eyelink, y= gaze_y_Webcam))+
  geom_point()+
  theme_minimal(16)+
  facet_wrap(~Trial_Nr, nrow = 4)+
  labs(x= 'Eyelink gaze y', y='Webcam gaze y')

ggsave(filename = 'preproc/gaze_y_corr.jpg', plot = P3, width = 10, height = 10)
