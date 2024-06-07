

ppl= 67
x_offset= 163
y_offset= 370


sent= 'Mrs. Clark is a\nsocial person who\ngets along with\neverybody.'
lines= unlist(strsplit(sent, '\n'))

library(tidyverse)


options(scipen=999)
trials <- read.csv("~/R/WebEye/preproc/trials.csv")
dat <- read.csv("preproc/timeseries.csv")
dat2<- dat %>% pivot_wider(names_from = variable_name, values_from = value)
dat2$timestamp<- format(dat2$timestamp, scientific = F)

dat2$question_answer<- NULL
dat2$gaze_data<- NULL
dat2$el_x<- NULL
dat2$gaze_conf<- NULL

dat2$timestamp<- as.numeric(dat2$timestamp)

dat2<- dat2 %>%
  mutate(time_diff = timestamp - lag(timestamp, default = first(timestamp)))


dat2$t_since_last_sample<- dat2$timestamp[1:nrow(dat2)]
