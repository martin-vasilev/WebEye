
rm(list= ls())

options(scipen=999)

library(readr)
dat <- read_csv("preproc/test/timeseries.csv")
trials <- read_csv("preproc/test/trials.csv")

dat<- subset(dat, variable_name== 'gaze_data')
dat$x<- NA
dat$y<- NA
dat$t<- NA
dat$ci<- NA

for(i in 1:nrow(dat)){
  dat$x[i]<- as.numeric(unlist(strsplit(dat$value[i], ','))[1])
  dat$y[i]<- as.numeric(unlist(strsplit(dat$value[i], ','))[2])
  dat$t[i]<- as.numeric(unlist(strsplit(dat$value[i], ','))[3])
  dat$ci[i]<- as.numeric(unlist(strsplit(dat$value[i], ','))[4])
}

s<- subset(dat, Trial_Nr==1)

# standardise to screen size:
s$x<- as.numeric(s$x)*2.4
s$y<- as.numeric(s$y)*2.4


s<- s %>%
  mutate(time_diff = t - lag(t, default = first(t)))

start_time<- trials$trial_start[2]
end_time<- trials$trial_end[2]

s<- subset(s, t>= start_time & t<= end_time)

duration<- end_time- start_time
duration/ nrow(s)
