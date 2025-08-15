
rm(list= ls())

library(readxl)
Corpus <- read_excel("corpus/Corpus_fq.xlsx")
Corpus<- Corpus[1:120,]
Corpus$Frequency<- as.numeric(Corpus$Frequency)
Corpus$Length<- as.numeric(Corpus$Length)
Corpus$Predictability<- as.numeric(Corpus$Predictability)

library(tidyverse)

# frequency
Corpus %>% group_by(`Frequency type`) %>% 
  summarise(M_freq= mean(`Zipf frequency`),
            SD_freq= sd(`Zipf frequency`),
            min= min(`Zipf frequency`),
            max= max(`Zipf frequency`))

t.test(Corpus$`Zipf frequency`[which(Corpus$`Frequency type`=='HF')], 
       Corpus$`Zipf frequency`[which(Corpus$`Frequency type`=='LF')])

# length:
Corpus %>% group_by(`Frequency type`) %>% 
  summarise(M= mean(Length),
            SD= sd(Length),
            min= min(Length),
            max= max(Length))

t.test(Corpus$Length[which(Corpus$`Frequency type`=='HF')], 
       Corpus$Length[which(Corpus$`Frequency type`=='LF')])

# predictability:
Corpus %>% group_by(`Frequency type`) %>% 
  summarise(M= mean(Predictability),
            SD= sd(Predictability),
            min= min(Predictability),
            max= max(Predictability))

t.test(Corpus$Predictability[which(Corpus$`Frequency type`=='HF')], 
       Corpus$Predictability[which(Corpus$`Frequency type`=='LF')])


## number of words:
mean(as.numeric(Corpus$`N words`))
sd(as.numeric(Corpus$`N words`))

mean(as.numeric(Corpus$`Length (char)`))

# target word position:
mean(Corpus$`Target position`)
sd(Corpus$`Target position`)


### Single-line corpus:
Corpus <- read_excel("corpus/Corpus_fq.xlsx", sheet = 'Single sentences')



Corpus <- read_excel("corpus/Corpus_fq.xlsx")

# number of words:
mean(Corpus$`number of words`)
sd(Corpus$`number of words`)

# number of characters:
Corpus$nchar<- nchar(Corpus$Sentence)

mean(Corpus$nchar)
sd(Corpus$nchar)


