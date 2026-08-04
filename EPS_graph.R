
rm(list= ls())

library(tidyverse)

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"


# load data:
Exp1 <- read.csv("LAB/data/target_word_fixation_data.csv")
Exp2 <- read.csv("LAB-SPAIN/data/target_word_fixation_data.csv")
Exp3<- read.csv("Prolific/data/target_word_frequency_dat.csv")

#Exp1<- Exp1 %>% filter(Tracker== "Webcam")
#Exp1$Tracker<- NULL

#Exp2<- Exp2 %>% filter(Tracker== "Webcam")
#Exp2$Tracker<- NULL

Exp3$corpus<- NULL

Exp1$Experiment<- "Experiment 1"
Exp2$Experiment<- "Experiment 2"
Exp3$Experiment<- "Experiment 3"
Exp3$Tracker<- "Webcam"


# Exp1 %>% filter(Tracker=="Webcam")%>% group_by(Freq)%>%
#   summarise(M_FFD= mean(FFD, na.rm=T), M_SFD= mean(SFD, na.rm=T))

dat<- rbind(Exp1, Exp2, Exp3)

dat_long<- dat %>% pivot_longer(cols = FFD:TVT,
                                names_to = 'measure',
                                values_to = 'duration') 


## Exp 1:

means<- dat_long %>%
  filter(Experiment== "Experiment 1")%>%
  #  select(-Freq)%>% 
  #  drop_na() %>% 
  group_by(Tracker, Freq, measure)%>% 
  summarise(Mean= mean(duration, na.rm=T))

fun_mean <- function(x, rounding= 0){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), rounding), sep= '')))}


dat_long$measure <- factor(
  dat_long$measure,
  levels = c("FFD", "SFD", "GD", "TVT")
)

pos_halfeye <- position_nudge(x = 0.15)   # further left
pos_box      <- position_nudge(x = -0.1)   # centered (or small nudge)


P1 <- ggplot(dat_long%>% filter(Experiment== "Experiment 3"),
             aes(x = Freq, y = duration,
                 color = Freq, fill = Freq)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    point_colour = NA,
    position = pos_halfeye   
  ) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA,
    fill = NA,
    position = pos_box   
  ) +
  facet_wrap(~ Tracker + measure, scales = "free", ncol=4) +
  coord_cartesian(xlim = c(1.2, NA), clip = "off") +
  scale_color_manual(values = pallete1[1:2]) +
  scale_fill_manual(values = pallete1[1:2]) +
  theme_classic(26) +
  ylab("Fixation durations (in ms)") +
  xlab("Target word lexical frequency") +
  theme(
    legend.position = 'none',
    strip.background = element_rect(colour = NA, fill = NA),
    strip.text = element_text(face = "bold")
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    colour = "black",
    size = 4,
    position = pos_box     
  ) +
  stat_summary(
    fun.data = fun_mean,
    geom = "text",
    vjust = -2.25,
    hjust = 0.53,              
    colour = "black",
    size = 6,
    fontface = "bold",
    position = pos_box     
  )

# ggsave(plot = P1, filename = 'LAB/Plots/webcam_effect.pdf', width = 16,
#        height = 16, units = 'in')

ggsave(plot = P1, filename = 'LAB/Plots/EPS_exp3.png',
       width = 18,
       height = 10, units = 'in')

