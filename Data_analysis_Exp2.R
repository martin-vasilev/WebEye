
# Martin R. Vasilev, 2025

rm(list= ls())


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

library(tidyverse)
library(readr)
trial <- read_csv("LAB-SPAIN/data/SPAIN_trial_data.csv")


# Comprehension accuracy --------------------------------------------------

# calculate correct answer:

# get corpora with correct answer:

library(readr)
listA <- read_csv("LAB-SPAIN/sentences_fullA.csv")
listB <- read_csv("LAB-SPAIN/sentences_fullB_final.csv")
listB$index<- listB$index-100 

trial$question_accuracy<- NA

for(i in 1:nrow(trial)){
  
  if(trial$current_answer[i]>=0){
    
    if(trial$list[i]=="A"){
      
      answer<- listA$answerA[which(listA$index== trial$Trial_Id[i])]
      
    }else{
      answer<- listB$answerB[which(listB$index== trial$Trial_Id[i])]
    }
    
    if(trial$current_answer[i]==1 &answer== "SI"){
      trial$question_accuracy[i]<- 1
    }else{
      if(trial$current_answer[i]==0 &answer== "NO"){
        trial$question_accuracy[i]<- 1
      }else{
        trial$question_accuracy[i]<- 0
      }
    }
    
    
    
    
  }
  
  
  
  
}


# mean:
mean(trial$question_accuracy, na.rm=T)*100

# sd:
sd(trial$question_accuracy, na.rm=T)*100


## accuracy by participant:

sub_acc<- trial %>% 
  group_by(Rec_Session_Id) %>%
  summarise(M= mean(question_accuracy, na.rm=T))

range(sub_acc$M)*100

## trial reading times:
trial$duration<- trial$trial_end - trial$trial_start

trial %>% 
 # group_by(Task_Name) %>%
  summarise(M= mean(duration, na.rm= T), SD= sd(duration, na.rm = T))


# Sample correlations -----------------------------------------------------

# load in webcam data:
webcam <- read_csv("LAB-SPAIN/data/SPAIN_webcam_data.csv")
table(webcam$Task_Name)


# remove samples within blinks:
n_full<- nrow(webcam)

webcam<- webcam %>% filter(conf>0 & el_pupil>0)

# samples removed due to blinks:
perc_blinks= (1- nrow(webcam)/n_full)*100
perc_blinks

n_blinks<- nrow(webcam)

# remove samples outside of screen bounds:
webcam<- webcam %>% 
  filter(el_x>0 & el_x<=1920 & el_y>0 & el_y<=1080 & x>0 & x<=1920 & y>0 & y<1080)

(1- nrow(webcam)/n_full)*100 - perc_blinks

## calculate effective sampling rate:

hz= webcam %>%
  filter(!is.na(time_diff))%>%
  mutate(valid_diffs= time_diff/1000)%>%
  mutate(hz= 1/valid_diffs)

# subject level sampling rate:
sub<- hz%>%
  group_by(subject)%>%
  summarise(M= mean(hz), 
            SD= sd(hz))

mean(sub$M)
sd(sub$M)
range(sub$M)

sub_correlations<- webcam %>%
#  select(-Freq)%>% 
#  drop_na() %>% 
  group_by(Task_Name, subject)%>% 
  summarise(Corr_x= cor(x,el_x), Corr_y= cor(y,el_y))

sub_correlations<- sub_correlations %>%
  pivot_longer(cols = 3:4, names_to = 'Dimension', values_to = 'corr' )

fun_mean <- function(x, rounding= 2){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), rounding), sep= '')))}

P1<- ggplot(sub_correlations, aes(x = Dimension, y = corr, 
                                  color= Dimension, fill= Dimension)) + 
#  facet_wrap(~Task_Name)+
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.7,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  theme_classic(20) +ylab("Pearson's (r) coefficient")+
  xlab("Screen dimension")+
  #labs(color= c('X', 'Y'))+
  scale_x_discrete(labels=c('X', 'Y'))+
  ylim(-.5, 1)+
  # geom_text(data = ann_text,label = "No sound", 
  #           colour= '#545252', size= 5.5, hjust = +1.25)+
  # geom_text(data = ann_text2,label = "No sound", 
  #           colour= '#545252', size= 5.5, hjust = +1.25)+
  theme(legend.position = 'none',
        strip.background = element_rect(colour=NA, fill=NA),
        strip.text = element_text(face="bold"))+
  stat_summary(fun = mean, geom="point",colour="black", size=4) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.15,
               hjust= 0.75, colour="black", size= 7);P1

ggsave(filename = 'LAB-SPAIN/Plots/correlations.pdf', width = 10,
       height = 8, units = 'in')

save(P1, file =  'LAB-SPAIN/Plots/correlations.Rda')

# Bland-Altman plot -------------------------------------------------------

# Compute sample-to-sample absolute deviations
webcam$dy_webcam <- abs(diff(c(NA, webcam$y)))
webcam$dy_eyelink <- abs(diff(c(NA, webcam$el_y)))

webcam$dx_webcam <- abs(diff(c(NA, webcam$x)))
webcam$dx_eyelink <- abs(diff(c(NA, webcam$el_x)))


# webcam %>% group_by(Task_Name)%>%
#   summarise(My_webcam= sd(dy_webcam, na.rm= T),
#             My_el= sd(dy_eyelink,na.rm=T),
#             Mx_webcam= sd(dx_webcam, na.rm= T),
#             Mx_el= sd(dx_eyelink,na.rm=T))
# 

library(ggplot2)
library(dplyr)

# Single-sentence, y dimension
ba <- webcam %>% 
 # filter(Task_Name=='Single_line_sentences') %>%
  mutate(means_y= (y + el_y) / 2,
         diffs_y= y- el_y,
         means_x= (x + el_x) / 2,
         diffs_x= x- el_x)


library(ggplot2)
library(ggpointdensity)

loa_data <- ba %>%
#  group_by(Task_Name) %>%
  summarise(
    mean_diff = mean(diffs_y, na.rm = TRUE),
    upper = mean_diff + 1.96 * sd(diffs_y, na.rm = TRUE),
    lower = mean_diff - 1.96 * sd(diffs_y, na.rm = TRUE)
  )

# lm1<- lm(diffs_y ~ means_y, data = subset(ba, Task_Name== 'Single-line corpus'))
# coef(lm1)
# 
# lm2<- lm(diffs_y ~ means_y, data = subset(ba, Task_Name!= 'Single-line corpus'))
# coef(lm2)
# 
# loa_data$intercept<- c(coef(lm2)[1], coef(lm1)[1])
# loa_data$slope<- c(coef(lm2)[2], coef(lm1)[2])

get_loa_sloped <- function(data, mean_col, diff_col) {
  data %>%
 #   group_by(Task_Name) %>%
    summarise(
      # Fit linear model per task
      mod = list(lm(reformulate(mean_col, diff_col), data = cur_data())),
      .groups = "drop"
    ) %>%
    mutate(
      intercept = map_dbl(mod, ~ coef(.x)[1]),
      slope     = map_dbl(mod, ~ coef(.x)[2]),
      sd_resid  = map_dbl(mod, ~ sd(resid(.x), na.rm = TRUE)),
      upper_int = intercept + 1.96 * sd_resid,
      lower_int = intercept - 1.96 * sd_resid
    ) %>%
    select(-mod)
}

loa_y <- get_loa_sloped(ba, "means_y", "diffs_y")

Plot_y<- ggplot(ba, aes(x = means_y, y = diffs_y)) +
  geom_pointdensity(aes(color = after_stat(density))) +
  scale_color_viridis_c() +
  ylim(-1550, 700)+
  #scale_y_continuous(breaks=c(-1500, -1000, -500, 0, 500)) +
  xlim(0, 1400)+
  #scale_x_continuous(breaks=c(0, 500, 1000)) +
  # Mean bias line (purple, dashed)
  
  geom_abline(data = loa_y, aes(intercept = intercept, slope = slope),
              color = "purple", linetype = "dashed", linewidth = 1.3) +
  
  # Upper LoA (red, dotted)
  geom_abline(data = loa_y, aes(intercept = upper_int, slope = slope),
              color = "red", linetype = "dotted", linewidth = 1.3) +
  
  # Lower LoA (red, dotted)
  geom_abline(data = loa_y, aes(intercept = lower_int, slope = slope),
              color = "red", linetype = "dotted", linewidth = 1.3) +
#   # Mean line per facet
#   geom_hline(data = loa_data, aes(yintercept = mean_diff), 
#              color = "purple", linetype = "dashed", linewidth=1.3) +
#   
#   # Upper & lower 95% limits per facet
#   geom_hline(data = loa_data, aes(yintercept = upper), 
#              color = "red", linetype = "dotted", linewidth=1.3) +
#   geom_hline(data = loa_data, aes(yintercept = lower), 
#              color = "red", linetype = "dotted", linewidth=1.3) +
# #  geom_smooth(method= 'lm', )+
 # facet_wrap(vars(Task_Name)) +
  labs(title = "Y position",
       x = "Mean Y (Webcam & EyeLink)",
       y = "Difference in Y (Webcam - EyeLink)",
       color = "Point Density") +
  theme_classic(base_size = 20)+
  theme(
    strip.background = element_blank(),  # removes box background
    strip.text = element_text(face = "bold", size = 14),  # optional: style text
    plot.title = element_text(hjust = 0.5))

ggsave(filename = 'LAB-SPAIN/Plots/BA_sl.png', plot = Plot_y, width = 10, height = 7, units = 'in')



loa_data2 <- ba %>%
#  group_by(Task_Name) %>%
  summarise(
    mean_diff = mean(diffs_x, na.rm = TRUE),
    upper = mean_diff + 1.96 * sd(diffs_x, na.rm = TRUE),
    lower = mean_diff - 1.96 * sd(diffs_x, na.rm = TRUE)
  )

loa_x <- get_loa_sloped(ba, "means_x", "diffs_x")

Plot_x<- ggplot(ba, aes(x = means_x, y = diffs_x)) +
  geom_pointdensity(aes(color = after_stat(density))) +
  scale_color_viridis_c() +
  ylim(-1550, 700)+
  #scale_y_continuous(breaks=c(-1500, -1000, -500, 0, 500)) +
  xlim(0, 1400)+
  #scale_x_continuous(breaks=c(0, 500, 1000)) +
  
  # Mean bias line
  geom_abline(data = loa_x, aes(intercept = intercept, slope = slope),
              color = "purple", linetype = "dashed", linewidth = 1.3) +
  
  # Upper LoA
  geom_abline(data = loa_x, aes(intercept = upper_int, slope = slope),
              color = "red", linetype = "dotted", linewidth = 1.3) +
  
  # Lower LoA
  geom_abline(data = loa_x, aes(intercept = lower_int, slope = slope),
              color = "red", linetype = "dotted", linewidth = 1.3) +
  # # Mean line per facet
  # geom_hline(data = loa_data2, aes(yintercept = mean_diff), 
  #            color = "purple", linetype = "dashed", linewidth=1.3) +
  # 
  # # Upper & lower 95% limits per facet
  # geom_hline(data = loa_data2, aes(yintercept = upper), 
  #            color = "red", linetype = "dotted", linewidth=1.3) +
  # geom_hline(data = loa_data2, aes(yintercept = lower), 
  #            color = "red", linetype = "dotted", linewidth=1.3) +
  
  #facet_wrap(vars(Task_Name)) +
  labs(title = "X position",
       x = "Mean X (Webcam & EyeLink)",
       y = "Difference in X (Webcam - EyeLink)",
       color = "Point Density") +
  theme_classic(base_size = 20)+
  theme(
    strip.background = element_blank(),  # removes box background
    strip.text = element_text(face = "bold", size = 14),  # optional: style text
    plot.title = element_text(hjust = 0.5))

ggsave(filename = 'LAB-SPAIN/Plots/BA_sl2.png', plot = Plot_x, width = 10, height = 7, units = 'in')


# combine the two plots:
library(ggpubr)
figure <- ggarrange(Plot_x, Plot_y,
                    ncol = 1, nrow = 2)

figure_e3<- figure#+ ggtitle("Experiment 3")+theme(plot.title = element_text(hjust = 0.5))
save(figure_e3, file= 'LAB-SPAIN/Plots/BA_combined.Rda')


ggsave(filename = 'LAB-SPAIN/Plots/BA_combined.png', plot = figure,
       width = 10, height = 12, units = 'in')

ggsave(filename = 'LAB-SPAIN/Plots/BA_combined.pdf', plot = figure,
       width = 10, height = 12, units = 'in', device = cairo_pdf)



# Subject-level accuracy --------------------------------------------------

webcam$diff_x<- (webcam$x-webcam$el_x)* 0.0162

webcam$diff_y<- (webcam$y- webcam$el_y)*0.0167

sub= webcam %>% group_by(subject)%>%
  summarise(`X position`= mean(diff_x, na.rm = T),
            `Y position`= mean(diff_y, na.rm = T))

webcam %>% 
  summarise(`X position_M`= mean(diff_x, na.rm = T),
            `X position_SD`= sd(diff_x, na.rm = T),
            `Y position_M`= mean(diff_y, na.rm = T),
            `Y position_SD`= sd(diff_y, na.rm = T))



sub= sub %>% 
  pivot_longer(cols = 2:3, names_to = 'Dimension', values_to = 'error')


P_density_E3=sub %>%
  ggplot(aes(x = error, fill = Dimension)) +
  geom_density(alpha = 0.4) +
  geom_rug(aes(color = Dimension), sides = "b", alpha = 0.6) +
  facet_wrap(~Dimension)+
  theme_minimal(18)+
  xlim(-10, 10)+
  theme(
    panel.spacing = unit(2, "lines") # increase spacing between facets
  )+ geom_vline(xintercept = 0,linetype = 2)+
  labs(x= 'Error (Webcam - Eyelink) in deg', y= "Density",
       title = "b) Experiment 2")+
      # title = "c) Mean error per subject and screen dimension (°)- Experiment 3")+
  theme(panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))

save(P_density_E3, file= 'LAB-SPAIN/Plots/Subject_acc_density.Rda')

ggsave(filename = 'LAB-SPAIN/Plots/Subject_acc_density.pdf',
       plot = P_density_E3, width = 10, height = 7, units = 'in')
ggsave(filename = 'LAB-SPAIN/Plots/Subject_acc_density.png',
       plot = P_density_E3, width = 10, height = 7, units = 'in')



# Accuracy by screen region -----------------------------------------------

# Written by Hemu Xu

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(ggpubr)

# # --- 1. Load the dataset ---
# df <- read_csv("webcam_data.csv")
# 
# # --- 2. Create trial index per participant ---
# df <- df %>%
#   group_by(sub, Task_Name, Trial_Nr, Trial_Id) %>%  # 加 Trial_Id
#   summarise(dummy = 1, .groups = "drop") %>%
#   group_by(sub) %>%
#   mutate(Trial = row_number()) %>%
#   select(-dummy) %>%
#   right_join(df, by = c("sub", "Task_Name", "Trial_Nr", "Trial_Id")) %>%  # join 时也加 Trial_Id
#   arrange(sub, Trial_Id, Trial_Nr)


# --- 1. Define 9 screen sections (3x3 grid, numbered 1-9 left to right, top to bottom) ---
df <- webcam %>%
  mutate(
    Section = case_when(
      el_x >= 0 & el_x < 640   & el_y >= 0   & el_y < 360   ~ 1,
      el_x >= 640 & el_x < 1280 & el_y >= 0   & el_y < 360   ~ 2,
      el_x >= 1280 & el_x <= 1920 & el_y >= 0   & el_y < 360   ~ 3,
      
      el_x >= 0 & el_x < 640   & el_y >= 360 & el_y < 720   ~ 4,
      el_x >= 640 & el_x < 1280 & el_y >= 360 & el_y < 720   ~ 5,
      el_x >= 1280 & el_x <= 1920 & el_y >= 360 & el_y < 720   ~ 6,
      
      el_x >= 0 & el_x < 640   & el_y >= 720 & el_y <= 1080  ~ 7,
      el_x >= 640 & el_x < 1280 & el_y >= 720 & el_y <= 1080  ~ 8,
      el_x >= 1280 & el_x <= 1920 & el_y >= 720 & el_y <= 1080 ~ 9,
      
      TRUE ~ NA_real_
    )
  )


# --- 2. Compute mean x/y error per section ---
# Pixel error and visual angle error (°) are both calculated
section_xy <- df %>%
  drop_na(x, y, el_x, el_y, Section) %>%
  mutate(
    x_error_px = (x - el_x),         # Horizontal error in px
    y_error_px = (y - el_y),         # Vertical error in px
    x_error    = x_error_px * 0.0187,  # Horizontal error in visual degrees
    y_error    = y_error_px * 0.0192   # Vertical error in visual degrees
  ) %>%
  group_by(Section) %>%
  summarise(
    mean_x_px = mean(x_error_px, na.rm = TRUE),
    mean_y_px = mean(y_error_px, na.rm = TRUE),
    mean_x    = mean(x_error, na.rm = TRUE),
    mean_y    = mean(y_error, na.rm = TRUE),
    se_x      = sd(x_error, na.rm = TRUE) / sqrt(n()),
    se_y      = sd(y_error, na.rm = TRUE) / sqrt(n()),
    mag       = sqrt(mean_x^2 + mean_y^2),  # Magnitude of error vector (°)
    .groups = "drop"
  )

# --- 3. Map sections to grid coordinates (col=1:3, row=3:1) ---
grid_xy <- section_xy %>%
  mutate(
    Section = as.integer(as.character(Section)),
    col = ((Section - 1) %% 3) + 1,         # 1=Left, 2=Center, 3=Right
    row = 3 - ((Section - 1) %/% 3)         # 3=Top, 2=Middle, 1=Bottom
  )

# --- 7. Scale arrow length based on maximum magnitude ---
max_mag <- max(grid_xy$mag, na.rm = TRUE)
arrow_scale <- ifelse(max_mag > 0, 0.35 / max_mag, 0.35)

grid_xy <- grid_xy %>%
  mutate(
    xend = col + arrow_scale * mean_x,
    yend = row - arrow_scale * mean_y
  )

# --- 8. Plot heatmap + arrows + labels ---
Pscreen_E3<- ggplot(grid_xy, aes(x = col, y = row)) +
  geom_tile(aes(fill = mag), color = "white") +
  
  geom_segment(aes(xend = xend, yend = yend),
               arrow = arrow(length = unit(0.12, "cm")),
               linewidth = 1.2) +
  
  geom_text(aes(label = sprintf("x: %.1fpx (%.2f°)\ny: %.1fpx (%.2f°)",
                                mean_x_px, mean_x, mean_y_px, mean_y),
                y = row + 0.35),
            lineheight = 0.95, size = 5) +
  
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_continuous(breaks = 1:3, labels = c("Left","Center","Right"), expand = c(0,0)) +
  scale_y_continuous(breaks = 1:3, labels = c("Bottom","Middle","Top"), expand = c(0,0)) +
  
  coord_fixed(ratio = 1080/1920) +
  labs(x = NULL, y = NULL, fill = "Magnitude (°)",
       #title = "b) Mean directional error per screen section- Experiment 3",
       title = "b) Experiment 2",
       subtitle = "Arrows = direction of error; tile colour = combined error magnitude (°) across x and y;\n labels = mean x/y error (°)") +
  theme_minimal(18) +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))

Pscreen_E3

save(Pscreen_E3, file= 'LAB-SPAIN/Plots/screen_error.Rda')

figure2 <- ggarrange(P_density_E3, Pscreen_E3,
                    ncol = 1, nrow = 2)

ggsave(filename = 'LAB-SPAIN/Plots/error_magnitude.png', plot = figure2,
       width = 12, height = 14, units = 'in')

ggsave(filename = 'LAB-SPAIN/Plots/error_magnitude.pdf', plot = figure2,
       width = 12, height = 14, units = 'in', device = cairo_pdf)


save(figure2, file = 'LAB-SPAIN/Plots/error_magnitude.Rda')


# Target word lexical frequency analysis ----------------------------------

# web_target<- subset(webcam, web_target_word== "Yes")
# el_target<- subset(webcam, el_target_word== "Yes")
# 
# web<- web_target %>% 
#   group_by(sub, Trial_Id, Freq, web_wordID) %>%
#   summarise(TVT= sum(time_diff, na.rm = T))%>%
#   filter(TVT>80 & TVT<=2000)
# 
# web %>% 
#   group_by(Freq) %>%
#   summarise(M= mean(TVT), SD= mean(TVT))
# 
# library(lme4)
# 
# summary(M1<- lmer(log(TVT) ~ Freq +(Freq|sub) +(Freq|Trial_Id), data= web))
# 
# el<- el_target %>% 
#   group_by(sub, Trial_Id, Freq, web_wordID) %>%
#   summarise(TVT= sum(time_diff, na.rm = T))%>%
#   filter(TVT>80 & TVT<=2000)
# 
# el %>% 
#   group_by(Freq) %>%
#   summarise(M= mean(TVT), SD= mean(TVT))
# 
# summary(M2<- lmer(log(TVT) ~ Freq +(Freq|sub) +(1|Trial_Id), data= el))

#library("devtools")
#install_github("tmalsburg/saccades/saccades", dependencies=TRUE)

library(saccades)

# webcam:
webfq<- webcam

nsubs<- unique(webfq$subject)

parsed_web<- NULL

for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  t<- subset(webfq, subject== nsubs[i])
  
  # find list:
  list= substr(t$Task_Name[1], nchar(t$Task_Name[1]), nchar(t$Task_Name[1]))
  
  freq<- t %>%
    distinct(Trial_Id, .keep_all = TRUE)%>%
    select(Trial_Id, Freq)
  
  colnames(freq)<- c('trial', 'Freq')
  
  t<- t[, c('x', 'y', 'Trial_Id', 'time_start')]
  colnames(t)<- c("x", "y", "trial", "time")
  ke.result<- detect.fixations(t, smooth.coordinates = T,
                               smooth.saccades = F)
  ke.result<- subset(ke.result, event=='fixation')
  ke.result$sub<- nsubs[i]
  ke.result$event<- NULL
  ke.result$list<- list
  
  ke.result<- ke.result %>% inner_join(freq, by= 'trial')
  
  parsed_web<- rbind(parsed_web, ke.result)
  
}


# Eyelink:
parsed_el<- NULL

for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  t<- subset(webfq, subject== nsubs[i])
  
  # find list:
  list= substr(t$Task_Name[1], nchar(t$Task_Name[1]), nchar(t$Task_Name[1]))
  
  freq<- t %>%
    distinct(Trial_Id, .keep_all = TRUE)%>%
    select(Trial_Id, Freq)
  
  colnames(freq)<- c('trial', 'Freq')
  
  t<- t[, c('el_x', 'el_y', 'Trial_Id', 'time_start')]
  colnames(t)<- c("x", "y", "trial", "time")
  ke.result<- detect.fixations(t, smooth.coordinates = T,
                               smooth.saccades = F)
  ke.result<- subset(ke.result, event=='fixation')
  ke.result$sub<- nsubs[i]
  ke.result$list<- list
  ke.result$event<- NULL
  
  ke.result<- ke.result %>% inner_join(freq, by= 'trial')
  
  parsed_el<- rbind(parsed_el, ke.result)
  
}

## re-map text coordinates to fixation data:

library(readr)
Corpus_A <- read_csv("LAB-SPAIN/sentences_fullA.csv")
Corpus_B <- read_csv("LAB-SPAIN/sentences_fullB_final.csv")

Corpus_B$index<- Corpus_B$index -100# bring to same id as list A 

source('LAB-SPAIN/get_coords.R')

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
    nitems<- unique(a$trial)
    
    for(i in 1:length(nitems)){ # for each item...
      
      c<- subset(a, trial== nitems[i])
      
      
      # get sentence coordinates and target word:
      if(c$list[1]=="A"){
        
        which_row<- which(Corpus_A$index== c$trial[1])
        freq<- Corpus_A$frecA[which_row]
        target<- ifelse(freq== 'low', Corpus_A$word_low[which_row], Corpus_A$word_high[which_row])
        
        sent<- paste(Corpus_A$sentence1A[which_row],
                     Corpus_A$sentence2A[which_row], sep= '@' )
        
        
      }else{
        which_row<- which(Corpus_B$index== c$trial[1])
        freq<- Corpus_B$frecB[which_row]
        target<- ifelse(freq== 'low', Corpus_B$word_low[which_row], Corpus_B$word_high[which_row])
        
        if(is.na(c$Freq[1])){
          c$Freq<- freq
        }
        
        sent<- paste(Corpus_B$sentece1B[which_row],
                     Corpus_B$sentence2B[which_row], sep= '@' )
        
      }
      
      coords<- get_coords(sent, revert = T)
        
      
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
    
    cat(k); cat(' ')
    
  } # end of k (subjects)
  



# Parse trial data to compute fixation duration metrics:

nsubs<- unique(web_fix$sub)

dat<- NULL

for(i in 1:length(nsubs)){
  a<- subset(web_fix, sub== nsubs[i])
  
  nitems<- unique(a$trial)
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial== nitems[j])
    
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
  
}  #end of subject (i)
  

### merge adjacent fixations under 80 ms that are next to each other (1 char)

dat_new<- NULL

nsubs<- unique(dat$sub)

for(i in 1:length(nsubs)){
  a<- subset(dat, sub== nsubs[i])
  
  nitems<- unique(a$trial)
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial==nitems[j])
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
nsubs<- sort(unique(dat$sub))

#web_fix<- web_fix %>%filter(dur>80 & dur<1000)

for(i in 1:length(nsubs)){
  a<- subset(dat, sub== nsubs[i])
  
  nitmes<- sort(unique(a$trial))
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial== nitems[j])
    
    nwords<- sort(unique(b$word_num))
    
    if(length(nwords)==0){
      next
    }
    
    for(k in 1:length(nwords)){
      
      c<- subset(b, word_num== nwords[k])
      
      TVT<- NA
      FFD<- NA
      SFD<- NA
      GD<- NA
      
      p1<- subset(c, regress==0)
      p2<- subset(c, regress==1)
      
      TVT<- sum(c$dur)
      
      if(nrow(p1)>0){
        GD<- sum(p1$dur)
        FFD<- p1$dur[1]
        
        if(nrow(p1)==1){
          SFD<- FFD
        }
        
      }
      
      t<- data.frame('sub'= b$sub[1], 'item'= b$trial[1], 'Freq'= b$Freq[1],
                     'word_num'= nwords[k], 'wordID'= c$wordID[1],
                     'target'= c$target_word[1], 'FFD'= FFD,
                     'SFD'= SFD, 'GD'= GD, 'TVT'= TVT)
      
      
      words_web<- rbind(words_web, t)
      
    }
  }
  
  
}

words_web_t<- words_web%>% filter(target== 'Yes')

outall<- which(words_web_t$FFD>1000|words_web_t$SFD>1000 | words_web_t$GD>2000 | words_web_t$TVT> 3000)

# percentange of words removed as outliers in reading measures:
length(outall)/nrow(words_web_t)*100

if(length(outall)>0){
  words_web_t<- words_web_t[-outall,]
}


  
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

#summary(M2<- lmer(log(GD) ~ Freq +(Freq|sub) +(Freq|item), data= words_web_t))


######### Eyelink data:

el_fix<- NULL
parsed_el$wordID<- NA
parsed_el$char<- NA
parsed_el$char_num<- NA
parsed_el$target_word<- NA
parsed_el$word_num<- NA
parsed_el$line<- NA
parsed_el$char_line<- NA

library(stringr)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(parsed_el, sub== nsubs[k])
  nitems<- unique(a$trial)
  
  for(i in 1:length(nitems)){ # for each item...
    
    c<- subset(a, trial== nitems[i])
    
    
    # get sentence coordinates and target word:
    if(c$list[1]=="A"){
      
      which_row<- which(Corpus_A$index== c$trial[1])
      freq<- Corpus_A$frecA[which_row]
      target<- ifelse(freq== 'low', Corpus_A$word_low[which_row], Corpus_A$word_high[which_row])
      
      sent<- paste(Corpus_A$sentence1A[which_row],
                   Corpus_A$sentence2A[which_row], sep= '@' )
      
      
    }else{
      which_row<- which(Corpus_B$index== c$trial[1])
      freq<- Corpus_B$frecB[which_row]
      target<- ifelse(freq== 'low', Corpus_B$word_low[which_row], Corpus_B$word_high[which_row])
      
      if(is.na(c$Freq[1])){
        c$Freq<- freq
      }
      
      sent<- paste(Corpus_B$sentece1B[which_row],
                   Corpus_B$sentence2B[which_row], sep= '@' )
      
    }
    
    coords<- get_coords(sent, revert = T)
    
    
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
    
    el_fix<- rbind(el_fix, c)
    
  } # end of i (items)
  
  cat(k); cat(' ')
  
} # end of k (subjects)

# Parse trial data to compute fixation duration metrics:
nsubs<- unique(el_fix$sub)

dat<- NULL

for(i in 1:length(nsubs)){
  a<- subset(el_fix, sub== nsubs[i])
  
  nitems<- unique(a$trial)
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial== nitems[j])
    
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
  
}  #end of subject (i)


### merge adjacent fixations under 80 ms that are next to each other (1 char)

dat_new<- NULL

nsubs<- unique(dat$sub)

for(i in 1:length(nsubs)){
  a<- subset(dat, sub== nsubs[i])
  
  nitems<- unique(a$trial)
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial==nitems[j])
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


words_el<- NULL
nsubs<- sort(unique(el_fix$sub))


for(i in 1:length(nsubs)){
  a<- subset(dat, sub== nsubs[i])
  
  nitmes<- sort(unique(a$trial))
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial== nitems[j])
    
    nwords<- sort(unique(b$word_num))
    
    if(length(nwords)==0){
      next
    }
    
    for(k in 1:length(nwords)){
      
      c<- subset(b, word_num== nwords[k])
      
      TVT<- NA
      FFD<- NA
      SFD<- NA
      GD<- NA
      
      p1<- subset(c, regress==0)
      p2<- subset(c, regress==1)
      
      TVT<- sum(c$dur)
      
      if(nrow(p1)>0){
        GD<- sum(p1$dur)
        FFD<- p1$dur[1]
        
        if(nrow(p1)==1){
          SFD<- FFD
        }
        
      }
      
      t<- data.frame('sub'= b$sub[1], 'item'= b$trial[1], 'Freq'= b$Freq[1],
                     'word_num'= nwords[k], 'wordID'= c$wordID[1],
                     'target'= c$target_word[1], 'FFD'= FFD,
                     'SFD'= SFD, 'GD'= GD, 'TVT'= TVT)
      
      
      words_el<- rbind(words_el, t)
      
    }
  }
  
  
}

words_el_t<- words_el%>% filter(target== 'Yes')

all<- which(words_el_t$FFD>1000|words_el_t$SFD>1000 | words_el_t$GD>2000 | words_el_t$TVT> 3000)

length(all)/nrow(words_el_t)*100

if(length(all)>0){
  words_el_t<- words_el_t[-all,]
}




words_el_t %>%  group_by(Freq)%>%
  summarise(FFD= mean(FFD, na.rm=T),
            SFD= mean(SFD, na.rm=T),
            GD= mean(GD, na.rm=T),
            TVT= mean(TVT, na.rm=T))

words_el_t %>%
  group_by(Freq) %>%
  summarise(FFD_sd = sd(FFD, na.rm = TRUE),
            SFD_sd = sd(SFD, na.rm = TRUE),
            GD_sd = sd(GD, na.rm = TRUE),
            TVT_sd = sd(TVT, na.rm = TRUE))


# combine two target word data frames:
words_web_t$Tracker<- "Webcam"
words_el_t$Tracker<- "Eyelink"

words_dat<- rbind(words_web_t, words_el_t)
write.csv(x = words_dat, file = 'LAB-SPAIN/data/target_word_fixation_data.csv')


### save all words in the sentence:
words_web$Tracker<- "Webcam"
words_el$Tracker<- "Eyelink"

all_words_dat<- rbind(words_web, words_el)
all_words_dat$word_length<- nchar(all_words_dat$wordID)
write.csv(all_words_dat, 'LAB-SPAIN/data/all_words_fixation_data.csv')


# generate descriptive statistics:
library(gtsummary)
library(dplyr)

words_dat %>%
  mutate(Freq_tracker = interaction(Freq,Tracker)) %>%   # combine into one
  select(Freq_tracker, FFD, SFD, GD, TVT) %>%
  tbl_summary(by = Freq_tracker,
              statistic = list(all_continuous() ~ "{mean} ({sd})"))


# fit lmer models:
words_dat$Tracker<- as.factor(words_dat$Tracker)
contrasts(words_dat$Tracker)<- c(-1, 1)

words_dat$Freq<- as.factor(words_dat$Freq)
contrasts(words_dat$Freq)<- c(-1, 1)

library(lmerTest)

## Models:

summary(M1<- lmer(log(FFD)~ Freq*Tracker +(Freq|sub)+(Freq|item), data= words_dat))

summary(M2<- lmer(log(SFD)~ Freq*Tracker +(Freq+Tracker|sub)+(Freq|item), data= words_dat))

summary(M3<- lmer(log(GD)~ Freq*Tracker +(Freq|sub)+(Freq|item), data= words_dat))

summary(M4<- lmer(log(TVT)~ Freq*Tracker +(Freq+Tracker|sub)+(Freq|item), data= words_dat))


library(sjPlot)

tab_model(M1,M2, show.stat = T,show.se = T,file="LAB/TW_models1.html", digits = 3,
          digits.p = 3, digits.rsq = 3, digits.re = 3, show.est = T,
          transform = NULL, string.est = "Estimate", show.ci = F)

tab_model(M3,M4, show.stat = T,show.se = T,file="LAB/TW_models2.html", digits = 3,
          digits.p = 3, digits.rsq = 3, digits.re = 3, show.est = T,
          transform = NULL, string.est = "Estimate", show.ci = F)

## emeans post-hoc tests:
library(emmeans)
# FFD
PH1= pairs(emmeans(M1, ~ Freq | Tracker), adjust = "holm")
PH1

#SFD
PH2= pairs(emmeans(M2, ~ Freq | Tracker), adjust = "holm")
PH2

#GD
PH3= pairs(emmeans(M3, ~ Freq | Tracker), adjust = "holm")
PH3

# TVT
PH4= pairs(emmeans(M4, ~ Freq | Tracker), adjust = "holm")
PH4

library(ggeffects)
library(ggplot2)
library(dplyr)

# Step 1: Create a list of models
models <- list(M1 = M1, M2 = M2, M3 = M3, M4 = M4)

# Step 2: Generate predicted values for Tracker × Freq interaction
pred_list <- lapply(names(models), function(name) {
  pred <- ggpredict(models[[name]], terms = c("Tracker", "Freq"))
  pred$model <- name  # add model name for faceting
  pred
})

# Step 3: Combine predictions into one data frame
pred_all <- bind_rows(pred_list)
pred_all$model[which(pred_all$model=="M1")]<- "FFD"
pred_all$model[which(pred_all$model=="M2")]<- "SFD"
pred_all$model[which(pred_all$model=="M3")]<- "GD"
pred_all$model[which(pred_all$model=="M4")]<- "TVT"



M_plot<- pred_all %>% ggplot(aes(x= group, y= predicted, color= x, fill= x, group= x))+
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(values=pallete1[1:2])+
  scale_fill_manual(values=pallete1[1:2])+
  facet_wrap(~model, ncol = 2,scales = "free_y" )+
  labs(x = "Frequency",
       y = "Predicted duration (log ms)",
       color = "Tracker",
       fill = "Tracker")+
  theme_classic(18)+
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_blank() 
  )

ggsave(filename = 'LAB/Plots/Model_interaction.pdf', plot = M_plot,
       width = 8, height = 7)
ggsave(filename = 'LAB/Plots/Model_interaction.png', plot = M_plot,
       width = 8, height = 7)
