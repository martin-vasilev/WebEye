
# Martin R. Vasilev, 2025

rm(list= ls())


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

library(tidyverse)
library(readr)
trial <- read_csv("LAB/data/trial_data.csv")


# Comprehension accuracy --------------------------------------------------

desc_acc<- trial %>% group_by(Task_Name) %>%summarise(M= mean(question_accuracy)*100,SD= sd(question_accuracy)*100)

desc_acc[,2:3]<- round(desc_acc[,2:3],1)

## accuracy by participant:

sub_acc<- trial %>% 
  group_by(Rec_Session_Id, Task_Name) %>%
  summarise(M= mean(question_accuracy))

range_freq<- round(range(sub_acc$M[which(sub_acc$Task_Name=='sentence')])*100,1)
range_single_line<- round(range(sub_acc$M[which(sub_acc$Task_Name=='sentence_DC')])*100,1)


## trial reading times:
trial$duration<- trial$trial_end - trial$trial_start

trial %>% 
  group_by(Task_Name) %>%
  summarise(M= mean(duration, na.rm= T), SD= sd(duration, na.rm = T))


# Sample correlations -----------------------------------------------------

# load in webcam data:
webcam <- read_csv("LAB/data/webcam_data.csv.zip")
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
  group_by(sub)%>%
  summarise(M= mean(hz), 
            SD= sd(hz))

mean(sub$M)

mean(sub$M[which(sub$sub>4)]) # excluding first 3 subjects recorded at 30 Hz

# # Convert ms to seconds
# time_diff_s <- valid_diffs / 1000
# 
# # Effective sampling rate in Hz
# sampling_rate_hz <- 1 / mean(time_diff_s)
# 
# sampling_rate_hz


sub_correlations<- webcam %>%
#  select(-Freq)%>% 
#  drop_na() %>% 
  group_by(Task_Name, sub)%>% 
  summarise(Corr_x= cor(x,el_x), Corr_y= cor(y,el_y))

sub_correlations<- sub_correlations %>%
  pivot_longer(cols = 3:4, names_to = 'Dimension', values_to = 'corr' )

sub_correlations$Task_Name[which(sub_correlations$Task_Name=='Freq_sentences')]<- 'Frequency corpus'
sub_correlations$Task_Name[which(sub_correlations$Task_Name=='Single_line_sentences')]<- 'Single-line corpus'

fun_mean <- function(x, rounding= 2){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), rounding), sep= '')))}

P1_cor<- ggplot(sub_correlations, aes(x = Dimension, y = corr, 
                                  color= Dimension, fill= Dimension)) + 
  facet_wrap(~Task_Name)+
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
               hjust= 0.75, colour="black", size= 7);P1_cor

ggsave(plot = P1_cor, filename = 'LAB/Plots/correlations.pdf', width = 10,
       height = 8, units = 'in')


load('LAB-SPAIN/Plots/correlations.Rda')

P1<- P1+ ggtitle('Experiment 3')+theme(plot.title = element_text(hjust = 0.5))
P1_cor<- P1_cor+ ggtitle('Experiment 1')+theme(plot.title = element_text(hjust = 0.5))

# combine the two plots:
library(ggpubr)
figure_corr <- ggarrange(P1_cor, P1,
                    ncol = 2, nrow = 1, widths = c(1.8, 1))

ggsave(filename = 'LAB/Plots/correlations_combined.png', plot = figure_corr,
       width = 16, height = 10, units = 'in')

ggsave(filename = 'LAB/Plots/correlations_combined.pdf', plot = figure_corr,
       width = 16, height = 10, units = 'in', device = cairo_pdf)



# Bland-Altman plot -------------------------------------------------------

# Compute sample-to-sample absolute deviations
webcam$dy_webcam <- abs(diff(c(NA, webcam$y)))
webcam$dy_eyelink <- abs(diff(c(NA, webcam$el_y)))

webcam$dx_webcam <- abs(diff(c(NA, webcam$x)))
webcam$dx_eyelink <- abs(diff(c(NA, webcam$el_x)))

webcam$Task_Name[which(webcam$Task_Name=='Freq_sentences')]<- 'Frequency corpus'

webcam$Task_Name[which(webcam$Task_Name=='Single_line_sentences')]<- 'Single-line corpus'


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
  group_by(Task_Name) %>%
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
    group_by(Task_Name) %>%
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
  facet_wrap(vars(Task_Name)) +
  labs(title = "Y position",
       x = "Mean Y (Webcam & EyeLink)",
       y = "Difference in Y (Webcam - EyeLink)",
       color = "Point Density") +
  theme_classic(base_size = 20)+
  theme(
    strip.background = element_blank(),  # removes box background
    strip.text = element_text(face = "bold", size = 14),  # optional: style text
    plot.title = element_text(hjust = 0.5))

ggsave(filename = 'LAB/Plots/BA_sl.png', plot = Plot_y, width = 10, height = 7, units = 'in')



loa_data2 <- ba %>%
  group_by(Task_Name) %>%
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
  
  facet_wrap(vars(Task_Name)) +
  labs(title = "X position",
       x = "Mean X (Webcam & EyeLink)",
       y = "Difference in X (Webcam - EyeLink)",
       color = "Point Density") +
  theme_classic(base_size = 20)+
  theme(
    strip.background = element_blank(),  # removes box background
    strip.text = element_text(face = "bold", size = 14),  # optional: style text
    plot.title = element_text(hjust = 0.5))

ggsave(filename = 'LAB/Plots/BA_sl2.png', plot = Plot_x, width = 10, height = 7, units = 'in')


# combine the two plots:
library(ggpubr)
figure <- ggarrange(Plot_x, Plot_y,
                    ncol = 1, nrow = 2)

#figure_e1<- figure+ ggtitle("Experiment 1")+theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
figure_e1<- figure

load('LAB-SPAIN/Plots/BA_combined.Rda')

figure_e1 <- ggarrange(text_grob("Experiment 1", size = 30),
                       figure_e1,
                               ncol = 1, heights = c(0.1, 1))

figure_e3 <- ggarrange(text_grob("Experiment 3", size = 30),
                       figure_e3,
                       ncol = 1, heights = c(0.1, 1))

figure_f <- ggarrange(figure_e1, figure_e3,
                    ncol = 2, nrow = 1, widths = c(1.4, 1))


ggsave(filename = 'LAB/Plots/BA_combined.png', plot = figure_f,
       width = 16, height = 12, units = 'in')

ggsave(filename = 'LAB/Plots/BA_combined.pdf', plot = figure_f,
       width = 16, height = 12, units = 'in', device = cairo_pdf)



# Subject-level accuracy --------------------------------------------------


webcam$diff_x<- (webcam$x-webcam$el_x)*0.0187

webcam$diff_y<- (webcam$y- webcam$el_y)*0.0192

sub= webcam %>% group_by(sub, Task_Name)%>%
  summarise(`X position`= mean(diff_x, na.rm = T),
            `Y position`= mean(diff_y, na.rm = T))

webcam %>% 
  summarise(`X position_M`= mean(diff_x, na.rm = T),
            `X position_SD`= sd(diff_x, na.rm = T),
            `Y position_M`= mean(diff_y, na.rm = T),
            `Y position_SD`= sd(diff_y, na.rm = T))



sub= sub %>% 
  pivot_longer(cols = 3:4, names_to = 'Dimension', values_to = 'error')


P_density=sub %>%
  ggplot(aes(x = error, fill = Dimension)) +
  geom_density(alpha = 0.4) +
  geom_rug(aes(color = Dimension), sides = "b", alpha = 0.6) +
  facet_wrap(~Dimension)+
  theme_minimal(18)+
  theme(
    panel.spacing = unit(2, "lines") # increase spacing between facets
  )+ geom_vline(xintercept = 0,linetype = 2)+
  labs(x= 'Error (Webcam - Eyelink) in deg', y= "Density",
       title = "a) Mean error per subject and screen dimension (°)")+
  theme(panel.grid = element_blank(),
        plot.title.position = "plot")


ggsave(filename = 'LAB/Plots/Subject_acc_density.pdf',
       plot = P_density, width = 10, height = 7, units = 'in')
ggsave(filename = 'LAB/Plots/Subject_acc_density.png',
       plot = P_density, width = 10, height = 7, units = 'in')




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
Pscreen<- ggplot(grid_xy, aes(x = col, y = row)) +
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
       title = "b) Mean directional error per screen section",
       subtitle = "Arrows = direction of error; tile colour = combined error magnitude (°) across x and y;\n labels = mean x/y error (°)") +
  theme_minimal(18) +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot")

Pscreen

library(ggpubr)
figure2 <- ggarrange(P_density, Pscreen,
                    ncol = 1, nrow = 2)

ggsave(filename = 'LAB/Plots/error_magnitude.png', plot = figure2,
       width = 12, height = 14, units = 'in')

ggsave(filename = 'LAB/Plots/error_magnitude.pdf', plot = figure2,
       width = 12, height = 14, units = 'in', device = cairo_pdf)




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
webfq<- webcam%>% filter(Task_Name== 'Frequency corpus')

nsubs<- unique(webfq$sub)

parsed_web<- NULL

for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  t<- subset(webfq, sub== nsubs[i])
  
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
  
  ke.result<- ke.result %>% inner_join(freq, by= 'trial')
  
  parsed_web<- rbind(parsed_web, ke.result)
  
}


# Eyelink:
parsed_el<- NULL

for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  t<- subset(webfq, sub== nsubs[i])
  
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
  ke.result$event<- NULL
  
  ke.result<- ke.result %>% inner_join(freq, by= 'trial')
  
  parsed_el<- rbind(parsed_el, ke.result)
  
}

## re-map text coordinates to fixation data:

library(readr)
Corpus_fq <- read_csv("LAB/Corpus_fq.csv")
Corpus_fq<- Corpus_fq[1:120,]
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
    nitems<- unique(a$trial)
    
    for(i in 1:length(nitems)){ # for each item...
      
      c<- subset(a, trial== nitems[i])
        
      freq<- ifelse(c$Freq[1]== 'low', 'LF', 'HF')
        
      sent<-Corpus_fq$line_breaks[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
      coords<- get_coords(sent, revert = T)
      
      target<- Corpus_fq$`Target (N)`[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
        
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

words_web_t<- words_web_t[-outall,]

  
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
    
    freq<- ifelse(c$Freq[1]== 'low', 'LF', 'HF')
    
    sent<-Corpus_fq$line_breaks[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
    coords<- get_coords(sent, revert = T)
    
    target<- Corpus_fq$`Target (N)`[which(Corpus_fq$Study_ID== nitems[i] & Corpus_fq$`Frequency type`== freq)[1]]
    
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

words_el_t<- words_el_t[-all,]


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
write.csv(x = words_dat, file = 'LAB/data/target_word_fixation_data.csv')


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

summary(M1<- lmerTest::lmer(log(FFD)~ Freq*Tracker +(Tracker|sub)+(Freq+Tracker|item), data= words_dat))

summary(M2<- lmerTest::lmer(log(SFD)~ Freq*Tracker +(Freq+Tracker|sub)+(Freq+Tracker|item), data= words_dat))

summary(M3<- lmerTest::lmer(log(GD)~ Freq*Tracker +(Freq+Tracker|sub)+(Freq|item), data= words_dat))

summary(M4<- lmerTest::lmer(log(TVT)~ Freq*Tracker +(Freq|sub)+(Freq|item), data= words_dat))


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



# Corpus frequency- Single sentences --------------------------------------

# webcam:
corpus_web<- webcam%>% filter(Task_Name== 'Single-line corpus')

nsubs<- unique(corpus_web$sub)

parsed_c_web<- NULL
library(saccades)

for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  t<- subset(corpus_web, sub== nsubs[i])
  
  t<- t[, c('x', 'y', 'Trial_Id', 'time_start')]
  colnames(t)<- c("x", "y", "trial", "time")
  ke.result<- detect.fixations(t, smooth.coordinates = T,
                               smooth.saccades = F)
  ke.result<- subset(ke.result, event=='fixation')
  ke.result$sub<- nsubs[i]
  ke.result$event<- NULL
  
  parsed_c_web<- rbind(parsed_c_web, ke.result)
  
}


# Eyelink:
parsed_c_el<- NULL

for(i in 1:length(nsubs)){
  
  cat(sprintf("subject %g \n", i))
  
  t<- subset(corpus_web, sub== nsubs[i])
  
  t<- t[, c('el_x', 'el_y', 'Trial_Id', 'time_start')]
  colnames(t)<- c("x", "y", "trial", "time")
  ke.result<- detect.fixations(t, smooth.coordinates = T,
                               smooth.saccades = F)
  ke.result<- subset(ke.result, event=='fixation')
  ke.result$sub<- nsubs[i]
  ke.result$event<- NULL
  
  parsed_c_el<- rbind(parsed_c_el, ke.result)
  
}


## re-map text coordinates to fixation data:

library(readr)
Corpus_sent <- read_csv("corpus/Corpus_sent.csv")
source('preproc/functions/get_coords.R')

web_c_fix<- NULL
parsed_c_web$wordID<- NA
parsed_c_web$char<- NA
parsed_c_web$char_num<- NA
parsed_c_web$word_num<- NA
parsed_c_web$line<- NA
parsed_c_web$char_line<- NA

library(stringr)


for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(parsed_c_web, sub== nsubs[k])
  nitems<- unique(a$trial)
  
  for(i in 1:length(nitems)){ # for each item...
    
    c<- subset(a, trial== nitems[i])
    
    sent<-Corpus_sent$Sentence[which(Corpus_sent$Study_ID== nitems[i])]
    coords<- get_coords(sent, revert = T)
    
    # sentence is on a single line so y dimension is not important
    # we set the y limits of the text box to the screen size
    coords$y1<- 1
    coords$y2<- 1080
    
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
        
        
      }
      
    } # end of j (fixations)
    
    web_c_fix<- rbind(web_c_fix, c)
    
  } # end of i (items)
  
  cat(k); cat(' ')
  
} # end of k (subjects)



# Parse trial data to compute fixation duration metrics:
nsubs<- unique(web_c_fix$sub)

dat_c<- NULL

for(i in 1:length(nsubs)){
  a<- subset(web_c_fix, sub== nsubs[i])
  
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
    
    dat_c<- rbind(dat_c, b)
    
  } # end of item (j)
  
}  #end of subject (i)



## merge adjacent fixations under 80 ms that are next to each other (1 char)

dat_new_c<- NULL

nsubs<- unique(dat_c$sub)

for(i in 1:length(nsubs)){
  a<- subset(dat_c, sub== nsubs[i])
  
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
    dat_new_c<- rbind(dat_new_c, b)
    
  }
}

table(dat_new_c$remove)
dat<- dat_new_c %>% filter(is.na(remove))

# percentage of < 80 ms fixations merged
(1-nrow(dat)/ nrow(dat_new_c))*100


out_L80<- which(dat$dur<80)

# percentage of < 80ms fixations left unmerged:
(length(out_L80)/nrow(dat_new_c))*100

dat<- dat[-out_L80,]


out<- which(dat$dur> 1000)

# percentage of >1000 ms fixations discarded
length(out)/nrow(dat_new_c)*100
dat<- dat[-out,]




words_c_web<- NULL
nsubs<- sort(unique(dat_c$sub))

#dat_c<- dat_c %>%filter(dur>80 & dur<1000)

for(i in 1:length(nsubs)){
  a<- subset(dat_c, sub== nsubs[i])
  
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
      
      t<- data.frame('sub'= b$sub[1], 'item'= b$trial[1],
                     'word_num'= nwords[k], 'wordID'= c$wordID[1], 'FFD'= FFD,
                     'SFD'= SFD, 'GD'= GD, 'TVT'= TVT)
      
      
      words_c_web<- rbind(words_c_web, t)
      
    }
  }
  
  
}

library(EMreading)

words_c_web<- Frequency(words_c_web)
words_c_web$word_length<- nchar(words_c_web$wordID)

outall<- which(words_c_web$FFD>1000|words_c_web$SFD>1000 | words_c_web$GD>2000 | words_c_web$TVT> 3000)

# percentange of words removed as outliers in reading measures:
length(outall)/nrow(words_c_web)*100

words_c_web<- words_c_web[-outall,]




summary(M5<- lmer(log(SFD) ~ scale(zipf)*scale(word_length) +(1|sub) +(1|item), 
                  data= words_c_web))


######### Eyelink data:

el_c_fix<- NULL
parsed_c_el$wordID<- NA
parsed_c_el$char<- NA
parsed_c_el$char_num<- NA
parsed_c_el$word_num<- NA
parsed_c_el$line<- NA
parsed_c_el$char_line<- NA

library(stringr)

for(k in 1:length(nsubs)){ # for each subject...
  
  a<- subset(parsed_c_el, sub== nsubs[k])
  nitems<- unique(a$trial)
  
  for(i in 1:length(nitems)){ # for each item...
    
    c<- subset(a, trial== nitems[i])
    
    
    sent<-Corpus_sent$Sentence[which(Corpus_sent$Study_ID== nitems[i])]
    coords<- get_coords(sent, revert = T)
    
    # sentence is on a single line so y dimension is not important
    # we set the y limits of the text box to the screen size
    coords$y1<- 1
    coords$y2<- 1080
    
    
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
        
        
      }
      
    } # end of j (fixations)
    
    el_c_fix<- rbind(el_c_fix, c)
    
  } # end of i (items)
  
  cat(k); cat(' ')
  
} # end of k (subjects)


# Parse trial data to compute fixation duration metrics:
nsubs<- unique(el_c_fix$sub)

dat_c<- NULL

for(i in 1:length(nsubs)){
  a<- subset(el_c_fix, sub== nsubs[i])
  
  nitems<- unique(a$trial)
  
  for(j in 1:length(nitems)){
    b<- subset(a, trial== nitems[j])
    
    b$regress<- NA
    
    max_word<- 1
    
    max_fixated<- max(b$word_num, na.rm=T)
    
    if(max_fixated<0){
      #dat<- rbind(dat, b)
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
    
    dat_c<- rbind(dat_c, b)
    
  } # end of item (j)
  
}  #end of subject (i)


## merge adjacent fixations under 80 ms that are next to each other (1 char)

dat_new_c<- NULL

nsubs<- unique(dat_c$sub)

for(i in 1:length(nsubs)){
  a<- subset(dat_c, sub== nsubs[i])
  
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
    dat_new_c<- rbind(dat_new_c, b)
    
  }
}

table(dat_new_c$remove)
dat<- dat_new_c %>% filter(is.na(remove))

# percentage of < 80 ms fixations merged
(1-nrow(dat)/ nrow(dat_new_c))*100


out_L80<- which(dat$dur<80)

# percentage of < 80ms fixations left unmerged:
(length(out_L80)/nrow(dat_new_c))*100

dat<- dat[-out_L80,]


out<- which(dat$dur> 1000)

# percentage of >1000 ms fixations discarded
length(out)/nrow(dat_new_c)*100
dat<- dat[-out,]




words_c_el<- NULL
nsubs<- sort(unique(dat$sub))

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
      
      t<- data.frame('sub'= b$sub[1], 'item'= b$trial[1],
                     'word_num'= nwords[k], 'wordID'= c$wordID[1],
                     'FFD'= FFD,
                     'SFD'= SFD, 'GD'= GD, 'TVT'= TVT)
      
      
      words_c_el<- rbind(words_c_el, t)
      
    }
  }
  
  
}


words_c_el<- Frequency(words_c_el)
words_c_el$word_length<- nchar(words_c_el$wordID)


outall<- which(words_c_el$FFD>1000|words_c_el$SFD>1000 | words_c_el$GD>2000 | words_c_el$TVT> 3000)

# percentange of words removed as outliers in reading measures:
length(outall)/nrow(words_c_el)*100

if(length(outall)>0){
  words_c_el<- words_c_el[-outall,]
}


# combine two target word data frames:
words_c_web$Tracker<- "Webcam"
words_c_el$Tracker<- "Eyelink"

words_corpus<- rbind(words_c_web, words_c_el)
write.csv(x = words_corpus, file = 'LAB/data/corpus_fixation_data.csv')




