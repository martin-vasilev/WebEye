
# Martin R. Vasilev, 2025

rm(list= ls())


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
webcam <- read.csv("~/R/WebEye/LAB/data/webcam_raw_data.csv")

# remove samples within blinks:
n_full<- nrow(webcam)

webcam<- webcam %>% filter(conf>0 & el_pupil>0)

# samples removed due to blinks:
(1- nrow(webcam)/n_full)*100
n_blinks<- nrow(webcam)

# remove samples outside of screen bounds:
webcam<- webcam %>% filter(el_x>0 & el_x<=1920 & el_y>0 & el_y<=1080 & x>0 & x<=1920 & y>0 & y<1080)



sub_correlations<- webcam %>% select(-Freq)%>% drop_na() %>% group_by(Task_Name, sub)%>% summarise(Corr_x= cor(x,el_x), Corr_y= cor(y,el_y))
sub_correlations<- sub_correlations %>% pivot_longer(cols = 3:4, names_to = 'Dimension', values_to = 'corr' )

sub_correlations$Task_Name[which(sub_correlations$Task_Name=='Freq_sentences')]<- 'Frequency corpus'

sub_correlations$Task_Name[which(sub_correlations$Task_Name=='Single_line_sentences')]<- 'Single-line corpus'

fun_mean <- function(x, rounding= 2){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), rounding), sep= '')))}

P1<- ggplot(sub_correlations, aes(x = Dimension, y = corr, 
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
               hjust= 0.75, colour="black", size= 7);P1

ggsave(filename = 'LAB/Plots/correlations.pdf', width = 10,
       height = 8, units = 'in')


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

ggsave(filename = 'LAB/Plots/BA_combined.png', plot = figure,
       width = 10, height = 12, units = 'in')

