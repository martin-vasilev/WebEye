
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)

# load the data
df <- read_csv("webcam_data.csv")
# convert Trial_Nr to numric
df$Trial_Nr <- as.numeric(df$Trial_Nr)

# Create a continuous 'Trial' index per participant (regardless of block)
# This is used for grouping and matching in later steps
participant_id <- numeric(nrow(df))
current_id <- 1
participant_id[1] <- current_id

# Logic: as soon as Trial_Nr goes from 60 -> 1, it is counted as a new participant
for (i in 2:nrow(df)) {
  if (df$Trial_Nr[i] == 1 && df$Trial_Nr[i - 1] == 60) {
    current_id <- current_id + 1
  }
  participant_id[i] <- current_id
}

# Add ‘Participants’ column
df$Participant <- paste0("P", sprintf("%02d", participant_id))

# Check: see how many rows each participant has
print(table(df$Participant))  # Each should have 100 lines (40+60)

# Generate consecutive trial number for participants
df <- df %>%
  group_by(Participant, Task_Name, Trial_Nr) %>%
  summarise(dummy = 1, .groups = "drop") %>%
  group_by(Participant) %>%
  mutate(Trial = row_number()) %>%
  dplyr::select(-dummy) %>%
  right_join(df, by = c("Participant", "Task_Name", "Trial_Nr")) %>%
  arrange(Participant, Trial, Trial_Nr)


# exlucde blinks detected in eyelink, exclude noise from webcam
df_clean <- df %>%
  filter(
    el_pupil != 0,
    x >= 1, x <= 1920,
    y >= 1, y <= 1080
  ) %>%
  select(Participant, Trial, x, y, el_x, el_y, Trial_Nr, Trial_Id, Task_Name, Freq)

# define sections
df_clean <- df_clean %>%
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

# check distribution
print(table(df_clean$Section, useNA = "ifany"))

# --- Correlation ---
# calclulate the correlation for x and y
cor_summary <- df_clean %>%
  filter(!is.na(Section)) %>%
  group_by(Participant, Section) %>%
  summarise(
    r_x = cor(el_x, x, use = "complete.obs"),
    r_y = cor(el_y, y, use = "complete.obs"),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    z_x = atanh(r_x),  # Fisher Z
    z_y = atanh(r_y)
  )

# visualization
cor_section_long <- cor_section_summary %>%
  pivot_longer(cols = c(r_x, r_y), names_to = "Dimension", names_prefix = "r_", values_to = "r")

ggplot(cor_section_long, aes(x = factor(Section), y = r, fill = Dimension)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("x" = "#377eb8", "y" = "#e41a1c")) +
  labs(
    title = "Gaze correlation by screen section and dimension",
    x = "Screen Section (1–9)",
    y = "Pearson Correlation (r)",
    fill = "Dimension"
  ) +
  theme_minimal(base_size = 14)
#webcam gaze tracking along the horizontal dimension was strongest in Sections 2, 5, and 8 (the vertical center of the screen)


# LMM to check whether section influence correlation
lmm_zx <- lmer(z_x ~ factor(Section) + (1|Participant), data = cor_summary)
summary(lmm_zx)

lmm_zy <- lmer(z_y ~ factor(Section) + (1|Participant), data = cor_summary)
summary(lmm_zy)

cor_summary <- cor_summary %>%
  mutate(Section = factor(Section),
         Section = relevel(Section, ref = "5"))


emmeans(lmm_zx, pairwise ~ Section)
emmeans(lmm_zy, pairwise ~ Section)


# --- Absolute Distnace --- 
# set the reference as section 5
df_clean <- df_clean %>%
  mutate(
    Gaze_Dist = sqrt((x - el_x)^2 + (y - el_y)^2)
  )

df_clean <- df_clean %>%
  mutate(Section = factor(Section),
         Section = relevel(Section, ref = "5"))

# LMM
lmm_dist_centerref <- lmer(Gaze_Dist ~ Section + (1 | Participant), data = df_clean)
summary(lmm_dist_centerref)

# Post-hoc
emmeans(lmm_dist_centerref, pairwise ~ Section)

#supplementary analysis (robust check)
# average to trial level
df_trial_level <- df_clean %>%
  filter(!is.na(Section)) %>%
  group_by(Participant, Trial_Id, Section) %>%
  summarise(
    Mean_Dist = mean(Gaze_Dist, na.rm = TRUE),
    .groups = "drop"
  )
df_trial_level <- df_trial_level %>%
  mutate(Section = factor(Section),
         Section = relevel(Section, ref = "5"))
lmm_trial <- lmer(Mean_Dist ~ Section + (1 | Participant), data = df_trial_level)
summary(lmm_trial)
