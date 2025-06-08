library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lme4)
library(lmerTest)  # 可选，用于获得 p 值

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

# save
# write.csv(df, "Data with Participant tag.csv", row.names = FALSE)



# Generate consecutive trial number for participants
df <- df %>%
  group_by(Participant, Task_Name, Trial_Nr) %>%
  summarise(dummy = 1, .groups = "drop") %>%
  group_by(Participant) %>%
  mutate(Trial = row_number()) %>%
  dplyr::select(-dummy) %>%
  right_join(df, by = c("Participant", "Task_Name", "Trial_Nr")) %>%
  arrange(Participant, Trial, Trial_Nr)

# Identify participants with an insufficient number of trials
incomplete_ids <- df %>%
  group_by(Participant) %>%
  summarise(n_trial = n_distinct(Trial)) %>%
  filter(n_trial < 100) %>%
  pull(Participant)

# exclude
df_clean <- df %>%
  filter(!Participant %in% incomplete_ids)

# --- Blink detection ---
#Mark each contiguous segment where conf == 0 or el_pupil == 0 as a separate blink group (can ajuste the sensitivity for conf to 0.2)
df_processed <- df_clean %>%
  arrange(Participant, Trial, time) %>%
  group_by(Participant, Trial) %>%
  mutate(
    webcam_group = rleid(conf == 0),
    eyelink_group = rleid(el_pupil == 0),
    conf_zero = (conf == 0),
    pupil_zero = (el_pupil == 0)
  ) %>%
  ungroup()

# --- Summarise webcam blink info ---
# For each blink episode detected by the webcam, calculate duration (by summing time_diff)
webcam_summary <- df_processed %>%
  filter(conf_zero) %>%
  group_by(Participant, Trial, webcam_group) %>%
  summarise(
    blink_dur = sum(time_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Participant, Trial) %>%
  summarise(
    webcam_blink = n(),
    webcam_duration_total = sum(blink_dur),
    webcam_duration_mean = ifelse(webcam_blink > 0, webcam_duration_total / webcam_blink, NA),
    .groups = "drop"
  )
# --- Summarise EyeLink blink info ---
# For each blink episode detected by EyeLink, calculate duration
eyelink_summary <- df_processed %>%
  filter(pupil_zero) %>%
  group_by(Participant, Trial, eyelink_group) %>%
  summarise(
    blink_dur = sum(time_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Participant, Trial) %>%
  summarise(
    eyelink_blink = n(),
    eyelink_duration_total = sum(blink_dur),
    eyelink_duration_mean = ifelse(eyelink_blink > 0, eyelink_duration_total / eyelink_blink, NA),
    .groups = "drop"
  )

# --- Get Freq condition per trial ---
# Extract the Freq condition for each trial (assumes constant within trial)
freq_info <- df_clean %>%
  group_by(Participant, Trial) %>%
  summarise(Freq = first(Freq), .groups = "drop")

# --- Combine summaries into one table ---
blink_summary <- freq_info %>%
  left_join(webcam_summary, by = c("Participant", "Trial")) %>%
  left_join(eyelink_summary, by = c("Participant", "Trial")) %>%
  arrange(Participant, Trial)

# View final summary table
View(blink_summary)


# --- Linear Mixed Model ---
# Convert wide-format summary into long format
blink_long <- blink_summary %>%
  pivot_longer(
    cols = c(webcam_blink, eyelink_blink),
    names_to = "Method",
    names_pattern = "(.*)_blink",
    values_to = "Blink_Count"
  ) %>%
  pivot_longer(
    cols = c(webcam_duration_mean, eyelink_duration_mean),
    names_to = "Method_dur",
    names_pattern = "(.*)_duration_mean",
    values_to = "Duration_Mean"
  ) %>%
  filter(Method == Method_dur) %>%
  select(Participant, Trial, Freq, Method, Blink_Count, Duration_Mean) %>%
  mutate(
    Blink_Count = replace_na(Blink_Count, 0),
    Duration_Mean = replace_na(Duration_Mean, 0)
  )

# Convert categorical predictors to factors and set reference levels
blink_long <- blink_long %>%
  mutate(
    Freq = factor(Freq, levels = c("low", "high")),
    Method = factor(Method, levels = c("eyelink", "webcam"))  # 设置参照组
  )

# Model 1: Blink count ~ Method (random intercept by Participant)
model_blink <- lmer(Blink_Count ~ Method + (1 | Participant), data = blink_long)

summary(model_blink)
# Model 2: Blink duration ~ Method (random intercept by Participant)
model_duration <- lmer(Duration_Mean ~ Method + (1 | Participant), data = blink_long)

summary(model_duration)

# --- false positive/false negative ---

# Reformat into wide format with one row per participant × trial
blink_wide <- blink_long %>%
  select(Participant, Trial, Method, Blink_Count) %>%
  pivot_wider(
    names_from = Method,
    values_from = Blink_Count,
    values_fill = 0  # 将 NA 填成 0
  )
# Classify each trial into detection types based on blink count combinations
blink_eval <- blink_wide %>%
  mutate(
    detection_type = case_when(
      eyelink == 0 & webcam > 0 ~ "False Positive",
      eyelink > 0 & webcam == 0 ~ "False Negative",
      eyelink == webcam & eyelink == 0 ~ "True Negative",
      eyelink == webcam & eyelink > 0 ~ "True Positive",
      eyelink > webcam & webcam > 0 ~ "Partial False Negative",
      webcam > eyelink & eyelink > 0 ~ "Partial False Positive",
      TRUE ~ "Unclassified"  # 保底，理论上不会出现
    )
  )
# Count and percentage of each detection type
table(blink_eval$detection_type)
prop.table(table(blink_eval$detection_type)) %>%
  round(4) * 100

# --- Exploratory Analysis ---
# Recode Freq into a 3-level factor: low, high, neutral
blink_long_exp <- blink_long %>%
  mutate(Freq3 = case_when(
    Freq == "low" ~ "low",
    Freq == "high" ~ "high",
    is.na(Freq) ~ "neutral"
  )) %>%
  mutate(Freq3 = factor(Freq3, levels = c("neutral", "low", "high")))

# Exploratory LMM: Interaction between Freq3 and Method
model_dur_exploratory <- lmer(
  Duration_Mean ~ Freq3 * Method + (1 + Method | Participant),# Random slope for Method
  data = blink_long_exp
)
model_count_exploratory <- lmer(
  Blink_Count ~ Freq3 * Method + (1 | Participant),# Random intercept only
  data = blink_long_exp
)
summary(model_dur_exploratory)
summary(model_count_exploratory)