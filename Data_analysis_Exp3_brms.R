library(tidyverse)
library(lme4)
library(lmerTest)
library(cmdstanr)
library(brms)
library(qs2)


target_words_exp3 <- read_csv("LAB-SPAIN/data/target_word_fixation_data.csv") %>%
  mutate(Freq = factor(Freq, levels = c("low", "high")),
         Tracker = factor(Tracker, levels = c("Webcam", "Eyelink")),
         item = factor(item),
         sub = factor(sub))

corpus_words_exp3 <- read_csv("LAB-SPAIN/data/all_words_fixation_data.csv") %>%
  mutate(Tracker = factor(Tracker, levels = c("Eyelink", "Webcam")),
         item = factor(item),
         sub = factor(sub))

# write corpus_words_exp3$wordID into a text file, one line per word, no punctuation, same order as in the data frame

# remove punctuation and all lower case
words_for_espal <- gsub("[[:punct:]]", "", corpus_words_exp3$wordID) %>% tolower() %>% unique()

# write into two files since they are quite large
writeLines(words_for_espal, "LAB-SPAIN/data/words_for_espal.txt")

contrasts(target_words_exp3$Freq) <- c(0.5,-.5) #contr.sum(2)
contrasts(target_words_exp3$Tracker) <- c(0.5,-.5) #contr.sum(2)

contrasts(corpus_words_exp3$Tracker) <- c(0.5,-.5) #contr.sum(2)


summary(target_words_exp3)

# brms LMM for rt

priors_gaussian <- c(set_prior("normal(0,1)", class = "b"))


priors_beta <- c(set_prior("normal(0,250)", class = "b"),
                 set_prior("normal(0,1)", class = "b", dpar = "beta"))

lmm_target_words_FFD <- lmer(FFD ~ Freq * Tracker + (Tracker  | sub) + (Freq | item),
                         data = target_words_exp3)

summary(lmm_target_words_FFD)
# brms

blmm_target_FFD_exp3_exgaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      FFD ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item),
      beta ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_FFD_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_target_FFD_exp3_exgaussian.qs")

blmm_target_FFD_exp3_gaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      log(FFD) ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_FFD_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_target_FFD_exp3_gaussian.qs")

## GD

blmm_target_GD_exp3_exgaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      GD ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item),
      beta ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_GD_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_target_GD_exp3_exgaussian.qs")

blmm_target_GD_exp3_gaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      log(GD) ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_GD_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_target_GD_exp3_gaussian.qs")

blmm_target_SFD_exp3_exgaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      SFD ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item),
      beta ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_SFD_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_target_SFD_exp3_exgaussian.qs")

blmm_target_SFD_exp3_gaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      log(SFD) ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_SFD_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_target_SFD_exp3_gaussian.qs")

blmm_target_TVT_exp3_exgaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      TVT ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item),
      beta ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_TVT_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_target_TVT_exp3_exgaussian.qs")

blmm_target_TVT_exp3_gaussian <-
  brm(
    data = target_words_exp3,
    formula = bf(
      log(TVT) ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_target_TVT_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_target_TVT_exp3_gaussian.qs")

# Load all qs models in folder
#model_files <- list.files("LAB-SPAIN/models/brms/", pattern = "*.qs", full.names = TRUE)
#models <- lapply(model_files, qs_read)
#names(models) <- gsub("LAB-SPAIN/models/brms/|\\.qs$", "", model_files)


## corpus analysis

# add column indicating which word (from column word_num) is the final word in the sentence (contains a ".")
corpus_words_exp3 <- corpus_words_exp3 %>% group_by(sub, item) %>% mutate(position = case_when(grepl("\\.$", wordID) & word_num == max(word_num) ~ "last",
                                                                                               word_num == 1 ~ "first",
                                                                                               TRUE ~ "middle"),
                                                  # make a word code uniquely identifying each word, combining item and word_num
                                                  word_code = paste(item, word_num, sep = "_"),
                                                  len = nchar(wordID))

exp3_word_frequency_data <- read_tsv("LAB-SPAIN/spanish_corpus_words_with_frequencies_espal.txt") %>% select(word,frq,zipf,num_letters)

corpus_words_exp3 <- corpus_words_exp3 %>%
  left_join(exp3_word_frequency_data, by = c("wordID" = "word"))

contrasts(corpus_words_exp3$Tracker) <- c(-.5,.5)
ctr <- function(x) scale(x, scale = FALSE)

lmm_corpus_words_FFD <- lmer(FFD ~ ctr(zipf) * ctr(len) * Tracker + (1 | sub) + (1 | word_code),
                             # exclude first and last word in a sentence and fixations < 80 ms and >800 ms
                             data = corpus_words_exp3 %>% filter(position != "first" & position != "last")) #& FFD > 80 & FFD < 800))
summary(lmm_corpus_words_FFD)


# brms model for FFD

blmm_corpus_FFD_exp3_exgaussian <-
  brm(
    # exclu
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      FFD ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (Tracker | item),
      beta ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )


qs_save(blmm_corpus_FFD_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_corpus_FFD_exp3_exgaussian.qs")

# gaussian brms

blmm_corpus_FFD_exp3_gaussian <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(FFD) ~ ctr(zipf) * Tracker + (Tracker+ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )
qs_save(blmm_corpus_FFD_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_corpus_FFD_exp3_gaussian.qs") 

# GD

blmm_corpus_GD_exp3_exgaussian <-
  brm(
    data = corpus_words_exp3,
    formula = bf(
      GD ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (1 | item),
      beta ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (1 | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )



qs_save(blmm_corpus_GD_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_corpus_GD_exp3_exgaussian.qs")

# gaussian

blmm_corpus_GD_exp3_gaussian <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(GD) ~ ctr(zipf) * Tracker + (Tracker+ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_GD_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_corpus_GD_exp3_gaussian.qs")

# SFD

blmm_corpus_SFD_exp3_exgaussian <-
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      SFD ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (1 | item),
      beta ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (1 | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )



qs_save(blmm_corpus_SFD_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_corpus_SFD_exp3_exgaussian.qs")

# gaussian

blmm_corpus_SFD_exp3_gaussian <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(SFD) ~ ctr(zipf) * Tracker + (Tracker+ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_SFD_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_corpus_SFD_exp3_gaussian.qs")

# TVT

blmm_corpus_TVT_exp3_exgaussian <-
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      TVT ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (1 | item),
      beta ~ ctr(zipf) * Tracker  + (ctr(zipf) * Tracker | sub) + (1 | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_TVT_exp3_exgaussian, "LAB-SPAIN/models/brms/blmm_corpus_TVT_exp3_exgaussian.qs")

# gaussian

blmm_corpus_TVT_exp3_gaussian <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(TVT) ~ ctr(zipf) * Tracker + (Tracker+ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_TVT_exp3_gaussian, "LAB-SPAIN/models/brms/blmm_corpus_TVT_exp3_gaussian.qs")

# same models, but with word length (len) plus interaction with zipf and tracker

blmm_corpus_FFD_exp3_exgaussian_len <-
  brm(
    # exclude first and last words
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      FFD ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (Tracker | item),
      beta ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_FFD_exp3_exgaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_FFD_exp3_exgaussian_len.qs")
# gaussian

blmm_corpus_FFD_exp3_gaussian_len <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(FFD) ~ ctr(zipf) * ctr(len) * Tracker + (Tracker*ctr(len)*ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )
qs_save(blmm_corpus_FFD_exp3_gaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_FFD_exp3_gaussian_len.qs")

# GD, with len

blmm_corpus_GD_exp3_exgaussian_len <-
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      GD ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (1 | item),
      beta ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (1 | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_GD_exp3_exgaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_GD_exp3_exgaussian_len.qs")

#gaussian

blmm_corpus_GD_exp3_gaussian_len <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(GD) ~ ctr(zipf) * ctr(len) * Tracker + (Tracker*ctr(len)*ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_GD_exp3_gaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_GD_exp3_gaussian_len.qs")
# SFD, with len

blmm_corpus_SFD_exp3_exgaussian_len <-
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      SFD ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (1 | item),
      beta ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (1 | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_SFD_exp3_exgaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_SFD_exp3_exgaussian_len.qs")
# gaussian

blmm_corpus_SFD_exp3_gaussian_len <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(SFD) ~ ctr(zipf) * ctr(len) * Tracker + (Tracker*ctr(len)*ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_SFD_exp3_gaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_SFD_exp3_gaussian_len.qs")

# TVT, with len

blmm_corpus_TVT_exp3_exgaussian_len <-
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      TVT ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (1 | item),
      beta ~ ctr(zipf) * ctr(len) * Tracker  + (ctr(zipf) * ctr(len) * Tracker | sub) + (1 | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_beta,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_TVT_exp3_exgaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_TVT_exp3_exgaussian_len.qs")
#gaussian

blmm_corpus_TVT_exp3_gaussian_len <- 
  brm(
    data = corpus_words_exp3 %>% filter(position != "first" & position != "last"),
    formula = bf(
      log(TVT) ~ ctr(zipf) * ctr(len) * Tracker + (Tracker*ctr(len)*ctr(zipf)  | sub) + (Tracker | word_code)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    #init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )

qs_save(blmm_corpus_TVT_exp3_gaussian_len, "LAB-SPAIN/models/brms/blmm_corpus_TVT_exp3_gaussian_len.qs")

