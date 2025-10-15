library(tidyverse)
library(lme4)
library(lmerTest)
library(cmdstanr)
library(brms)
library(qs2)


target_words_exp1 <- read_csv("LAB/data/target_word_fixation_data.csv") %>%
  mutate(Freq = factor(Freq, levels = c("low", "high")),
         Tracker = factor(Tracker, levels = c("Webcam", "Eyelink")),
         item = factor(item),
         sub = factor(sub))

corpus_words_exp1 <- read_csv("LAB/data/corpus_fixation_data.csv") %>%
  mutate(Tracker = factor(Tracker, levels = c("Eyelink", "Webcam")),
         item = factor(item),
         sub = factor(sub))


contrasts(target_words_exp1$Freq) <- c(0.5,-.5) #contr.sum(2)
contrasts(target_words_exp1$Tracker) <- c(0.5,-.5) #contr.sum(2)

summary(target_words_exp1)

# brms LMM for rt

priors_gaussian <- c(set_prior("normal(0,1)", class = "b"))


priors_beta <- c(set_prior("normal(0,250)", class = "b"),
                 set_prior("normal(0,1)", class = "b", dpar = "beta"))

lmm_target_words_FFD <- lmer(FFD ~ Freq * Tracker + (Tracker  | sub) + (Freq | item),
                         data = target_words_exp1)

summary(lmm_target_words_FFD)
# brms

blmm_target_FFD_exp1_exgaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_FFD_exp1_exgaussian, "LAB/models/brms/blmm_target_FFD_exp1_exgaussian.qs")

blmm_target_FFD_exp1_gaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_FFD_exp1_gaussian, "LAB/models/brms/blmm_target_FFD_exp1_gaussian.qs")

## GD

blmm_target_GD_exp1_exgaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_GD_exp1_exgaussian, "LAB/models/brms/blmm_target_GD_exp1_exgaussian.qs")

blmm_target_GD_exp1_gaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_GD_exp1_gaussian, "LAB/models/brms/blmm_target_GD_exp1_gaussian.qs")

blmm_target_SFD_exp1_exgaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_SFD_exp1_exgaussian, "LAB/models/brms/blmm_target_SFD_exp1_exgaussian.qs")

blmm_target_SFD_exp1_gaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_SFD_exp1_gaussian, "LAB/models/brms/blmm_target_SFD_exp1_gaussian.qs")

blmm_target_TVT_exp1_exgaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_TVT_exp1_exgaussian, "LAB/models/brms/blmm_target_TVT_exp1_exgaussian.qs")

blmm_target_TVT_exp1_gaussian <-
  brm(
    data = target_words_exp1,
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

qs_save(blmm_target_TVT_exp1_gaussian, "LAB/models/brms/blmm_target_TVT_exp1_gaussian.qs")


## corpus analysis

# add column indicating which word (from column word_num) is the final word in the sentence (contains a ".")
corpus_words_exp1 <- corpus_words_exp1 %>% mutate(position = case_when(grepl("\\.$", wordID) & word_num == max(word_num) ~ "last",
                                                                       word_num == 1 ~ "first",
                                                                       TRUE ~ "middle"),
                                                  # make a word code uniquely identifying each word, combining item and word_num
                                                  word_code = paste(item, word_num, sep = "_"),
                                                  len = nchar(wordID))

contrasts(corpus_words_exp1$Tracker) <- c(-.5,.5)
ctr <- function(x) scale(x, scale = FALSE)

lmm_corpus_words_FFD <- lmer(FFD ~ ctr(zipf) * ctr(len) * Tracker + (1 | sub) + (1 | word_code),
                             # exclude first and last word in a sentence and fixations < 80 ms and >800 ms
                             data = corpus_words_exp1 %>% filter(position != "first" & position != "last")) #& FFD > 80 & FFD < 800))
summary(lmm_corpus_words_FFD)


# brms model for FFD

blmm_corpus_FFD_exp1_exgaussian <-
  brm(
    # exclu
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_FFD_exp1_exgaussian, "LAB/models/brms/blmm_corpus_FFD_exp1_exgaussian.qs")

# gaussian brms

blmm_corpus_FFD_exp1_gaussian <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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
qs_save(blmm_corpus_FFD_exp1_gaussian, "LAB/models/brms/blmm_corpus_FFD_exp1_gaussian.qs") 

# GD

blmm_corpus_GD_exp1_exgaussian <-
  brm(
    data = corpus_words_exp1,
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



qs_save(blmm_corpus_GD_exp1_exgaussian, "LAB/models/brms/blmm_corpus_GD_exp1_exgaussian.qs")

# gaussian

blmm_corpus_GD_exp1_gaussian <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_GD_exp1_gaussian, "LAB/models/brms/blmm_corpus_GD_exp1_gaussian.qs")

# SFD

blmm_corpus_SFD_exp1_exgaussian <-
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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



qs_save(blmm_corpus_SFD_exp1_exgaussian, "LAB/models/brms/blmm_corpus_SFD_exp1_exgaussian.qs")

# gaussian

blmm_corpus_SFD_exp1_gaussian <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_SFD_exp1_gaussian, "LAB/models/brms/blmm_corpus_SFD_exp1_gaussian.qs")

# TVT

blmm_corpus_TVT_exp1_exgaussian <-
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_TVT_exp1_exgaussian, "LAB/models/brms/blmm_corpus_TVT_exp1_exgaussian.qs")

# gaussian

blmm_corpus_TVT_exp1_gaussian <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_TVT_exp1_gaussian, "LAB/models/brms/blmm_corpus_TVT_exp1_gaussian.qs")

# same models, but with word length (len) plus interaction with zipf and tracker

blmm_corpus_FFD_exp1_exgaussian_len <-
  brm(
    # exclude first and last words
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_FFD_exp1_exgaussian_len, "LAB/models/brms/blmm_corpus_FFD_exp1_exgaussian_len.qs")
# gaussian

blmm_corpus_FFD_exp1_gaussian_len <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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
qs_save(blmm_corpus_FFD_exp1_gaussian_len, "LAB/models/brms/blmm_corpus_FFD_exp1_gaussian_len.qs")

# GD, with len

blmm_corpus_GD_exp1_exgaussian_len <-
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_GD_exp1_exgaussian_len, "LAB/models/brms/blmm_corpus_GD_exp1_exgaussian_len.qs")

#gaussian

blmm_corpus_GD_exp1_gaussian_len <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_GD_exp1_gaussian_len, "LAB/models/brms/blmm_corpus_GD_exp1_gaussian_len.qs")
# SFD, with len

blmm_corpus_SFD_exp1_exgaussian_len <-
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_SFD_exp1_exgaussian_len, "LAB/models/brms/blmm_corpus_SFD_exp1_exgaussian_len.qs")
# gaussian

blmm_corpus_SFD_exp1_gaussian_len <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_SFD_exp1_gaussian_len, "LAB/models/brms/blmm_corpus_SFD_exp1_gaussian_len.qs")

# TVT, with len

blmm_corpus_TVT_exp1_exgaussian_len <-
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_TVT_exp1_exgaussian_len, "LAB/models/brms/blmm_corpus_TVT_exp1_exgaussian_len.qs")
#gaussian

blmm_corpus_TVT_exp1_gaussian_len <- 
  brm(
    data = corpus_words_exp1 %>% filter(position != "first" & position != "last"),
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

qs_save(blmm_corpus_TVT_exp1_gaussian_len, "LAB/models/brms/blmm_corpus_TVT_exp1_gaussian_len.qs")


####### Report results for paper:

target_words_exp1_means_table <- target_words_exp1 %>%
  #filter(FFD > 80 & FFD < 1500) %>%
  group_by(Tracker, Freq) %>%
  summarise(
    mean_FFD = mean(FFD, na.rm = TRUE),
    sd_FFD = sd(FFD, na.rm = TRUE),
    mean_SFD = mean(SFD, na.rm = TRUE),
    sd_SFD = sd(SFD, na.rm = TRUE),
    mean_GD = mean(GD, na.rm = TRUE),
    sd_GD = sd(GD, na.rm = TRUE),
    mean_TVT = mean(TVT, na.rm = TRUE),
    sd_TVT = sd(TVT, na.rm = TRUE),
    #N = n() # N here is the number of correct trials within the RT filter
  ) %>%
  ungroup()


target_words_exp1_means_table[,3:10]<- round(target_words_exp1_means_table[,3:10])


# load the model results
blmm_target_FFD_exp1_gaussian <- qs_read(here("LAB", "models", "brms", "blmm_target_FFD_exp1_gaussian.qs"))
blmm_target_SFD_exp1_gaussian <- qs_read(here("LAB", "models", "brms", "blmm_target_SFD_exp1_gaussian.qs"))
blmm_target_GD_exp1_gaussian <- qs_read(here("LAB", "models", "brms", "blmm_target_GD_exp1_gaussian.qs"))
blmm_target_TVT_exp1_gaussian <- qs_read(here("LAB", "models", "brms", "blmm_target_TVT_exp1_gaussian.qs"))

summary(blmm_target_FFD_exp1_gaussian)

library(sjPlot)

tab_model(blmm_target_FFD_exp1_gaussian,
          blmm_target_SFD_exp1_gaussian,
          blmm_target_GD_exp1_gaussian,
          blmm_target_TVT_exp1_gaussian,
          show.stat = T,show.se = F, file="LAB/models/Freq_models.html",
          digits = 2, show.ngroups = T,
          digits.p = 2, digits.rsq = 2, digits.re = 2, show.est = T,
          transform = NULL, string.est = "Estimate", show.ci = 0.95)

#sjPlot::tab_model(blmm_target_FFD_exp1_gaussian)



library(tidybayes)
library(ggdist)

get_variables(blmm_target_FFD_exp1_gaussian)

Posterior1<-bm1 %>%
  spread_draws(b_Intercept, b_W1lenshort,b_W2lenshort, `b_W1lenshort:W2lenshort`) %>%
  pivot_longer(cols = 5:7, names_to = 'condition', values_to = 'value')%>%
  # mutate(condition_mean = b_Intercept + value) %>%
  mutate(condition= as.factor(condition),
         condition= fct_relevel(condition,"b_W1lenshort:W2lenshort",
                                "b_W2lenshort", "b_W1lenshort"))%>%
  ggplot(aes(y = condition, x = value)) +
  geom_vline(xintercept = 0, linetype=2)+
  stat_halfeye(color= 'darkred', alpha=0.75)+
  theme_bw(26)+
  scale_y_discrete(label = c('Word 1 length x\nWord 2 length',
                             'Word 2 length\n(short vs long)',
                             'Word 1 length\n(short vs long)'))+
  labs(x= "Regression slope estimate [b]", y= "Condition", 
       title = 'Return-sweep landing positions')+
  theme(plot.title = element_text(hjust = 0.5))#,
#axis.title.y=element_blank(),
#axis.text.y=element_blank(),
#axis.ticks.y=element_blank())


