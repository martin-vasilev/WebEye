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

# Corpus analysis

contrasts(corpus_words_exp1$Tracker) <- c(-.5,.5)
ctr <- function(x) scale(x, scale = FALSE)

lmm_corpus_words_FFD <- lmer(FFD ~ ctr(zipf) * Tracker + (Tracker+zipf  | sub) + (1 | item),
                         data = corpus_words_exp1)
summary(lmm_corpus_words_FFD)
