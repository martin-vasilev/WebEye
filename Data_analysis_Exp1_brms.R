library(tidyverse)
library(lme4)
library(lmerTest)
library(cmdstanr)
library(brms)


target_words_exp1 <- read_csv("LAB/data/target_word_fixation_data.csv") %>%
  mutate(Freq = factor(Freq, levels = c("low", "high")),
         Tracker = factor(Tracker, levels = c("Webcam", "Eyelink")),
         item = factor(item),
         sub = factor(sub))

corpus_words_exp1 <- read_csv("LAB/data/corpus_fixation_data.csv") %>%
  mutate(Tracker = factor(Tracker, levels = c("Eyelink", "Webcam")),
         item = factor(item),
         sub = factor(sub))


contrasts(target_words_exp1$Freq) <- contr.sum(2)
contrasts(target_words_exp1$Tracker) <- contr.sum(2)

summary(target_words_exp1)

# brms LMM for rt

priors_gaussian <- c(set_prior("normal(0,1)", class = "b"))


priors_beta <- c(set_prior("normal(0,250)", class = "b"),
                 set_prior("normal(0,1)", class = "b", dpar = "beta"))

lmm_target_words_FFD <- lmer(FFD ~ Freq * Tracker + (Tracker  | sub) + (Freq | item),
                         data = target_words_exp1)

summary(lmm_target_words_FFD)

# brms


blmm_target_FFD_exp1 <-
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



blmm_target_FFD_exp1_gaussian <-
  brm(
    data = target_words_exp1,
    formula = bf(
      FFD ~ Freq * Tracker  + (Freq * Tracker | sub) + (Freq * Tracker | item)),
    warmup = 1000,
    iter = 5000,
    chains = 4,
    prior = priors_gaussian,
    sample_prior = "yes",
    family = gaussian(),
    init = "0",
    control = list(adapt_delta = 0.8),#, max_treedepth = 15),
    cores = 4,
    backend = "cmdstanr",
    threads = threading(2),
    silent = 0
  )


