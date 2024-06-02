#### empiracal data analysis
sapply(
  c("bayesplot",
    "brms",
    "tidybayes",
    "rstan",
    "tidyverse",
    "ggplot2",
    "posterior"),
  library,
  character.only = TRUE
)
# using wide arthropod priors
# convert relevant columns to integer
incidence_dataset$Incidence_H$nSpecimens <-
  as.integer(incidence_dataset$Incidence_H$nSpecimens)
incidence_dataset$Incidence_H$nInfected <-
  as.integer(incidence_dataset$Incidence_H$nInfected)

incidence_dataset$Incidence_C$nSpecimens <-
  as.integer(incidence_dataset$Incidence_C$nSpecimens)
incidence_dataset$Incidence_C$nInfected <-
  as.integer(incidence_dataset$Incidence_C$nInfected)

incidence_dataset$Incidence_W$nSpecimens <-
  as.integer(incidence_dataset$Incidence_W$nSpecimens)
incidence_dataset$Incidence_W$nInfected <-
  as.integer(incidence_dataset$Incidence_W$nInfected)

incidence_dataset$Incidence_S$nSpecimens <-
  as.integer(incidence_dataset$Incidence_S$nSpecimens)
incidence_dataset$Incidence_S$nInfected <-
  as.integer(incidence_dataset$Incidence_S$nInfected)

incidence_dataset$Incidence_R$nSpecimens <-
  as.integer(incidence_dataset$Incidence_R$nSpecimens)
incidence_dataset$Incidence_R$nInfected <-
  as.integer(incidence_dataset$Incidence_R$nInfected)

control <- list(
  adapt_engaged = TRUE,
  adapt_delta = 0.95, #increased from default of 0.8
  stepsize = 0.02 # 0.05 default
  #max_treedepth = 15
)

#### all ####

sims_card <- incidence_dataset$Incidence_C %>%
  subset(., .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_card,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_card.rda")


sims_rick <- incidence_dataset$Incidence_R %>%
  subset(., .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)


save(sims_rick,
     file = "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_rick.rda")

sims_ham <-incidence_dataset$Incidence_H %>%
  subset(., .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data =.,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)


save(sims_ham,
     file = "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_ham.rda")

sims_spir <- incidence_dataset$Incidence_S %>%
  subset(., .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data =.,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)


save(sims_spir,
     file = "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_spir.rda")

sims_wol <- incidence_dataset$Incidence_W %>%
  subset(., .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_wol,
     file = "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_wol.rda")

#### insecta ####
sims_card_ins <- incidence_dataset$Incidence_C %>%
  subset(.$Class == "Insecta" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_card_ins,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_card_ins.rda")

sims_ham_ins <- incidence_dataset$Incidence_H %>%
  subset(.$Class == "Insecta" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_ham_ins,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_ham_ins.rda")

sims_rick_ins <- incidence_dataset$Incidence_R %>%
  subset(.$Class == "Insecta" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_rick_ins,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_rick_ins.rda")

sims_spir_ins <- incidence_dataset$Incidence_S %>%
  subset(.$Class == "Insecta"& .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_spir_ins,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_spir_ins.rda")

sims_wol_ins <- incidence_dataset$Incidence_W %>%
  subset(.$Class == "Insecta" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_wol_ins,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_wol_ins.rda")

#### hemiptera ####
sims_card_hemip <- incidence_dataset$Incidence_C %>%
  subset(.$Order == "Hemiptera" &
           .$type != "Scaleinsect" & .$pool_size == 1
  ) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_card_hemip,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_card_hemip.rda")

sims_rick_hemip <- incidence_dataset$Incidence_R %>%
  subset(.$Order == "Hemiptera" &
           .$type != "Scaleinsect" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_rick_hemip,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_rick_hemip.rda")


sims_spir_hemip <- incidence_dataset$Incidence_S %>%
  subset(.$Order == "Hemiptera" &
           .$type != "Scaleinsect") %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_spir_hemip,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_spir_hemip.rda")

sims_wol_hemip <- incidence_dataset$Incidence_W %>%
  subset(.$Order == "Hemiptera" &
           .$type != "Scaleinsect" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_wol_hemip,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_wol_hemip.rda")

sims_ham_hemip <- incidence_dataset$Incidence_H %>%
  subset(incidence_dataset$Incidence_H$Order == "Hemiptera" &
           incidence_dataset$Incidence_H$type != "Scaleinsect" ) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_ham_hemip,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_ham_hemip.rda")

#### scale insects ####
sims_wol_scales <- incidence_dataset$Incidence_W %>%
  subset(.$type == "Scaleinsect" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_wol_scales,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_wol_scales.rda")

sims_rick_scales <- incidence_dataset$Incidence_R %>%
  subset(.$type == "Scaleinsect" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_rick_scales,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_rick_scales.rda")

sims_card_scales <- incidence_dataset$Incidence_C %>%
  subset(.$type == "Scaleinsect" & .$pool_size == 1) %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_card_scales,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_card_scales.rda")
sims_spir_scales <- incidence_dataset$Incidence_S %>%
  subset(incidence_dataset$Incidence_S$type == "Scaleinsect") %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_spir_scales,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_spir_scales.rda")
sims_ham_scales <- incidence_dataset$Incidence_H %>%
  subset(incidence_dataset$Incidence_H$type == "Scaleinsect") %>%
  group_by(Species) %>%
  summarise(nSpecimens = sum(nSpecimens), nInfected = sum(nInfected)) %>%
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = .,
      prior = arth_priors$wide$priors$brm_prior,
      stanvars = arth_priors$wide$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)

save(sims_ham_scales,
     file =
       "R:/scale_data_chapter_3/final_analysis/sup_rerun/sims_ham_scales.rda")






