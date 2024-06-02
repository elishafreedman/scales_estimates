# analysis on empirical data
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

sims_ham <- brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
                data = incidence_dataset$Incidence_H,
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

sims_spir <- brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
                 data = incidence_dataset$Incidence_S,
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

sims_rick_ins <- incidence_dataset$Incidence_R %>%
  subset(.$Class == "Insecta" & .$pool_size == 1) %>%
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
  subset(incidence_dataset$Incidence_S$Order == "Hemiptera" &
           incidence_dataset$Incidence_S$type != "Scaleinsect") %>%
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

#summarise draws

sims_card_sums <- posterior::summarise_draws(sims_card)
sims_rick_sums <- posterior::summarise_draws(sims_rick)
sims_wol_sums <- posterior::summarise_draws(sims_wol)
sims_spir_sums <- posterior::summarise_draws(sims_spir)
sims_ham_sums <- posterior::summarise_draws(sims_ham)
# plotting

# quick plot check
x <- seq(0, 1, 0.01)
plot(x = seq(0, 1, 0.01),
     y = dbeta(x, 0.27, 0.69))
plot(sims_wol)

# fancy thesis plots
calc_alpha <- function(b_int, phi){
  alpha <-  b_int * phi
  return(alpha)
}
calc_beta <- function(b_int, phi){
  beta <- (1 - b_int) * phi
  return(beta)
}
#make data frame
cols <- c("alpha", "beta", "mu", "phi")
all_spreads <- sims_wol %>%
  spread_draws(c(b_Intercept, phi)) %>%
  cbind(., data.frame("Symbiont" = rep("Wolbachia", nrow(.)),
                      "alpha" = calc_alpha(.$b_Intercept, .$phi),
                      "beta" = calc_beta(.$b_Intercept, .$phi))) %>%
  rbind(sims_rick %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Rickettsia", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%

  rbind(sims_card %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Cardinium", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rbind(sims_ham %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Hamiltonella", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%

  rbind(sims_spir %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Spiroplasma", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rename("mu" = b_Intercept) %>%
  pivot_longer(all_of(cols),
               names_to = "Parameter")

ggplot(all_spreads,
       aes(y = Symbiont,
           x = value))+
  theme_sets+
  stat_halfeye()+
  facet_grid(rows = vars(Parameter), scales = "free_x")+
  theme(axis.text = element_text(face = "italic"))+
  theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey"))


bayesplot::pp_check(sims_rick, nsamples = 20)+theme_bw()+xlim(c(0, 1))

rick_rep <- as.matrix(sims_rick, variable = "b_Intercept")
yrep <- posterior_predict(sims_rick)

pp_check(sims_rick$data$nInfected,
         yrep[1:50,], ppc_dens_overlay)

post <- as_draws_df(sims_wol)

head(post)
lines <-
  post %>%
  slice_sample(n = 1000) %>%
  expand_grid(x = seq(from = 0, to = 1, by = .001)) %>%
  mutate(density = dbeta(x, shape1 = calc_alpha(.$b_Intercept, .$phi),
                         shape2 = calc_beta(.$b_Intercept, .$phi)))

#estiamte infection function
alpha <- mean(calc_alpha(post$b_Intercept, post$phi))
beta <- mean(calc_beta(post$b_Intercept, post$phi))

est_infections <- function(x, posteriors, a, b ){
  dats <- data.frame(
    Species = x$Species,
    nSpecimens = x$nSpecimens,
    prob = rbeta(n = x$nSpecimens,
                 shape1 =a ,
                 shape2 = b), #the initial beta function
    nInfected = rep(NA, nrow(x))
  )
  Ns <- nrow(dats$Species)

  #get nInfected
  dats$nInfected <- rbinom(n = Ns,
                           size = as.numeric(dats$nSpecimens),
                           prob = as.numeric(dats$prob))
  return(dats)
}



est_infections(incidence_dataset$Incidence_W, post, a = alpha, b = beta)
lines %>%
  ggplot() +
  #pp_check
  geom_density(data = incidence_dataset$Incidence_W,
               mapping = aes(x = incidence_dataset$Incidence_W$nInfected/
                               incidence_dataset$Incidence_W$nSpecimens),
               stat = "density",
               fill = "snow4", color = "snow4", alpha = 0.5)+

  #infect_estimates
  geom_histogram(aes(x = ,
                     y = ))
#shapes
geom_line(aes(x = x, y = density, group = .draw),
          alpha = .2, color = "skyblue",
          linewidth = 1)+
  #mean
  stat_function(fun = dbeta,
                args = list(shape1 = mean(calc_alpha(post$b_Intercept,
                                                     post$phi)),
                            shape2 = mean(calc_beta(post$b_Intercept,
                                                    post$phi))),
                linewidth = 1, color = "deepskyblue4")+
  scale_y_continuous(limits = c(0, 3), expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  xlab("infection prevalence")+
  theme_sets












