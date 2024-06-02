# run model on fully simulated data
#### arthropods ####
sims_arth_10 <- lapply(arth_priors, function(x)
  run_models(
    sims = param_arth_10,
    priors = x$priors$brm_prior,
    vars = x$priors$int_prior,
    cont = control,
    outpath = "R:/scale_data_chapter_3/final_analysis/sims_arth_10.rda"
  ))

sims <- c("wide", "medium", "narrow")
sizes <- c("10", "100", "1000")
res_arth_10 <- lapply(sims_arth_10, function(x)
  get_res_and_diag(x))


plot_arth_10 <- dplyr::bind_rows(lapply(seq_along(res_arth_10),
                                        function(x, y, i){
  dataset <- plot_all_distributions(x[[i]], df = TRUE)
  prior <- rep(y[i], nrow(dataset))
  sizes <- rep(sizes[1], nrow(dataset))
  cbind(prior, sizes, dataset)
},
x = res_arth_10,
y = sims
  ))

sims_arth_100 <- lapply(arth_priors, function(x)
  run_models(
    sims = param_arth_100,
    priors = x$priors$brm_prior,
    vars = x$priors$int_prior,
    cont = control,
    outpath = "R:/scale_data_chapter_3/final_analysis/sims_arth_100.rda"
  ))

res_arth_100 <- lapply(sims_arth_100, function(x)
  get_res_and_diag(x))
plot_arth_100 <- dplyr::bind_rows(lapply(seq_along(res_arth_100),
                                         function(x, y, i){
  dataset <- plot_all_distributions(x[[i]], df = TRUE)
  prior <- rep(y[i], nrow(dataset))
  sizes <- rep(sizes[2], nrow(dataset))
  cbind(prior, sizes, dataset)
},
x = res_arth_100,
y = sims
))


sims_arth_1k <- lapply(arth_priors, function(x)
  run_models(
    sims = param_arth_1k,
    priors = x$priors$brm_prior,
    vars = x$priors$int_prior,
    cont = control,
    outpath = "R:/scale_data_chapter_3/final_analysis/sims_arth_1k.rda"
  ))

res_arth_1k <- lapply(sims_arth_1k, function(x)
  get_res_and_diag(x))

plot_arth_1k <- dplyr::bind_rows(lapply(seq_along(res_arth_1k), function(x, y, i){
  dataset <- plot_all_distributions(x[[i]], df = TRUE)
  prior <- rep(y[i], nrow(dataset))
  sizes <- rep(sizes[3], nrow(dataset))
  cbind(prior, sizes, dataset)
},
x = res_arth_1k,
y = sims
))

# plot all

df_all_arth <- dplyr::bind_rows(plot_arth_10, plot_arth_100, plot_arth_1k)

all_arth_plot <- df_all_arth |>
  mutate(prior = forcats::fct_relevel(prior, c("wide", "medium", "narrow")))|>
  plot_all_distributions(df_prov = _)+
  scale_color_viridis_d(option = "rocket", begin = 0.5, end = 1)+
  geom_line(data = data,
                aes(x = d, y = db_arth),
                color = "blue", size = 1)+
  scale_x_continuous(sec.axis = sec_axis(~., name = "prior distribution",
                     breaks = NULL, labels = NULL))+
  scale_y_continuous(sec.axis = sec_axis(~., name = "# species",
                     breaks = NULL,
                     labels = NULL))+
  theme(legend.position = "none",
        strip.background = element_blank(),
        #strip.background = element_rect(fill = "black", colour = "black"),
        strip.text = element_text(size = 15, color = "black"),
        panel.spacing = unit(1, "lines"),
        axis.title.x.top = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 18)
        )+
  facet_grid(cols = vars(prior),
             rows = vars(sizes))

##### scale data ####

sims_scale_10 <- lapply(arth_priors, function(x)
  run_models(
    sims = param_scale_10,
    priors = x$priors$brm_prior,
    vars = x$priors$int_prior,
    cont = control,
    outpath = "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_scale_10.rda"
  ))

res_scale_10 <- lapply(sims_scale_10, function(x)
  get_res_and_diag(x))

plot_scale_10 <- dplyr::bind_rows(lapply(seq_along(res_scale_10), function(x, y, i){
  dataset <- plot_all_distributions(x[[i]], df = TRUE)
  prior <- rep(y[i], nrow(dataset))
  sizes <- rep(sizes[1], nrow(dataset))
  cbind(prior, sizes, dataset)
},
x = res_scale_10,
y = sims
))


sims_scale_100 <- lapply(arth_priors, function(x)
  run_models(
    sims = param_scale_100,
    priors = x$priors$brm_prior,
    vars = x$priors$int_prior,
    cont = control,
    outpath = "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_scale_100.rda"
  ))

res_scale_100 <- lapply(sims_scale_100, function(x)
  get_res_and_diag(x))

plot_scale_100 <- dplyr::bind_rows(
  lapply(seq_along(res_scale_100), function(x, y, i){
  dataset <- plot_all_distributions(x[[i]], df = TRUE)
  prior <- rep(y[i], nrow(dataset))
  sizes <- rep(sizes[2], nrow(dataset))
  cbind(prior, sizes, dataset)
},
x = res_scale_100,
y = sims
))

#sims_scale_1k <- lapply(arth_priors, function(x)
  sims_arth_1k_w <- run_models(
    sims = param_arth_1k,
    priors = arth_priors[[1]]$priors$brm_prior,
    vars = arth_priors[[1]]$priors$int_prior,
    cont = control
    #outpath = "R:/scale_data_chapter_3/final_analysis/sims_scale_1k.rda"
  #
  )
  beepr::beep()
  #)
  sims_arth_100_m <- run_models(
    sims = param_scale_100,
    priors = arth_priors[[2]]$priors$brm_prior,
    vars = arth_priors[[2]]$priors$int_prior,
                       cont = control
                       #outpath = "R:/scale_data_chapter_3/final_analysis/sims_scale_1k.rda"
                       )
  beepr::beep()
  sims_arth_1k_n <- run_models(
    sims = param_arth_1k,
    priors = arth_priors[[3]]$priors$brm_prior,
    vars = arth_priors[[3]]$priors$int_prior,
                       cont = control
                       #outpath = "R:/scale_data_chapter_3/final_analysis/sims_scale_1k.rda"
                       )
  beepr::beep()

sims_arth_100 <- list(wide = sims_arth_1k_w,
                      medium = sims_arth_1k_m,
                      narrow = sims_arth_1k_n)
save(sims_arth_1k, file = "sims_arth_1k.rda")

res_scale_1k <- lapply(sims_scale_1k, function(x)
  get_res_and_diag(x))

plot_scale_1k <- dplyr::bind_rows(lapply(seq_along(res_scale_1k), function(x, y, i){
  dataset <- plot_all_distributions(x[[i]], df = TRUE)
  prior <- rep(y[i], nrow(dataset))
  sizes <- rep(sizes[3], nrow(dataset))
  cbind(prior, sizes, dataset)
},
x = res_scale_1k,
y = sims
))
 df_all_scale <- dplyr::bind_rows(plot_scale_10, plot_scale_100, plot_scale_1k)
all_scale_plot <- df_all_scale |>
  mutate(prior = forcats::fct_relevel(prior, c("wide", "medium", "narrow")))|>
  plot_all_distributions(df_prov = _)+
  scale_color_viridis_d(option = "rocket", begin = 0.5, end = 1)+
  geom_line(data = data,
            aes(x = d, y = db_scale),
            color = "blue", size = 1)+
  scale_x_continuous(sec.axis = sec_axis(~., name = "prior distribution",
                                         breaks = NULL, labels = NULL))+
  scale_y_continuous(sec.axis = sec_axis(~., name = "# species",
                                         breaks = NULL,
                                         labels = NULL))+
  theme(legend.position = "none",
        strip.background = element_blank(),
        #strip.background = element_rect(fill = "black", colour = "black"),
        strip.text = element_text(size = 15, color = "black"),
        axis.title.x.top = element_text(face = "bold", size = 18),
        axis.title.y.right = element_text(face = "bold", size = 18),
        panel.spacing = unit(1, "lines")
  )+
  facet_grid(cols = vars(prior),
             rows = vars(sizes))
# sims real dataset

sims_card_run <- lapply(arth_priors, function(x)
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = Rsim_Card_arth[[2]],
      prior = x$priors$brm_prior,
      stanvars = x$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)
  )

save(sims_card_run,
     file =
       "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_card.rda")


sims_rick_run <- lapply(arth_priors, function(x)
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = Rsim_Rick_arth[[2]],
      prior = x$priors$brm_prior,
      stanvars = x$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)
)

save(sims_rick_run,
     file = "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_rick.rda")

sims_ham_run <- lapply(arth_priors, function(x)
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = Rsim_Ham_arth[[2]],
      prior = x$priors$brm_prior,
      stanvars = x$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)
)

save(sims_ham_run,
     file = "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_ham.rda")

Rsim_Spir_arth$dataset$nSpecimens <- as.numeric(Rsim_Spir_arth$dataset$nSpecimens)
sims_spir_run <- lapply(arth_priors, function(x)
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = Rsim_Spir_arth$dataset,
      prior = x$priors$brm_prior,
      stanvars = x$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)
)

save(sims_spir_run,
     file = "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_spir.rda")

sims_wol_run <- lapply(arth_priors, function(x)
  brm(nInfected | trials(nSpecimens) ~ 0 + Intercept,
      data = Rsim_Wol_arth$dataset,
      prior = x$priors$brm_prior,
      stanvars = x$priors$int_prior,
      family = beta_binomial(link = "identity",
                             link_phi = "identity"),
      iter = 5000,
      warmup = 2000,
      control = control,
      cores = 2)
)
save(sims_wol_run,
     file = "/Users/freed/Documents/PhD_local_files/Bayes_data_analysis/sims_wol.rda")

