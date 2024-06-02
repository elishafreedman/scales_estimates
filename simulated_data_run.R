# load real datasets
load("R:/scale_data_chapter_3/final_analysis/incidence_dataset.rda")

#simulate data
param_list <- list(
  arthropods = c("alpha" = 0.159,
                 "beta" = 0.384),
  insects = c("alpha" = 0.170,
              "beta" = 0.388),
  hemiptera = c("alpha" = 0.229,
                "beta" = 0.428),
  coccomorpha = c("alpha" = 0.535,
                  "beta" = 1.454)
)

# dataset sizes
##### testing incidence_dataset_sample_size #####
source("C:/Users/uqefreed/Documents/GitHub/scale_data/simulate_dataset.R")
# test using arthropod alpha and beta
species_list <-
  c(1 %o% 10 ^ (1:4)) #5 combinations exponentially increasing sample each incidence_dataset 100 times
resim <- 25
#10
#arthropod
param_arth_10 <- list(
  sim_details = c(
    param_list$arthropods[1],
    param_list$arthropods[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_arth_10$sims[[i]] <- make_sim_data(
    nSpecies = species_list[1],
    alpha = param_list$arthropods[1],
    beta = param_list$arthropods[2],
    sample_range = c(1, 500)
  )
}
#scale
param_scale_10 <- list(
  sim_details = c(
    param_list$coccomorpha[1],
    param_list$coccomorpha[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_scale_10$sims[[i]] <-
    make_sim_data(
      nSpecies = species_list[1],
      alpha = param_list$coccomorpha[1],
      beta = param_list$coccomorpha[2],
      sample_range = c(1, 500)
    )
}
#100
#arthropod
param_arth_100 <- list(
  sim_details = c(
    param_list$arthropods[1],
    param_list$arthropods[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_arth_100$sims[[i]] <-
    make_sim_data(
      nSpecies = species_list[2],
      alpha = param_list$arthropods[1],
      beta = param_list$arthropods[2],
      sample_range = c(1, 500)
    )
}

#scale
param_scale_100 <- list(
  sim_details = c(
    param_list$coccomorpha[1],
    param_list$coccomorpha[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_scale_100$sims[[i]] <-
    make_sim_data(
      nSpecies = species_list[2],
      alpha = param_list$coccomorpha[1],
      beta = param_list$coccomorpha[2],
      sample_range = c(1, 500)
    )
}

#1000

#arthropod
param_arth_1k <- list(
  sim_details = c(
    param_list$arthropods[1],
    param_list$arthropods[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_arth_1k$sims[[i]] <- make_sim_data(
    nSpecies = species_list[3],
    alpha = param_list$arthropods[1],
    beta = param_list$arthropods[2],
    sample_range = c(1, 500)
  )
}
#scale
param_scale_1k <- list(
  sim_details = c(
    param_list$coccomorpha[1],
    param_list$coccomorpha[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_scale_1k$sims[[i]] <-
    make_sim_data(
      nSpecies = species_list[3],
      alpha = param_list$coccomorpha[1],
      beta = param_list$coccomorpha[2],
      sample_range = c(1, 500)
    )
}
#10000
#arthropod
param_arth_5k <- list(
  sim_details = c(
    param_list$arthropods[1],
    param_list$arthropods[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_arth_5k$sims[[i]] <-
    make_sim_data(
      nSpecies = 5000,
      alpha = param_list$arthropods[1],
      beta = param_list$arthropods[2],
      sample_range = c(1, 500)
    )
}

#scale
param_scale_5k <- list(
  sim_details = c(
    param_list$coccomorpha[1],
    param_list$coccomorpha[2],
    sim_num = resim,
    max_sample_size = 500
  ),
  sims = list()
)
for (i in 1:resim) {
  param_scale_5k$sims[[i]] <-
    make_sim_data(
      nSpecies = 5000,
      alpha = param_list$coccomorpha[1],
      beta = param_list$coccomorpha[2],
      sample_range = c(1, 500)
    )
}
# real data simulations

#hamiltonella
#arthropod
Rsim_Ham_arth <- list(
  sim_details = c(param_list$arthropods[1],
                  param_list$arthropods[2],
                  sim_num = resim),
  make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_H[, "Species"],
    sample_range = incidence_dataset$Incidence_H[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
Rsim_Ham_scale <- list(
  sim_details = c(param_list$coccomorpha[1],
                  param_list$coccomorpha[2],
                  sim_num = resim),
  make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_H[, "Species"],
    sample_range = incidence_dataset$Incidence_H[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)

#ricekttsia
#arthropod
Rsim_Rick_arth <- list(
  sim_details = c(param_list$arthropods[1],
                  param_list$arthropods[2],
                  sim_num = resim),
  dataset =  make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_R[, "Species"],
    sample_range = incidence_dataset$Incidence_R[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
#scale
Rsim_Rick_scale <- list(
  sim_details = c(param_list$coccomorpha[1],
                  param_list$coccomorpha[2],
                  sim_num = resim),
  dataset = make_sim_data(
    alpha = param_list$coccomorpha["alpha"],
    beta = param_list$coccomorpha["beta"],
    nSpecies = incidence_dataset$Incidence_R[, "Species"],
    sample_range = incidence_dataset$Incidence_R[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
#spiroplasma
#arthropod
Rsim_Spir_arth <- list(
  sim_details = c(param_list$arthropods[1],
                  param_list$arthropods[2],
                  sim_num = resim),
  dataset = make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_S[, "Species"],
    sample_range = incidence_dataset$Incidence_S[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
#scale
Rsim_Spir_scale <- list(
  sim_details = c(param_list$coccomorpha[1],
                  param_list$coccomorpha[2],
                  sim_num = resim),
  make_sim_data(
    alpha = param_list$coccomorpha["alpha"],
    beta = param_list$coccomorpha["beta"],
    nSpecies = incidence_dataset$Incidence_S[, "Species"],
    sample_range = incidence_dataset$Incidence_S[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
#cardinium
#arthropod
Rsim_Card_arth <- list(
  sim_details = c(param_list$arthropods[1],
                  param_list$arthropods[2],
                  sim_num = resim),
  dataset = make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_C[, "Species"],
    sample_range = incidence_dataset$Incidence_C[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)

#scale
Rsim_Card_arth <- list(
  sim_details = c(param_list$coccomorpha[1],
                  param_list$coccomorpha[2],
                  sim_num = resim),
  make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_C[, "Species"],
    sample_range = incidence_dataset$Incidence_C[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
#wolbachia
#arthropod
Rsim_Wol_arth <- list(
  sim_details = c(
    param_list$arthropods[1],
    param_list$arthropods[2],
    sim_num = resim),
  dataset = make_sim_data(
    alpha = param_list$arthropods["alpha"],
    beta = param_list$arthropods["beta"],
    nSpecies = incidence_dataset$Incidence_W[, "Species"],
    sample_range = incidence_dataset$Incidence_W[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)
#scale
Rsim_Wol_arth <- list(
  sim_details = c(
    param_list$coccomorpha[1],
    param_list$coccomorpha[2],
    sim_num = resim),
  dataset = make_sim_data(
    alpha = param_list$coccomorpha["alpha"],
    beta = param_list$coccomorpha["beta"],
    nSpecies = incidence_dataset$Incidence_W[, "Species"],
    sample_range = incidence_dataset$Incidence_W[, "nSpecimens"],
    fixed_sample_size = TRUE
  )
)

save(param_arth_10, file = "param_arth_10.rda")
save(param_arth_100, file = "param_arth_100.rda")
save(param_arth_1k, file = "param_arth_1k.rda")


