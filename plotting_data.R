#### plot empiracal data #####
# plotting theme
theme_sets <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    title = element_text(size = 22),
    legend.title = element_text(size = 15),
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    panel.spacing = unit(1, "lines"),
    legend.text = element_text(size = 12)
  )
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
##### posterior summary table #####

##### plot posterior distribution ####
cols <- c("alpha", "beta", "mu", "phi")
# all samples

all_spreads_all_ins <- sims_wol %>%
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

# hemiptera
all_spreads_hemip <- sims_wol_hemip %>%
  spread_draws(c(b_Intercept, phi)) %>%
  cbind(., data.frame("Symbiont" = rep("Wolbachia", nrow(.)),
                      "alpha" = calc_alpha(.$b_Intercept, .$phi),
                      "beta" = calc_beta(.$b_Intercept, .$phi))) %>%
  rbind(sims_rick_hemip %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Rickettsia", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  
  rbind(sims_card_hemip %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Cardinium", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rbind(sims_ham_hemip %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Hamiltonella", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  
  rbind(sims_spir_hemip %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Spiroplasma", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rename("mu" = b_Intercept) %>%
  pivot_longer(all_of(cols),
               names_to = "Parameter")

# insects
all_spreads_ins <- sims_wol_ins %>%
  spread_draws(c(b_Intercept, phi)) %>%
  cbind(., data.frame("Symbiont" = rep("Wolbachia", nrow(.)),
                      "alpha" = calc_alpha(.$b_Intercept, .$phi),
                      "beta" = calc_beta(.$b_Intercept, .$phi))) %>%
  rbind(sims_rick_ins %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Rickettsia", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  
  rbind(sims_card_ins %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Cardinium", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rbind(sims_ham_ins %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Hamiltonella", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  
  rbind(sims_spir_ins %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Spiroplasma", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rename("mu" = b_Intercept) %>%
  pivot_longer(all_of(cols),
               names_to = "Parameter")

# scales
all_spreads_scales <- sims_wol_scales %>%
  spread_draws(c(b_Intercept, phi)) %>%
  cbind(., data.frame("Symbiont" = rep("Wolbachia", nrow(.)),
                      "alpha" = calc_alpha(.$b_Intercept, .$phi),
                      "beta" = calc_beta(.$b_Intercept, .$phi))) %>%
  rbind(sims_rick_scales %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Rickettsia", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  
  rbind(sims_card_scales %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Cardinium", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rbind(sims_ham_scales %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Hamiltonella", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  
  rbind(sims_spir_scales %>%
          spread_draws(c(b_Intercept, phi)) %>%
          cbind(., data.frame("Symbiont" = rep("Spiroplasma", nrow(.)),
                              "alpha" = calc_alpha(.$b_Intercept, .$phi),
                              "beta" = calc_beta(.$b_Intercept, .$phi)))) %>%
  rename("mu" = b_Intercept) %>%
  pivot_longer(all_of(cols),
               names_to = "Parameter")

scales_post <- ggplot(all_spreads_hemip,
                      aes(y = Symbiont,
                          x = value))+
  theme_sets+
  stat_halfeye(fill = "skyblue")+
  facet_grid(cols = vars(Parameter), scales = "free_x")+
  theme(axis.text = element_text(face = "italic"))+
  theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey"))+
  ggtitle("Infraorder Coccomorpha")

ggarrange(all_post, ins_post, hemip_post, scales_post, labels = LETTERS[1:4], ncol = 1)



# facet grid

all_spreads_all_ins$level <- rep("Phylum Arthropoda", nrow(all_spreads_all_ins))
all_spreads_ins$level <- rep("Class Insecta", nrow(all_spreads_ins))
all_spreads_hemip$level <- rep("Order Hemiptera", nrow(all_spreads_hemip))
all_spreads_scales$level <- rep("Infraorder Coccomorpha", nrow(all_spreads_scales))

combined_spreads <- rbind(all_spreads_all_ins,
                          all_spreads_ins,
                          all_spreads_hemip,
                          all_spreads_scales)
combined_spreads %>% mutate(level = forcats::fct_relevel(level,
                                                         c("Phylum Arthropoda",
                                                           "Class Insecta",
                                                           "Order Hemiptera",
                                                           "Infraorder Coccomoprha"))) %>%
  ggplot(.,
         aes(y = Symbiont,
             x = value))+
  theme_sets+
  stat_halfeye(fill = "skyblue", color = "black", linewidth = 0.5, point_size = 2)+
  facet_grid(cols = vars(Parameter), rows = vars(level), scales = "free_x")+
  theme(axis.text = element_text(face = "italic"),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        strip.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        axis.ticks.length.x = unit(0.25,"cm"))+
  theme(panel.grid.major.y = element_line(linewidth = 0.5, color = "grey"))


bayesplot::pp_check(sims_rick, nsamples = 20)+theme_bw()+xlim(c(0, 1))

rick_rep <- as.matrix(sims_rick, variable = "b_Intercept")
yrep <- posterior_predict(sims_rick)

post <- as_draws_df(sims_wol)

pp_check(sims_rick$data$nInfected,
         yrep[1:50,], ppc_dens_overlay)


##### posterior predictive checks ####

all_sims_arth <- list(Wolbachia = sims_wol,
                      Cardinium = sims_card,
                      Rickettsia = sims_rick,
                      Spiroplasma = sims_spir,
                      Hamiltonella = sims_ham)

all_sims_ins <- list(Wolbachia =sims_wol_ins,
                     Cardinium =  sims_card_ins,
                     Rickettsia = sims_rick_ins,
                     Spiroplasma =  sims_spir_ins,
                     Hamiltonella = sims_ham_ins)

all_sims_hemip <- list(Wolbachia = sims_wol_hemip,
                       Cardinium = sims_card_hemip,
                       Rickettsia = sims_rick_hemip,
                       Spiroplasma = sims_spir_hemip,
                       Hamiltonella = sims_ham_hemip)

all_sims_scales <- list(Wolbachia = sims_wol_scales,
                        Cardinium = sims_card_scales,
                        Rickettsia = sims_rick_scales,
                        Spiroplasma = sims_spir_scales,
                        Hamiltonella = sims_ham_scales)

all_sims_wol <- list("Phylum Arthropoda" = sims_wol,
                     "Class Insecta" = sims_wol_ins,
                     "Order Hemiptera" = sims_wol_hemip,
                     "Infraorder Coccomorpha" = sims_wol_scales)

all_sims_card <- list("Phylum Arthropoda" = sims_card,
                      "Class Insecta" = sims_card_ins,
                      "Order Hemiptera" = sims_card_hemip,
                      "Infraorder Coccomorpha" = sims_card_scales)
all_sims_ham <- list("Phylum Arthropoda" = sims_ham,
                     "Class Insecta" = sims_ham_ins,
                     "Order Hemiptera" = sims_ham_hemip,
                     "Infraorder Coccomorpha" = sims_ham_scales)
all_sims_spir <- list("Phylum Arthropoda" = sims_spir,
                      "Class Insecta" = sims_spir_ins,
                      "Order Hemiptera" = sims_spir_hemip,
                      "Infraorder Coccomorpha" = sims_spir_scales)
all_sims_rick <- list("Phylum Arthropoda" = sims_rick,
                      "Class Insecta" = sims_rick_ins,
                      "Order Hemiptera" = sims_rick_hemip,
                      "Infraorder Coccomorpha" = sims_rick_scales)


#summarise draws
symbs <- c("Wolbachia", "Cardinium", "Rickettsia", "Spiroplasma", "Hamiltonella")
taxon_rank <- c("Phylum Arthropoda",
                "Class Insecta",
                "Order Hemiptera",
                "Infraorder Coccomorpha")
sims_sums_wol <- all_sims_wol %>%
  lapply(seq_along(.),
         function(x, i, j) {
           p <- posterior::summarise_draws(x[[i]])
           type <- rep(j[i], nrow(p))
           return(cbind(type, p))
         },
         x = .,
         j = taxon_rank) %>%
  bind_rows()

sums_wol <- lapply(all_sims_wol,
                   function(x)
                     posterior::summarise_draws(x))
# make

make_plot_data <- function(x){
  names <- names(x)
  lines <- lapply(seq_along(x), function(k, i, j) {
    post <- as_draws_df(x[[i]]) %>%
      slice_sample(n = 1000) %>%
      expand_grid(X = seq(from = 0, to = 1, by = .001)) %>%
      mutate(density = dbeta(X,
                             shape1 = calc_alpha(.$b_Intercept, .$phi),
                             shape2 = calc_beta(.$b_Intercept, .$phi)))
    
    type <- data.frame(type = rep(j[i], nrow(post)))
    post <- cbind(type, post)
    print(head(post))
    
    return(post)
  },
  k = .,
  j = names
  )
  dframes <- bind_rows(lines)
  return(dframes)
}

plots_rick <- make_plot_data(lines_rick)



#estiamte infection function
alpha <- mean(calc_alpha(post$b_Intercept, post$phi))
beta <- mean(calc_beta(post$b_Intercept, post$phi))

est_infections <- function(x, posteriors, a, b){
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

est_wol_hemip <- incidence_dataset$Incidence_W %>%
  subset(.$Order == "Hemiptera" &
           .$type != "Scaleinsect" & .$pool_size == 1) %>%
  est_infections(., post, a = alpha, b = beta)


lines_arth %>%
  
  
  ggplot() +
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



#### plot prior distributions #### 
#packages
sapply(
  c(
    "ggplot2",
    "tidyverse",
    "egg"
  ),
  library,
  character.only = TRUE
)

# gamma dists

rgams_arth <- lapply(arth_priors, function(x)
  rgamma(n = 1000, shape = x$params["alphaG"],
         rate = x$params["betaG"]))

rgams_scale <- lapply(scale_priors, function(x)
  rgamma( n = 1000, shape = x$params["alphaG"],
          rate = x$params["betaG"]))


#generate data
arth_names <- names(arth_priors)
dataset_arth <- dplyr::bind_rows(lapply(seq_along(arth_priors),
                                        function(t, y, k,  i)
                                          data.frame(
                                            dist = rep("Phylum Arthropoda",
                                                       length(k[[i]])),
                                            x = k[[i]],
                                            type = rep(y[i], length(k[[i]])),
                                            data = dgamma(x = k[[i]],
                                                          shape = t[[i]]$params["alphaG"],
                                                          rate = t[[i]]$params["betaG"])
                                          ),
                                        t = arth_priors,
                                        y = arth_names,
                                        k = rgams_arth
)
)

dataset_scale <- dplyr::bind_rows(lapply(seq_along(scale_priors),
                                         function(t, y, k,  i)
                                           data.frame(
                                             dist = rep("Superfamily Coccomorpha",
                                                        length(k[[i]])),
                                             x = k[[i]],
                                             type = rep(y[i], length(k[[i]])),
                                             data = dgamma(x = k[[i]],
                                                           shape = t[[i]]$params["alphaG"],
                                                           rate = t[[i]]$params["betaG"])
                                           ),
                                         t = scale_priors,
                                         y = arth_names,
                                         k = rgams_scale
)
)
#combined
theme_sets <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    title = element_text(size = 22),
    legend.title = element_text(size = 15),
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    panel.spacing = unit(1, "lines"),
    legend.text = element_text(size = 12)
  )

data_comb <- dplyr::bind_rows(dataset_arth,
                              dataset_scale)
hlines <- data.frame(dist = c("Phylum Arthropoda",
                              "Superfamily Coccomorpha"),
                     param = c(arth_priors$wide$params["mu"],
                               scale_priors$wide$params["mu"]))
data_comb |>
  mutate(type = forcats::fct_relevel(type, c("wide", "medium", "narrow")))|>
  ggplot() +
  facet_grid(rows = vars(dist))+
  geom_line(aes(x = x,
                y = data,
                color = type), linewidth = 1) +
  geom_vline(data = hlines,
             aes(xintercept = param),
             linetype = "longdash",
             linewidth = 1,
             colour = "black"
  ) +
  xlab(expression(mu)) +
  ylab("probability density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50),
                     breaks = seq(0, 50, 10))+
  scale_x_continuous(limits = c(0.1, 0.8),
                     expand = c(0, 0),
                     breaks = seq(0.1, 0.8, 0.05))+
  scale_color_manual(values = c("skyblue2", "dodgerblue3", "navy"))+
  theme_sets


#arth only
dataset_arth |>
  mutate(type = forcats::fct_relevel(type, c("wide", "medium", "narrow")))|>
  ggplot() +
  geom_line(aes(x = x,
                y = data,
                color = type), linewidth = 1) +
  scale_color_manual(name = "variance",
                     values = c("skyblue", "dodgerblue3", "navy"))+
  ggnewscale::new_scale_color() +
  geom_vline(data = hlines,
             aes(xintercept = param, colour = dist),
             linetype = "dashed",
             linewidth = 1
             
  )+
  scale_color_manual(name = "parameter estimates",
                     values = c("blue", "firebrick"))+
  xlab(expression(mu)) +
  ylab("probability density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 45),
                     breaks = seq(0, 45, 15))+
  scale_x_continuous(limits = c(0.05, 0.8),
                     breaks = seq(0.05, 0.8, 0.15))+
  theme_sets


#phi

phi_data <-
  ggplot(dataset_phi) +
  geom_line(aes(x = x,
                y = data), linewidth = 1) +
  geom_vline(
    xintercept = phi,
    linetype = "longdash",
    linewidth = 1,
    colour = "violetred"
  ) +
  xlab(expression(phi)) +
  ylab("probability density") +
  #scale_y_continuous(expand = c(0, 0))+
  #scale_x_continuous(expand = c(0, 0))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 22)
  )


##initial 


d <- seq(0, 1, 0.01)
db_arth<-   dbeta(seq(0, 1, 0.01),
                  shape1 = param_list$arthropods[[1]],
                  shape2 = param_list$arthropods[[2]])
db_scale <- dbeta(seq(0, 1, 0.01),
                  shape1 = param_list$coccomorpha[[1]],
                  shape2 = param_list$coccomorpha[[2]])
data <- as.data.frame(cbind(d, db_arth, db_scale))

piv <- c("db_arth", "db_scale")
#plot_distribution
data |>
  pivot_longer(all_of(piv),
               names_to = "dists") |>
  ggplot()+
  geom_line(aes(x = d, y = value, colour = dists), size = 1)+
  xlab("infection prevalence")+
  ylab("probability density") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    title = element_text(size = 15),
    legend.text =element_text(size = 12)
  )+
  scale_colour_manual(name = "taxonomic rank",
                      values = c("blue", "firebrick"),
                      labels = c("Phylum Arthropoda",
                                 "Superfamily Coccomorpha"))



