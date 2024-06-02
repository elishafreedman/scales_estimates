# plot prior distributions
#packages
sapply(
  c(
    "ggplot2",
    "tidyverse",
    "gtable"
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

#fancy thesis plot

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
phi_x <- rexp(1000, rate = 1)
phi_data <- data.frame(x = phi_x,
                       dist = dexp(phi_x, rate = 1))
  ggplot(phi_data) +
  geom_line(aes(x = x,
                y = dist), color = "deepskyblue4", linewidth = 1) +
  xlab(expression(phi)) +
  ylab("probability density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    title = element_text(size = 22)
  )

##initial ehsan parameters plot


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
   xlab("proportion infected")+
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

# final distribution for data analysis plot


mu_dist <-  dataset_arth |>
   subset(dataset_arth$type == "wide")|>
   ggplot()+
   geom_area(aes(x = x,
                 y = data,
                 color = type),
             linewidth = 1,
             fill = "dodgerblue3",
             alpha = 0.5,
             color = "dodgerblue3")+
   ggnewscale::new_scale_color() +
     theme_bw() +
     theme(
       panel.grid = element_blank(),
       axis.text = element_text(size = 15),
       axis.title = element_text(size = 15),
       title = element_text(size = 15),
       legend.text =element_text(size = 12)
     )+
   ylab("probability density")+
   xlab(expression(mu))+
   scale_y_continuous(expand = c(0, 0), limits = c(0, 5),
                      breaks = seq(0, 5, 1))+
   scale_x_continuous(limits = c(0.05, 0.8),
                      breaks = seq(0.05, 0.8, 0.15))

 phi_dist <- ggplot(phi_data) +
   geom_area(aes(x = x,
                 y = dist),
             color = "dodgerblue3",
             fill = "dodgerblue3",
             alpha = 0.5,
             linewidth = 1) +
   xlab(expression(phi)) +
   ylab("probability density") +
   scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
   scale_x_continuous(expand = c(0, 0), limits = c(0, 8))+
   theme_bw() +
   theme(
     panel.grid = element_blank(),
     axis.text = element_text(size = 15),
     axis.title = element_text(size = 15),
     title = element_text(size = 22),
   )

  ggpubr::ggarrange(mu_dist, phi_dist, nrow = 1, ncol = 2)

