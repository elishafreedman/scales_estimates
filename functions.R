#### Code associated with the following manuscript:
# Freedman, Cook, and Engelstaedter, 2024, "The prevalence of secondary symbionts across scale insects"

#### CREATE SIMULATED DATA
make_sim_data <- function(alpha = alpha,
                          beta = beta,
                          nSpecies = specs,
                          sample_range = NA,
                          fixed_sample_size = FALSE#vector with minimum and maximum sample sizes for each species
){ #column name to draw from
  
  
  if(fixed_sample_size == FALSE){
    sizes <- seq(sample_range[1],
                 sample_range[2], 1)
    nSpecs <- seq(1, nSpecies, 1)
    nSpecimens <- sample(sizes,
                         length(nSpecs),
                         replace = TRUE)
  } else{
    nSpecimens <- sample_range
    nSpecs <- nSpecies
  }
  dats <- data.frame(
    Species = nSpecs,
    nSpecimens = nSpecimens,
    prob = rbeta(nSpecimens,
                 alpha,
                 beta), #the initial beta function
    nInfected = rep(NA, length(nSpecs))
  )
  #get nInfected
  dats$nInfected <- rbinom(length(nSpecs),
                           size = as.numeric(dats$nSpecimens),
                           prob = as.numeric(dats$prob))
  return(dats)
}


#### GENERATE PRIOR DISTRIBUTIONS

#### POST PROCESSING DATA
# get diagnostics for each dataset
get_res_and_diag <- function(models) {
  averages <- list(
    av_b_intercept = data.frame(
      Variable = rep(NA, length(models)),
      mean = NA,
      median = NA,
      sd = NA,
      mad = NA,
      q5 = NA,
      q95 = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    ),
    av_phi = data.frame(
      Variable = rep(NA, length(models)),
      mean = NA,
      median = NA,
      sd = NA,
      mad = NA,
      q5 = NA,
      q95 = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    ),
    av_lprior = data.frame(
      Variable = rep(NA, length(models)),
      mean = NA,
      median = NA,
      sd = NA,
      mad = NA,
      q5 = NA,
      q95 = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    ),
    av_lp_ = data.frame(
      Variable = rep(NA, length(models)),
      mean = NA,
      median = NA,
      sd = NA,
      mad = NA,
      q5 = NA,
      q95 = NA,
      rhat = NA,
      ess_bulk = NA,
      ess_tail = NA
    )
  )
  
  for (i in 1:length(models)) {
    draw_sums <- posterior::summarise_draws(models[[i]])
    averages[["av_b_intercept"]][i, ] <- c(
      draw_sums[which(draw_sums$variable == "b_Intercept"), ]
    )
    averages[["av_phi"]][i, ] <- c(
      draw_sums[which(draw_sums$variable == "phi"), ]
    )
    averages[["av_lprior"]][i, ] <- c(
      draw_sums[which(draw_sums$variable == "lprior"), ]
    )
    averages[["av_lp_"]][i, ] <- c(
      draw_sums[which(draw_sums$variable == "lp__"), ]
    )
  }
  return(averages)
}


#### PLOTTING DATA
plot_all_distributions <- function(param_ests,
                                   df = TRUE,
                                   df_prov = NULL, ...) {
  if(is.null(df_prov)){
    est_data <- data.frame(run = seq(1, nrow(param_ests[[1]])),
                           mu = param_ests[["av_b_intercept"]]$mean,
                           phi = param_ests[["av_phi"]]$mean)
    data_to_plot <- list()
    for (i in 1:nrow(est_data)) {
      alpha <- est_data$mu[i] * est_data$phi[i]
      beta <- (1 - est_data$mu[i]) * est_data$phi[i]
      data_to_plot[[i]] <- data.frame(
        dataset = rep(est_data$run[i], 101),
        alpha = rep(alpha, 101),
        beta = rep(beta, 101),
        p = seq(0, 1, 0.01),
        db = dbeta(
          x = seq(0, 1, 0.01),
          shape1 = alpha,
          shape2 = beta
        )
      )
    }
    data_to_plot <- do.call("rbind", data_to_plot)
    
    #get the average distribution
    averages <- c(av_mu = mean(est_data$mu),
                  av_phi = mean(est_data$phi))
    av_alpha <- averages["av_mu"] * averages["av_mu"]
    av_beta <- (1 - averages["av_mu"]) * averages["av_phi"]
    
    av_at <- round(av_alpha, digits = 3)
    title <- (bquote(list(bar(alpha) == .(av_at),
                          bar(beta) == .(av_beta))))
    av_data_plot <- data.frame(p = seq(0, 1, 0.01),
                               db = dbeta(
                                 x = seq(0, 1, 0.01),
                                 shape1 = av_alpha,
                                 shape2 = av_beta
                               ))
    
    plots <- ggplot(data_to_plot) +
      geom_line(
        aes(
          x = p,
          y = db,
          color = as.character(dataset),
          alpha = 0.5
        ),
        linewidth = 1,
        show.legend = FALSE
      ) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(size = 15)
      ) +
      ylab("probability density") +
      xlab("infection probability")
    #ggtitle(bquote(.(title)))
  } else {
    plots <- ggplot(df_prov) +
      geom_line(
        aes(
          x = p,
          y = db,
          color = as.character(dataset),
          alpha = 0.5
        ),
        linewidth = 1,
        show.legend = FALSE
      ) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(size = 15)
      ) +
      ylab("probability density") +
      xlab("infection probability")
  }
  if (df == TRUE & is.null(df_prov)) {
    return(data_to_plot)
  } else {
    return(plots)
  }
}

