require(incidence)
require(coarseDataTools)
require(coda)
nd <- read.table("./dados/nishi_si_table.txt", header = TRUE)
nishi_si <- read.table("./dados/nishi_si_posterior.txt", header = TRUE)

estimate.R0.cori <- function(novos.casos, day0 = NA, delay=7, method, parameter.table, 
                             p.distribution, bayes.control, si.data, si.sample, modified = FALSE, n2 = NA, ...) {

  if(is.na(day0)){
    mdif = mean(si.data$SL - si.data$EL) # empirical SI
    day0= min(which(cumsum(novos.casos) > 12 & 
                      1:length(novos.casos) > ceiling(mdif) 
                    & 1:length(novos.casos) > delay)) 
    
  }
  
  if(method == "uncertain_si")  {
    
    parameter.list  <- lapply(parameter.table, function(x) x=x)
    parameter.list$si_parametric_distr = p.distribution 
    parameter.list$t_start = seq(day0,length(novos.casos)-delay)
    parameter.list$t_end = seq(delay+day0,length(novos.casos))
    
    config <- make_config(parameter.list)
    return(estimate_R(novos.casos, method = method, config = config))
  }
  
  if(method == "parametric_si"){
    
    parameter.list  <- lapply(parameter.table, function(x) x=x)
    parameter.list$si_parametric_distr =  p.distribution
    parameter.list$t_start = seq(day0,length(novos.casos)-delay)
    parameter.list$t_end = seq(delay+day0,length(novos.casos))
    
    config <- make_config(parameter.list)
    return(estimate_R(novos.casos, method = "parametric_si", config = config))
  }
  
  if (method == "si_from_data") {
  config <- make_config(incid = novos.casos, 
                        method = method, 
                        list(si_parametric_distr = p.distribution,
                             mcmc_control = make_mcmc_control(burnin = bayes_control$burnin, 
                                                              thin = bayes_control$thin),
                             mean_prior = 5,
                             std_prior = 5,
                             n1 = bayes_control$n1, 
                             n2 = bayes_control$n2,
                             t_start = seq(day0,length(novos.casos)-delay),
                             t_end = seq(delay+day0,length(novos.casos))))
  
  return(estimate_R(novos.casos, method = method, 
                    si_data = si.data, config = config))
  }
  
  if(method == "si_from_sample"){
    
    parameter.list  <- list()
    parameter.list$t_start = seq(day0,length(novos.casos)-delay)
    parameter.list$t_end = seq(delay+day0,length(novos.casos))
    
    if(is.na(n2)){
    parameter.list$n2 = 50 } else {
    parameter.list$n2 = n2  
    }
    
    config <- make_config(parameter.list)
    
    if(modified == FALSE){
    return(estimate_R(novos.casos, method = method,
                                   si_sample = si.sample, 
                                   config = config)) } else {
    return(estimate_R_modified(novos.casos, method = method,
                                   si_sample = si.sample, 
                                   config = config))
                                   }
    
    
  }
  
}

estimate_R_modified <- function(incid,
                       method = c(
                         "non_parametric_si", "parametric_si",
                         "uncertain_si", "si_from_data",
                         "si_from_sample"
                       ),
                       si_data = NULL,
                       si_sample = NULL,
                       config = make_config(incid = incid, method = method)) {
  
  method <- match.arg(method)
  config <- make_config(incid = incid, method = method, config = config)
  config <- EpiEstim:::process_config(config)
  EpiEstim:::check_config(config, method)
  
  if (method == "si_from_data") {
    ## Warning if the expected set of parameters is not adequate
    si_data <- EpiEstim:::process_si_data(si_data)
    config <- EpiEstim:::process_config_si_from_data(config, si_data)
    
    ## estimate serial interval from serial interval data first
    if (!is.null(config$mcmc_control$seed)) {
      cdt <- dic.fit.mcmc(
        dat = si_data,
        dist = config$si_parametric_distr,
        burnin = config$mcmc_control$burnin,
        n.samples = config$n1 * config$mcmc_control$thin,
        init.pars = config$mcmc_control$init_pars,
        seed = config$mcmc_control$seed
      )
    } else {
      cdt <- dic.fit.mcmc(
        dat = si_data,
        dist = config$si_parametric_distr,
        burnin = config$mcmc_control$burnin,
        n.samples = config$n1 * config$mcmc_control$thin,
        init.pars = config$mcmc_control$init_pars
      )
    }
    
    ## check convergence of the MCMC and print warning if not converged
    MCMC_conv <- check_cdt_samples_convergence(cdt@samples)
    
    ## thin the chain, and turn the two parameters of the SI distribution into a
    ## whole discrete distribution
    c2e <- coarse2estim(cdt, thin = config$mcmc_control$thin)
    
    cat(paste(
      "\n\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@",
      "\nEstimating the reproduction number for these serial interval",
      "estimates...\n",
      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    ))
    
    ## then estimate R for these serial intervals
    
    if (!is.null(config$seed)) {
      set.seed(config$seed)
    }
    
    out <- estimate_R_func(
      incid = incid,
      method = "si_from_data",
      si_sample = c2e$si_sample,
      config = config
    )
    out[["MCMC_converged"]] <- MCMC_conv
  } else {
    if (!is.null(config$seed)) {
      set.seed(config$seed)
    }
    
    out <- estimate_R_func(
      incid = incid, method = method, si_sample = si_sample,
      config = config
    )
  }
  return(out)
}

##########################################################
## estimate_R_func: Doing the heavy work in estimate_R  ##
##########################################################

#'
#' @importFrom stats median qgamma quantile rnorm sd
#'
#' @importFrom incidence as.incidence
#'
estimate_R_func <- function(incid,
                            si_sample,
                            method = c(
                              "non_parametric_si", "parametric_si",
                              "uncertain_si", "si_from_data", "si_from_sample"
                            ),
                            config) {
  
  #########################################################
  # Calculates the cumulative incidence over time steps   #
  #########################################################
  
  calc_incidence_per_time_step <- function(incid, t_start, t_end) {
    nb_time_periods <- length(t_start)
    incidence_per_time_step <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(i) 
      sum(incid[seq(t_start[i], t_end[i]), c("local", "imported")]))
    return(incidence_per_time_step)
  }
  
  #########################################################
  # Calculates the parameters of the Gamma posterior      #
  # distribution from the discrete SI distribution        #
  #########################################################
  
  posterior_from_si_distr <- function(incid, si_distr, a_prior, b_prior,
                                      t_start, t_end) {
    nb_time_periods <- length(t_start)
    lambda <- overall_infectivity(incid, si_distr)
    final_mean_si <- sum(si_distr * (seq(0, length(si_distr) -
                                           1)))
    a_posterior <- vector()
    b_posterior <- vector()
    a_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      a_prior + sum(incid[seq(t_start[t], t_end[t]), "local"]) 
      ## only counting local cases on the "numerator"
    }
    else {
      NA
    })
    b_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      1 / (1 / b_prior + sum(lambda[seq(t_start[t], t_end[t])]))
    }
    else {
      NA
    })
    return(list(a_posterior, b_posterior))
  }
  
  #########################################################
  # Samples from the Gamma posterior distribution for a   #
  # given mean SI and std SI                              #
  #########################################################
  
  sample_from_posterior <- function(sample_size, incid, mean_si, std_si,
                                    si_distr = NULL,
                                    a_prior, b_prior, t_start, t_end) {
    nb_time_periods <- length(t_start)
    
    if (is.null(si_distr)) {
      si_distr <- discr_si(seq(0, T - 1), mean_si, std_si)
    }
    
    final_mean_si <- sum(si_distr * (seq(0, length(si_distr) -
                                           1)))
    lambda <- overall_infectivity(incid, si_distr)
    a_posterior <- vector()
    b_posterior <- vector()
    a_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      a_prior + sum(incid[seq(t_start[t], t_end[t]), "local"]) 
      ## only counting local cases on the "numerator"
    }
    else {
      NA
    })
    b_posterior <- EpiEstim:::vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      1 / (1 / b_prior + sum(lambda[seq(t_start[t], t_end[t])], na.rm = TRUE))
    }
    else {
      NA
    })
    sample_r_posterior <- vapply(seq_len(nb_time_periods), function(t) 
      if (!is.na(a_posterior[t])) {
        rgamma(sample_size,
               shape = unlist(a_posterior[t]),
               scale = unlist(b_posterior[t])
        )
      }
      else {
        rep(NA, sample_size)
      }, numeric(sample_size))
    if (sample_size == 1L) {
      sample_r_posterior <- matrix(sample_r_posterior, nrow = 1)
    }
    return(list(sample_r_posterior, si_distr))
  }
  
  method <- match.arg(method)
  
  incid <- EpiEstim:::process_I(incid)
  T <- nrow(incid)
  
  a_prior <- (config$mean_prior / config$std_prior)^2
  b_prior <- config$std_prior^2 / config$mean_prior
  
  EpiEstim:::check_times(config$t_start, config$t_end, T)
  nb_time_periods <- length(config$t_start)
  
  if (method == "si_from_sample") {
    if (is.null(config$n2)) {
      stop("method si_from_sample requires to specify the config$n2 argument.")
    }
    si_sample <- EpiEstim:::process_si_sample(si_sample)
  }
  
  min_nb_cases_per_time_period <- ceiling(1 / config$cv_posterior^2 - a_prior)
  incidence_per_time_step <- calc_incidence_per_time_step(
    incid, config$t_start,
    config$t_end
  )
  if (incidence_per_time_step[1] < min_nb_cases_per_time_period) {
    warning("You're estimating R too early in the epidemic to get the desired
            posterior CV.")
  }
  
  if (method == "non_parametric_si") {
    si_uncertainty <- "N"
    parametric_si <- "N"
  }
  if (method == "parametric_si") {
    si_uncertainty <- "N"
    parametric_si <- "Y"
  }
  if (method == "uncertain_si") {
    si_uncertainty <- "Y"
    parametric_si <- "Y"
  }
  if (method == "si_from_data" | method == "si_from_sample") {
    si_uncertainty <- "Y"
    parametric_si <- "N"
  }
  if (si_uncertainty == "Y") {
    if (parametric_si == "Y") {
      mean_si_sample <- rep(-1, config$n1)
      std_si_sample <- rep(-1, config$n1)
      for (k in seq_len(config$n1)) {
        while (mean_si_sample[k] < config$min_mean_si || mean_si_sample[k] >
               config$max_mean_si) {
          mean_si_sample[k] <- rnorm(1,
                                     mean = config$mean_si,
                                     sd = config$std_mean_si
          )
        }
        while (std_si_sample[k] < config$min_std_si || std_si_sample[k] >
               config$max_std_si) {
          std_si_sample[k] <- rnorm(1, mean = config$std_si,
                                    sd = config$std_std_si)
        }
      }
      temp <- lapply(seq_len(config$n1), function(k) sample_from_posterior(config$n2,
                                                                           incid, mean_si_sample[k], std_si_sample[k],
                                                                           si_distr = NULL, a_prior,
                                                                           b_prior, config$t_start, config$t_end
      ))
      config$si_distr <- cbind(
        t(vapply(seq_len(config$n1), function(k) (temp[[k]])[[2]], numeric(T))),
        rep(0, config$n1)
      )
      r_sample <- matrix(NA, config$n2 * config$n1, nb_time_periods)
      for (k in seq_len(config$n1)) {
        r_sample[seq((k - 1) * config$n2 + 1, k * config$n2), which(config$t_end >
                                                                      mean_si_sample[k])] <- (temp[[k]])[[1]][, which(config$t_end >
                                                                                                                        mean_si_sample[k])]
      }
      mean_posterior <- apply(r_sample, 2, mean, na.rm = TRUE)
      std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
      quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                        0.025,
                                        na.rm = TRUE
      )
      quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                       0.05,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.25,
                                       na.rm = TRUE
      )
      median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.75,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.95,
                                       na.rm = TRUE
      )
      quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                        0.975,
                                        na.rm = TRUE
      )
    }
    else {
      config$n1 <- dim(si_sample)[2]
      mean_si_sample <- rep(-1, config$n1)
      std_si_sample <- rep(-1, config$n1)
      for (k in seq_len(config$n1)) {
        mean_si_sample[k] <- sum((seq_len(dim(si_sample)[1]) - 1) * 
                                   si_sample[, k])
        std_si_sample[k] <- sqrt(sum(si_sample[, k] * 
                                       ((seq_len(dim(si_sample)[1]) - 1) - 
                                          mean_si_sample[k])^2))
      }
      temp <- lapply(seq_len(config$n1), function(k) sample_from_posterior(config$n2,
                                                                           incid,
                                                                           mean_si = NULL, std_si = NULL, si_sample[, k], a_prior,
                                                                           b_prior, config$t_start, config$t_end
      ))
      config$si_distr <- cbind(
        t(vapply(seq_len(config$n1), function(k) (temp[[k]])[[2]], 
                 numeric(nrow(si_sample)))),
        rep(0, config$n1)
      )
      r_sample <- matrix(NA, config$n2 * config$n1, nb_time_periods)
      for (k in seq_len(config$n1)) {
        r_sample[seq((k - 1) * config$n2 + 1,k * config$n2), which(config$t_end >
                                                                     mean_si_sample[k])] <- (temp[[k]])[[1]][, which(config$t_end >
                                                                                                                       mean_si_sample[k])]
      }
      mean_posterior <- apply(r_sample, 2, mean, na.rm = TRUE)
      std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
      quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                        0.025,
                                        na.rm = TRUE
      )
      quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                       0.05,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.25,
                                       na.rm = TRUE
      )
      median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.75,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.95,
                                       na.rm = TRUE
      )
      quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                        0.975,
                                        na.rm = TRUE
      )
    }
  } else {
    # CertainSI
    if (parametric_si == "Y") {
      config$si_distr <- discr_si(seq(0,T - 1), config$mean_si, config$std_si)
    }
    if (length(config$si_distr) < T + 1) {
      config$si_distr[seq(length(config$si_distr) + 1,T + 1)] <- 0
    }
    final_mean_si <- sum(config$si_distr * (seq(0,length(config$si_distr) -
                                                  1)))
    Finalstd_si <- sqrt(sum(config$si_distr * (seq(0,length(config$si_distr) -
                                                     1))^2) - final_mean_si^2)
    post <- posterior_from_si_distr(
      incid, config$si_distr, a_prior, b_prior,
      config$t_start, config$t_end
    )
    
    a_posterior <- unlist(post[[1]])
    b_posterior <- unlist(post[[2]])
    mean_posterior <- a_posterior * b_posterior
    std_posterior <- sqrt(a_posterior) * b_posterior
    quantile_0.025_posterior <- qgamma(0.025,
                                       shape = a_posterior,
                                       scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.05_posterior <- qgamma(0.05,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.25,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    median_posterior <- qgamma(0.5,
                               shape = a_posterior,
                               scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.75,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.95,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.975_posterior <- qgamma(0.975,
                                       shape = a_posterior,
                                       scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
  }
  
  results <- list(R = as.data.frame(cbind(
    config$t_start, config$t_end, mean_posterior,
    std_posterior, quantile_0.025_posterior, quantile_0.05_posterior,
    quantile_0.25_posterior, median_posterior, quantile_0.25_posterior,
    quantile_0.25_posterior, quantile_0.975_posterior
  )))
  
  names(results$R) <- c(
    "t_start", "t_end", "Mean(R)", "Std(R)",
    "Quantile.0.025(R)", "Quantile.0.05(R)", "Quantile.0.25(R)",
    "Median(R)", "Quantile.0.75(R)", "Quantile.0.95(R)",
    "Quantile.0.975(R)"
  )
  results$method <- method
  results$si_distr <- config$si_distr
  if (is.matrix(results$si_distr)) {
    colnames(results$si_distr) <- paste0("t", seq(0,ncol(results$si_distr) - 1))
  } else {
    names(results$si_distr) <- paste0("t", seq(0,length(results$si_distr) - 1))
  }
  if (si_uncertainty == "Y") {
    results$SI.Moments <- as.data.frame(cbind(
      mean_si_sample,
      std_si_sample
    ))
  } else {
    results$SI.Moments <- as.data.frame(cbind(
      final_mean_si,
      Finalstd_si
    ))
  }
  names(results$SI.Moments) <- c("Mean", "Std")
  
  
  if (!is.null(incid$dates)) {
    results$dates <- EpiEstim:::check_dates(incid)
  } else {
    results$dates <- seq_len(T)
  }
  results$I <- rowSums(incid[, c("local", "imported")])
  results$I_local <- incid$local
  results$I_imported <- incid$imported
  
  results$r_sample <- r_sample
  
  class(results) <- "estimate_R"
  return(results)
}


default.R.cori <- partial(estimate.R0.cori,
                          delay = 7,
                          day0 = 2,
                          method = "si_from_sample",
                          si.data = nd, 
                          si.sample = nishi_si[,sample(1:ncol(nd), 1)], # Samples 1 SI interval
                          p.distribution = "G",
                          modified = TRUE,
                          n2 = 50)

