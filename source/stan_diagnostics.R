# Functions for diagnostic checks of stan model fits
# Adapted from Michael Betancourt here: https://github.com/betanalpha/knitr_case_studies/blob/master/rstan_workflow/stan_utility.R
librarian::shelf(rstan)

stan_diagnostics <- function(fit, max_treedepth=NULL, warnings.only=FALSE) {
  
  if (is.null(max_treedepth)) {
    max_treedepth <- 10
    warning("Evaluated stanfit assuming default max_treedepth=10. If different maximum tree depth was used to fit model, specify that value or diagnostic may be incorrect.")
  }
  suppressPackageStartupMessages(require(rstan))
  if (class(fit)[1]=="stanreg") fit <- fit$stanfit
  
  
  fit_summary <- rstan::summary(fit, probs=c(0.5))$summary
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  par_names <- row.names(fit_summary)
  
  # Check the effective sample size per iteration (n_eff/iter)
  n_eff <- fit_summary[, "n_eff"]
  iter <- nrow(rstan::extract(fit)[[1]])
  if (any(n_eff/iter < 0.001)) {
    bad_n_eff <- which(n_eff / iter<0.001)
    message("  Effective Sample Size per Iteration (n_eff/iter) Diagnostic")
    message(paste(c("", rep("\n", length(bad_n_eff)-1)),"    n_eff/iter for", par_names[bad_n_eff], "is below threshold 0.001."))
    message("    The effective sample size has likely been overestimated.")
  } else {
    if(warnings.only==FALSE) {
      message("  Effective Sample Size per Iteration (n_eff/iter) Diagnostic")
      message("    No warnings: all n_eff/iter above 0.001 threshold")
    }
  }
  
  # Check potential scale reduction factor (Rhat)
  rhat <- fit_summary[, "Rhat"]
  if (any(rhat > 1.01)) {
    bad_rhat <- which(rhat > 1.01)
    message("  Potential Scale Reduction Factor (Rhat) Diagnostic")
    message(paste(c("", rep("\n", length(bad_rhat)-1)), "   Rhat for", par_names[bad_rhat], "is above threshold 1.01."))
    message("    The chains very likely have not mixed.")
  } else {
    if(warnings.only==FALSE) {
      message("  Potential Scale Reduction Factor (Rhat) Diagnostic")
      message("    No warnings: all Rhats below 1.01 threshold")
    }
  }
  
  # Check if any transitions ended in divergence
  divergent <- do.call(rbind, sampler_params)[, "divergent__"]
  n_div <- sum(divergent)
  N_div <- length(divergent)
  if (n_div > 0) {
    message("  Divergence Diagnostic")
    message(sprintf("    %s of %s iterations ended with a divergence (%s%%).", n_div, N_div, 100*n_div/N_div))
    message("    Try fitting model with a larger adapt_delta (default=0.95).")
  } else {
    if(warnings.only==FALSE) {
      message("  Divergence Diagnostic")
      message("    No warnings: there were no divergent transitions")
    }
  }
  
  # Check if any transitions ended prematurely due to maximum tree depth limit
  treedepths <- do.call(rbind, sampler_params)[, "treedepth__"]
  n_tree <-length(treedepths[sapply(treedepths, function(x) x==max_treedepth)])
  N_tree <- length(treedepths)
  if (n_tree > 0) {
    message("  Maximum Tree Depth Diagnostic")
    message(sprintf("    %s of %s iterations saturated the maximum tree depth of %s (%s%%).", n_tree, N_tree, max_treedepth, 100*n_tree/N_tree))
    message("    Try fitting model with larger max_treedepth (default=10). Change within argument control (e.g. control=list(max_treedepth=15).")
  } else {
    if(warnings.only==FALSE) {
      message("  Maximum Tree Depth Diagnostic")
      message("    No warnings: no iterations saturated the maximum tree depth")
    }
  }
  
  # Check the energy Bayesian fraction of missing information (E-BFMI)
  warning <- FALSE
  for (n in 1:length(sampler_params)) {
    nrg <- sampler_params[n][[1]][, "energy__"]
    n_nrg <- sum(diff(nrg)**2)/length(nrg)
    d_nrg <- var(nrg)
    
    if (n_nrg/d_nrg < 0.2) {
      if(warning==FALSE) message("  Energy Bayesian Fraction of Missing Information (E-BFMI) Diagnostic")
      message(sprintf("    Chain %s: E-BFMI = %s.", n, n_nrg / d_nrg))
      warning <- TRUE
    }
  }
  if (warning) {
    message("    An E-BFMI below 0.2 indicates you may need to reparameterize your model.")
  } else {
    if(warnings.only==FALSE) {
      message("  Energy Bayesian Fraction of Missing Information (E-BFMI) Diagnostic")
      message("    No warnings: no E-BFMI below 0.2")
    }
  }
}