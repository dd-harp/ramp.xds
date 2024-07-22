#' @title Compute local exposure and travel
#' @description Function that model the FoI or AR as a function of
#' the local daily entomological inoculation rate (dEIR),
#' and a model for exposure to malaria while traveling.
#' @param t the time
#' @param y the state variables
#' @param pars an `xds` object
#' @return the modified `xds` object; FoI are stored as `pars$FoI`
#' @export
Exposure <- function(t, y, pars){
  UseMethod("Exposure", pars$Xpar)
}

#' @title Compute the force of infection (FoI) given the local EIR and a travel model
#' @description For xde models, compute the force of infection (FoI) for
#' all the population strata. The FoI is a function of the daily entomological
#' inoculation rates (dEIR), and a model for exposure to malaria while traveling.
#' @inheritParams Exposure
#' @return the modified `xds` object; FoIs are stored as `pars$FoI`
#' @export
Exposure.xde <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$FoI[[i]] = F_foi(pars$EIR[[i]], b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Compute the attack rate (AR) given the local EIR and a travel model
#' @description For dts models, compute the attack rate (AR) for
#' all the population strata. The AR is a function of local daily entomological
#' inoculation rates (dEIR), and a model for exposure to malaria while traveling.
#' @inheritParams Exposure
#' @return the modified `xds` object; ARs are stored as `pars$AR`
#' @export
Exposure.dts <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$AR[[i]] = F_ar(pars$EIR[[i]]*pars$Xday, b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Compute the local daily FoI as a function of the local daily EIR.
#' @description This function compute the daily local FoI as a function
#' of the daily EIR and effects of partial immunity.
#' It is computed based on a probabilistic model of exposure and
#' possibly including environmental heterogeneity. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$FoIpar`.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars an `xds` object
#' @return the daily, local force of infection as a [numeric] vector
#' @export
F_foi <- function(eir, b, pars){
  UseMethod("F_foi", pars$FoIpar)
}

#' @title Convert the daily FoI into a daily EIR under a model
#' @description This function is the inverse of [F_foi]: compute the
#' daily EIR given an estimated FoI under a probabilistic model
#' of exposure. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$FOIpar`.
#' @param foi the attack rate
#' @param b the probability of infection, per bite
#' @param pars an `xds` object
#' @return the daily, local force of infection as a [numeric] vector
#' @export
foi2eir <- function(foi, b, pars){
  UseMethod("foi2eir", pars$FoIpar)
}

#' @title Daily attack rates as a function of the daily EIR
#' @description This function implements a model for environmental heterogeneity.
#' It computes the daily local AR as a function
#' of the daily local EIR and effects of partial immunity.
#' It is computed based on a probabilistic model of exposure, the distribution of the number of
#' bites per person. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$ARpar`.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars an `xds` object
#' @return the daily, local force of infection as a [numeric] vector
#' @export
F_ar <- function(eir, b, pars){
  UseMethod("F_ar", pars$ARpar)
}

#' @title Convert the attack rate into an eir under a model
#' @description This function is the inverse of [F_ar]: compute the
#' daily EIR given an estimated attack rate under a probabilistic model
#' of exposure. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$FOIpar`.
#' @param ar the attack rate
#' @param b the probability of infection, per bite
#' @param pars an `xds` object
#' @return the daily, local force of infection as a [numeric] vector
#' @export
ar2eir <- function(ar, b, pars){
  UseMethod("ar2eir", pars$ARpar)
}


