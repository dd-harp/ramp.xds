#' @title Compute Infection Rates
#' @description Compute the force of infection (FoI) or
#' attack rates (AR) as a function of the local
#' daily entomological inoculation rate (dEIR),
#' immunity, and exposure to malaria while traveling.
#' @param t the time
#' @param y the state variables
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @seealso Cases: [Exposure.xde] & [Exposure.dts]. Related: [travel_malaria] & [F_ar] & [F_foi]
#' @export
Exposure <- function(t, y, pars){
  UseMethod("Exposure", pars$xds)
}

#' @title Compute the FoI
#' @description For **`xde`** models, compute the FoI
#' @details
#' The local force of infection (FoI or \eqn{h}) is
#' a function of local daily entomological inoculation
#' rate (dEIR or \eqn{E}) under
#' a model for the probability of infection
#' per infectious bite, \eqn{b} (called from \eqn{F_b},
#' as defined by an \eqn{\cal X} model).
#' The total FoI is a weighted sum of the local FoI and
#' exposure to malaria while traveling, (\eqn{T_h}):
#' \deqn{h = (1-\delta) \; F_h(E, b) + \delta\; T_h(b,t)}
#' @inheritParams Exposure
#' @seealso Related: [Exposure] & [F_foi.pois] & [F_foi.nb] & [travel_malaria]
#' @return an **`xds`** object
#' @export
Exposure.xde <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = as.vector(F_b(y, pars, i))
    pars$FoI[[i]] = F_foi(pars$EIR[[i]], b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Compute Attack Rates
#' @description For `dts` models, compute
#' the attack rates, the expected probability of infection
#' over one time step, is a weighted sum of local and travel exposure.
#' @details
#' Local attack rates (AR or \eqn{\alpha}) are
#' a function of local daily entomological inoculation
#' rates (dEIR or \eqn{E}), and
#' a model for the probability of infection
#' per infectious bite, \eqn{b} (from \eqn{F_b},
#' as defined by an \eqn{\cal X} model).
#' The total attack rates also consider
#' exposure to malaria while traveling, (\eqn{T_\delta}):
#' \deqn{\alpha = (1-\delta) \; F_\alpha(E, b) + \delta\; T_\alpha(b)}
#' @inheritParams Exposure
#' @return an **`xds`** object
#' @seealso Related: [Exposure] & [F_ar.pois] & [F_ar.nb] & [travel_malaria]
#' @export
Exposure.dts <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = as.vector(F_b(y, pars, i))
    pars$AR[[i]] = F_ar(pars$EIR[[i]]*pars$Xday, b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Compute the Local FoI
#' @description Compute the daily local FoI as a function
#' of the daily EIR and effects of partial immunity.
#' @details
#' This function computes the daily
#' force of infection (FoI)
#' as a function of the daily EIR under
#' a probabilistic model for environmentally
#' heterogeneous exposure --
#' the distribution of bites per person.
#'
#' The model for human / host infection (\eqn{\cal X}) provide
#' a term \eqn{b}, from [F_b] that describe the probability of infection
#' per infectious bite, possibly affected by
#' pre-erythrocytic immunity.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars an **`xds`** object
#' @return the local FoI as a [numeric] vector of length \eqn{n_h =} `nStrata`
#' @seealso Cases: [F_foi.pois] & [F_foi.nb]. Related: [Exposure.xde] & [foi2eir]
#' @export
F_foi <- function(eir, b, pars){
  UseMethod("F_foi", pars$FoIpar)
}

#' @title Compute Local Attack Rates
#' @description Compute the local attack rates as a function of the daily EIR and immunity.
#' @details This function computes the daily
#' local attack rates (ARs)
#' as a function of the daily EIR under
#' a probabilistic model for environmentally
#' heterogeneous exposure --
#' the distribution of bites per person.
#'
#' The model for human / host infection (\eqn{\cal X}) provide
#' a term \eqn{b}, from [F_b] that describe the probability of infection
#' per infectious bite, possibly affected by
#' pre-erythrocytic immunity.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars an **`xds`** object
#' @return a [numeric] vector: local attack rates for the strata
#' @seealso Cases: [F_ar.nb] and [F_ar.pois]. Related: [ar2eir] & [Exposure.dts]
#' @export
F_ar <- function(eir, b, pars){
  UseMethod("F_ar", pars$ARpar)
}

#' @title Convert FoI to EIR
#' @description This function computes the inverse of [F_foi]
#' under a model for environmentally heterogeneous exposure.
#' @param foi the daily FoI for each stratum
#' @param b the probability of infection, per bite
#' @param pars an **`xds`** object
#' @return the daily EIR as a [numeric] vector
#' @seealso Cases: [foi2eir.pois] & [foi2eir.nb]
#' @export
foi2eir <- function(foi, b, pars){
  UseMethod("foi2eir", pars$FoIpar)
}

#' @title Convert AR to EIR
#' @description This function computes the inverse of [F_ar]
#' under a model for environmentally heterogeneous exposure.
#' @param ar the attack rate
#' @param b the probability of infection, per bite
#' @param pars an `xds` object
#' @return the daily EIR as a [numeric] vector
#' @seealso Cases: [ar2eir.pois] & [ar2eir.nb]
#' @export
ar2eir <- function(ar, b, pars){
  UseMethod("ar2eir", pars$ARpar)
}


