#' @title Compute local exposure and travel
#' @description Compute the force of infection (FoI) or
#' attack rates (AR) as a function of the local
#' daily entomological inoculation rate (dEIR),
#' and exposure to malaria while traveling.
#' @param t the time
#' @param y the state variables
#' @param pars an **`xds`** object
#' @return the modified **`xds`** object
#' @export
Exposure <- function(t, y, pars){
  UseMethod("Exposure", pars$xds)
}

#' @title Compute the FoI
#' @description For xde models, compute
#' the force of infection (FoI) for
#' as a function of the daily entomological
#' inoculation rates (dEIR), and
#' exposure to malaria while traveling.
#' @inheritParams Exposure
#' @return the modified **`xds`** object
#' @export
Exposure.xde <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$FoI[[i]] = F_foi(pars$EIR[[i]], b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Compute the Attack Rates
#' @description For dts models, compute
#' the attack rates (AR) as a function
#' of local daily entomological inoculation
#' rates (dEIR), and a model for exposure
#' to malaria while traveling.
#' @details
#' Local attack rates are computed
#' under a model for immunity that
#' computes the probability of infection
#' per infectious bite, \eqn{b}, returned
#' from \eqn{F_b}, as defiend by the
#' \eqn{\cal X} model.
#' @inheritParams Exposure
#' @return the modified **`xds`** object
#' @export
Exposure.dts <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$AR[[i]] = F_ar(pars$EIR[[i]]*pars$Xday, b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Compute the FoI from local exposure
#' @description This function compute the daily local FoI as a function
#' of the daily EIR and effects of partial immunity.
#' It is computed based on a probabilistic model of
#' environmentally heterogeneous exposure.
#' If a model of human / host infection (\eqn{\cal X}) has terms
#' that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called
#' by [F_b], those effects are implemented here.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars an **`xds`** object
#' @return the local FoI as a [numeric] vector of length \eqn{n_h =} `nStrata`
#' @export
F_foi <- function(eir, b, pars){
  UseMethod("F_foi", pars$FoIpar)
}

#' @title Convert the AR from local exposure
#' @description This function compute the daily
#' local attack rates (ARs) as a function
#' of the daily EIR and effects of partial immunity.
#' It is computed based on a probabilistic model
#' environmentally heterogeneous exposure.
#' If a model of human / host infection (\eqn{\cal X}) has terms
#' that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called
#' by [F_b], those effects are implemented here.
#' @param foi the attack rate
#' @param b the probability of infection, per bite
#' @param pars an **`xds`** object
#' @return the local ARs as a [numeric] vector of length \eqn{n_h =} `nStrata`
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
#' @export
ar2eir <- function(ar, b, pars){
  UseMethod("ar2eir", pars$ARpar)
}


