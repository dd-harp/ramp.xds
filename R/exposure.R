#' @title Exposure and Infection
#' @description Function that model the FoI or AR as a function of
#' the local daily entomological inoculation rate (dEIR).
#' exposure while traveling.
#' @param t the time
#' @param y the state variables
#' @param pars the model object as a [list]
#' @return the function modifies **pars** and returns it: the computed FoI are stored as `pars$FoI`
#' @export
Exposure <- function(t, y, pars){
  UseMethod("Exposure", pars$Xpar)
}

#' @title Exposure and Infection
#' @description For xde models, compute the daily force of infection (dFoI) for
#' a population. The dFoI is a function of the local daily entomological
#' inoculation rate (dEIR), and a function [travel_malaria] that accounts for
#' exposure while traveling.
#' @inheritParams Exposure
#' @return the function modifies **pars** and returns it: the computed FoI are stored as `pars$FoI`
#' @export
Exposure.xde <- function(t, y, pars){

  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$FoI[[i]] = F_foi(pars$EIR[[i]], b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title Exposure and Infection
#' @description For xde models, compute the daily force of infection (dFoI) for
#' a population. The dFoI is a function of the local daily entomological
#' inoculation rate (dEIR), and a function [travel_malaria] that accounts for
#' exposure while traveling.
#' @inheritParams Exposure
#' @return the function modifies **pars** and returns it: the computed FoI are stored as `pars$FoI`
#' @export
Exposure.dts <- function(t, y, pars){

  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$AR[[i]] = F_ar(pars$EIR[[i]], b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

