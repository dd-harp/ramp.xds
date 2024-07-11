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
#' @return the function modifies **pars** and returns it: the computed FoI are stored as `pars$AR`
#' @export
Exposure.dts <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    b = F_b(y, pars, i)
    pars$AR[[i]] = F_ar(pars$EIR[[i]]*pars$Xday, b, pars) + travel_malaria(t, pars)
  }
  return(pars)
}

#' @title A model for daily FoI as a function of the daily EIR.
#' @description This function compute the daily local FoI as a function
#' of the daily EIR and effects of partial immunity.
#' It is computed based on a probabilistic model of exposure and
#' possibly including environmental heterogeneity. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$FOIpar`.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars the model object as a [list]
#' @return the daily, local force of infection as a [numeric] vector
#' @export
F_foi <- function(eir, b, pars){
  UseMethod("F_foi", pars$FOIpar)
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
#' @param pars the model object as a [list]
#' @return the daily, local force of infection as a [numeric] vector
#' @export
foi2eir <- function(foi, b, pars){
  UseMethod("foi2eir", pars$FOIpar)
}

#' @title A model for daily attack rate as a function of the daily EIR.
#' @description This function computes the daily local AR as a function
#' of the daily EIR and effects of partial immunity.
#' It is computed based on a probabilistic model of exposure and
#' possibly including environmental heterogeneity. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$ARpar`.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars the model object as a [list]
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
#' @param pars the model object as a [list]
#' @return the daily, local force of infection as a [numeric] vector
#' @export
ar2eir <- function(ar, b, pars){
  UseMethod("ar2eir", pars$ARpar)
}


