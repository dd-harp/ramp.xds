
#' @title Exposure and Infection
#' @description This function translates seven days of daily entomological
#' inoculation rate (dEIR) into a multiday attack rate. The daily FoI
#' is the sum of two terms: 1) a function [F_foi] computes the local dFoI;
#' 2) a function [travel_eir] and [traveling]
#' exposure while traveling.
#' @inheritParams Exposure
#' @return the function modifies **pars** and returns it: the computed FoI are stored as `pars$FoI`
#' @export
Exposure.multiday <- function(t, y, pars){
  dd = t%%pars$Xday + 1
  for(i in 1:pars$nHosts){
    trv = pars$time_traveling[[i]]
    travelEIR = travel_eir(t, pars, i)*pars$Xday
    b = as.vector(F_b(y, pars, i))
    pars$AR[[i]] = (1-trv)*F_ar(pars$EIR[[i]]*pars$Xday, b, pars) + trv*F_ar(travelEIR, b, pars)
    #pars$EIR_D[[i]][dd] = pars$EIR[[i]]
    #b = F_b(y, pars, i)
    #pars$AR[[i]] = F_ar(pars$EIR_D[[i]], b, pars) + F_a(t, pars)
    #pars$EIR_D[[i]] = 0*pars$EIR[[i]]
  }
  return(pars)
}

#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @return none
#' @export
setup_exposure_multiday <- function(pars) {
  class(pars$Xpar) <- "multiday"
  pars$EIR_D <- pars$EIR
  return(pars)
}

