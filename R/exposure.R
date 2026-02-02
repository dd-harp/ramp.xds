#' @title Set Up Exposure 
#' 
#' @description Set up the model for 
#' exposure. Current options for `EHname` 
#' 
#' + `pois` - a Poisson model 
#'
#' + `nb` - a Negative Binomial model family
#' 
#' @note The daily EIR is an expected value, but that expectation
#' can have a distribution in a population. For
#' example, if the expectation is gamma distributed, 
#' then we would get a negative binomial distribution
#' of bites per person. 
#' 
#' Note that [Exposure] handles local exposure and 
#' exposure while traveling separately.  
#' 
#' @param EHname environmental heterogeneity model name 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options set up options list
#' 
#' @return an **`xds`** object
#' 
#' @export
setup_exposure <- function(EHname, xds_obj, i=1, options=list()){
  class(EHname) <- EHname
  UseMethod("setup_exposure", EHname)
}

#' @title Compute Infection Rates
#' @description Compute the force of infection (FoI) or
#' attack rates (AR) as a function of the local
#' daily entomological inoculation rate (dEIR),
#' immunity, and exposure to malaria while traveling.
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @seealso Cases: [Exposure.xde] & [Exposure.dts]. Related: [F_ar] & [F_foi]
#' @export
#' @keywords internal
Exposure <- function(t, y, xds_obj){
  UseMethod("Exposure", xds_obj$xds)
}

#' @title Compute the Force of Infection 
#' 
#' @description For **`xde`** models, compute the FoI
#' 
#' @details
#' The local force of infection (FoI or \eqn{h}) is
#' a function of local daily entomological inoculation
#' rate (dEIR or \eqn{E}) under
#' a model for the probability of infection
#' per infectious bite, \eqn{b} (called from \eqn{F_infectivity},
#' as defined by an \eqn{\cal X} model).
#' The total FoI is a weighted sum of the local FoI and
#' exposure to malaria while traveling,
#' computed from the the travel EIR, \eqn{E_T}:
#' \deqn{h = (1-\delta) \; F_h(E, b) + \delta\; F_h(E_T, b,t)}
#' @inheritParams Exposure
#' @seealso Related: [Exposure] & [F_foi.pois] & [F_foi.nb] & [Travel]
#' @return an **`xds`** object
#' @export
#' @keywords internal
Exposure.xde <- function(t, y, xds_obj){
  with(xds_obj$XY_interface,{
    for(i in 1:xds_obj$nHostSpecies){
      b = as.vector(F_infectivity(y, xds_obj, i))
      eir = xds_obj$terms$EIR[[i]]
      at_home = time_at_home[[i]]
      local_foi  = F_foi(eir, b, env_het_obj[[i]])
      tEIR = xds_obj$terms$travel_EIR[[i]]
      travel_foi = F_foi(tEIR, b, env_het_obj[[i]])
      xds_obj$terms$FoI[[i]] = local_foi*at_home + travel_foi*(1-at_home)
  }
  
  return(xds_obj)
})}

#' @title Compute Attack Rates
#' @description For `dts` models, compute
#' the attack rates, the expected probability of infection
#' over one time step, is a weighted sum of local and travel exposure.
#' @details
#' Local attack rates (AR or \eqn{\alpha}) are
#' a function of local daily entomological inoculation
#' rates (dEIR or \eqn{E}), and
#' a model for the probability of infection
#' per infectious bite, \eqn{b} (from \eqn{F_infectivity},
#' as defined by an \eqn{\cal X} model).
#' The total attack rates also consider
#' exposure to malaria while traveling, (\eqn{T_\delta}):
#' \deqn{\alpha = 1-((1-\delta) \; F_\alpha(E, b))(1-\delta\; T_\alpha(b))}
#' @inheritParams Exposure
#' @return an **`xds`** object
#' @seealso Related: [Exposure] & [F_ar.pois] & [F_ar.nb] & [Travel] 
#' @export
#' @keywords internal
Exposure.dts <- function(t, y, xds_obj){
  with(xds_obj$XY_interface,{
    for(i in 1:xds_obj$nHostSpecies){
      b = as.vector(F_infectivity(y, xds_obj, i))
      eir = xds_obj$terms$EIR[[i]]
      tisp_local = time_at_home[[i]]
      local_ar  = F_ar(eir, b, env_het_obj[[i]])
      tEIR = xds_obj$terms$travel_EIR[[i]]
      travel_ar = F_ar(tEIR, b, env_het_obj[[i]])
      xds_obj$ar[[i]] = 1-(1-local_ar*tisp_local)*(1-travel_ar*(1-tisp_local))
  }
  return(xds_obj)
})}

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
#' a term \eqn{b}, from [F_infectivity] that describe the probability of infection
#' per infectious bite, possibly affected by
#' pre-erythrocytic immunity.
#' 
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param env_het_obj an environmental heterogeneity model object
#' 
#' @return the local FoI as a [numeric] vector of length \eqn{n_h =} `nStrata`
#' @seealso Cases: [F_foi.pois] & [F_foi.nb]. Related: [Exposure.xde] & [foi2eir]
#' @export
#' @keywords internal
F_foi <- function(eir, b, env_het_obj){
  UseMethod("F_foi", env_het_obj)
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
#' a term \eqn{b}, from [F_infectivity] that describe the probability of infection
#' per infectious bite, possibly affected by
#' pre-erythrocytic immunity
#' 
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param env_het_obj an environmental heterogeneity model object
#' 
#' @return a [numeric] vector: local attack rates for the strata
#' 
#' @seealso Cases: [F_ar.nb] and [F_ar.pois]. Related: [ar2eir] & [Exposure.dts]
#' @export
#' @keywords internal
F_ar <- function(eir, b, env_het_obj){
  UseMethod("F_ar", env_het_obj)
}

#' @title Convert FoI to EIR
#' @description This function computes the inverse of [F_foi]
#' under a model for environmentally heterogeneous exposure.
#' 
#' @param foi the daily FoI for each stratum
#' @param b the probability of infection, per bite
#' @param env_het_obj an environmental heterogeneity model object
#' 
#' @return the daily EIR as a [numeric] vector
#' @seealso Cases: [foi2eir.pois] & [foi2eir.nb]
#' @export
foi2eir <- function(foi, b, env_het_obj){
  UseMethod("foi2eir", env_het_obj)
}

#' @title Convert AR to EIR
#' @description This function computes the inverse of [F_ar]
#' under a model for environmentally heterogeneous exposure.
#' 
#' @param ar the attack rate
#' @param b the probability of infection, per bite
#' @param env_het_obj an environmental heterogeneity model object
#' 
#' @return the daily EIR as a [numeric] vector
#' @seealso Cases: [ar2eir.pois] & [ar2eir.nb]
#' @export
ar2eir <- function(ar, b, env_het_obj){
  UseMethod("ar2eir", env_het_obj)
}



