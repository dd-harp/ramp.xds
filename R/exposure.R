# X

#' @title Exposure 
#' 
#' @description
#' The EIR computed by [xds_info_transmission] is a measure of *local* risk: it is 
#' the expected number of infective
#' bites, per person, and it is computed for every population stratum. In 
#' **exposure,** the daily local EIR is converted into a measure
#' of the population FoI in three steps: 
#' + local and travel exposure are evaluated separately;
#' + the distribution of the number of bites per person is computed under a 
#' model of environmental heterogeneity;
#' + local and travel FoI are computed under a model of partial immunity
#' and combined using time here *vs.* time away.
#' 
#' @section Environmental Heterogeneity:
#' While EIR is an expected value, the expected value can have a distribution
#' in a human population stratum. For example, if the expectation has a Gamma
#' distribution, then the expected number of bites per person would be negatively
#' binomially distributed. For more, see [xds_info_environmental_heterogeneity].
#'  
#' @section Travel Malaria:
#' Travel malaria is defined by exposure to malaria while spending
#' time outside of the spatial domain represented by the patches.
#' See [xds_info_travel_malaria].
#' 
#' @section Pre-Erythrocytic Immunity:
#' Depending on the X module, a parameter called \eqn{b} describes the probability
#' of infection per infectious bite.  
#'  
#' @seealso [xds_info_environmental_heterogeneity] | [xds_info_travel_malaria] 
#' 
#' @name xds_info_exposure
NULL


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
#' @param options set up options list
#' @param i the host species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_exposure <- function(EHname, xds_obj, options=list(), i=1){
  class(EHname) <- EHname
  UseMethod("setup_exposure", EHname)
}

#' @title Compute Infection Rates
#' @description Compute the force of infection (FoI) or
#' attack rates (AR) as a function of the local
#' daily entomological inoculation rate (dEIR),
#' immunity, and exposure to malaria while traveling.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @keywords internal
#' @rdname Exposure
#' @export
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
#' computed from the travel EIR, \eqn{E_\delta}:
#' \deqn{h = (1-\delta) \; F_h(E, b) + \delta\; F_h(E_\delta, b,t)}
#' @inheritParams Exposure
#' @seealso Related: [Exposure] & [F_foi.pois] & [F_foi.nb] & [xds_info_travel_malaria] 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
Exposure.xde <- function(t, y, xds_obj){
  for(i in 1:xds_obj$nHostSpecies){
    xds_obj <- update_travel_eir(t, xds_obj, i)
    b = as.vector(F_infectivity(y, xds_obj, i))
    eir = xds_obj$terms$EIR[[i]]
    away = get_time_away(xds_obj, i)
    at_home = 1 - away
    local_foi  = F_foi(eir, b, xds_obj$env_het_obj[[i]])
    teir= xds_obj$XH_obj[[i]]$travel_eir
    travel_foi = F_foi(teir, b, xds_obj$env_het_obj[[i]])
    xds_obj$terms$FoI[[i]] = local_foi*at_home + travel_foi*away
  }
  return(xds_obj)
}

#' @title Compute Attack Rates
#' @description For `dts` models, compute
#' the attack rates, the expected probabilities of infection
#' over one time step, from the time-weighted local and travel FoI.
#' @details
#' Local attack rates (AR or \eqn{\alpha}) are
#' a function of local daily entomological inoculation
#' rates (dEIR or \eqn{E}), and
#' a model for the probability of infection
#' per infectious bite, \eqn{b} (from \eqn{F_infectivity},
#' as defined by an \eqn{\cal X} model).
#' The total attack rates are computed from the time-weighted local and travel
#' FoI, using the travel EIR, \eqn{E_\delta}:
#' \deqn{\alpha = 1-\exp(-((1-\delta) F_h(E,b) + \delta F_h(E_\delta,b)))}
#' @inheritParams Exposure
#' @return an **`xds`** model object
#' @seealso Related: [Exposure] & [F_ar.pois] & [F_ar.nb] & [xds_info_travel_malaria]
#' @export
#' @keywords internal
Exposure.dts <- function(t, y, xds_obj){
  for(i in 1:xds_obj$nHostSpecies){
    xds_obj <- update_travel_eir(t, xds_obj, i)
    b = as.vector(F_infectivity(y, xds_obj, i))
    eir = xds_obj$terms$EIR[[i]]
    away = get_time_away(xds_obj, i)
    at_home = 1 - away
    local_foi  = F_foi(eir, b, xds_obj$env_het_obj[[i]])
    teir= xds_obj$XH_obj[[i]]$travel_eir
    travel_foi = F_foi(teir, b, xds_obj$env_het_obj[[i]])
    xds_obj$terms$FoI[[i]] = local_foi*at_home + travel_foi*away
    xds_obj$terms$AR[[i]] = 1 - exp(-(local_foi*at_home + travel_foi*away)) 
  }
  return(xds_obj)
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
#' The model for human / host infection (\eqn{\cal X}) provides
#' a term \eqn{b}, from [F_infectivity], that describes the probability of infection
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
#' The model for human / host infection (\eqn{\cal X}) provides
#' a term \eqn{b}, from [F_infectivity], that describes the probability of infection
#' per infectious bite, possibly affected by
#' pre-erythrocytic immunity.
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
