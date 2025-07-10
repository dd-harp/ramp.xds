
#' @title Create the Residency Matrix 
#' @description The residency matrix, \eqn{J}, holds
#' information about residency for each human population stratum.
#' It is the template for the time spent and time at risk matrices, making it possible
#' to compute mosquito parameters describing blood feeding, the mixing matrix,
#' and terms describing transmission.
#' @details Information about residence in a patch location for each stratum
#' is passed as the residence vector, an ordered list of patch locations. If
#' the \eqn{i^{th}} stratum lives in the \eqn{j^{th}} patch, then
#' \eqn{{J}_{j,i}=1.} Otherwise, \eqn{{J}_{j,i}=0.}
#'
#' Since \eqn{J} is a matrix, it is readily used for computation. Let:
#' - \eqn{n_h = } `nStrata`, the number of population strata;
#' - \eqn{n_p = } `nPatches`, the number of patches.
#'
#' If \eqn{w} is any vector describing a quantity in strata (*i.e.*, \eqn{\left|w\right|=n_h}), then
#' \deqn{W={J}\cdot w} is a vector that has summed \eqn{w} by residency for the strata, and \eqn{\left|W\right|= n_p}.
#' @param nPatches the number of patches
#' @param residence a vector describing the patch index for each habitat
#' @return the residence [matrix], denoted \eqn{J} where \eqn{\left|J\right|= n_p \times n_h}
#' @seealso see [setup_BLOOD_FEEDING]
#' @seealso see [view_residence_matrix]
#' @export
create_residence_matrix = function(nPatches, residence){
  nStrata = length(residence)
  Jmatrix = matrix(0, nPatches, nStrata)
  Jmatrix[cbind(residence, 1:nStrata)]=1
  return(Jmatrix)
}

#' @title Set up Blood Feeding
#' @description
#' Set up a part of the **`xds`** object that defines the interface for blood feeding
#' @details
#' This implements a blood feeding model described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in ramp.xds requires a rigid interface to
#' guarantee mathematical consistency for blood feeding and transmission.
#' The interface is for blood feeding is defined by an object called `BFpar` that
#' is attached to the **`xds`** object pars as `pars$BFpar`. The blood feeding interface
#' - the residency matrix \eqn{J}
#' - a time spent (TiSp) matrix \eqn{\Theta}
#' - a circadian function `F_circadian` for each vector species
#' - a time at risk (TaR) matrix \eqn{\Psi} that is the product the TiSp matrix and the circadian function
#' - blood feeding search weights
#' - a vector describing \eqn{W}, the availability the population strata for blood feeding: the availability of the parasite/pathogen's hosts
#' - a vector describing the availability of visitors
#' - a vector describing the availability of other blood hosts
#' - a vector describing \eqn{B}, the total availability of all vertebrate blood hosts for blood feeding (see [compute_B()])
#'
#' These quantities are used to model transmission (see [setup_TRANSMISSION()]).
#'
#' Mosquito bionomic parameters *ought* to be constrained. If bionomic parameters are
#' assigned, there's no guarantee they are internally mathematically consistent or sensible.
#' To guarantee internal consistency, the the concept of resource availability should be
#' used to compute the blood feeding rates (\eqn{f})
#' using *functional responses.* The human fraction ought to be \eqn{q=W/B}.
#' Availability can also be used to model mosquito movement.
#'
#' **Mulit-Host Models** - In models with multiple host species, with availability \eqn{W_i}, the fraction on
#' each host species would be \eqn{W_i/B}.
#' In models with multiple vector species, each species could have different search habits and preferences,
#' so blood feeding availability is indexed for each species: \eqn{B_s} and \eqn{W_{s}}.
#' In models with multiple host species, \eqn{W_{i,s}} is the availability of the
#' \eqn{i^{th}} host species to the \eqn{s^{th}} vector species.
#' For hosts, availability is based on *time spent* in each patch, and *time at risk,* or
#' time spent by time of day weighted by mosquito species-specific *search weights* reflecting different preferences
#' and a circadian function describing relative mosquito blood feeding rates by time of day.
#' @param pars an **`xds`** object
#' @return a modified **`xds`** object
#' @seealso [setup_TRANSMISSION]
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @export
setup_BLOOD_FEEDING <- function(pars){

  up = list()
  class(up) <- "setup"

  H = rep(1, pars$nStrata)
  wts = rep(1, pars$nStrata)
  Jmatrix = pars$residence_matrix[[1]]
  W = compute_W(wts, H, Jmatrix)

  # There is a set of search weights for
  # each combination of host and vector species:
  # search_weights[[s]][[i]]
  up$search_weights = list()
  up$search_weights[[1]] = list()
  up$search_weights[[1]][[1]] = wts

  # Relative activity rates for mosquitoes
  up$F_circadian = list()
  up$F_circadian[[1]] = F_flat

  # Mosquito
  pars$BFpar <- up

  # Time Spent (TiSp): For each Host Species, i
  pars$TimeSpent = list()
  pars$TimeSpent[[1]] = Jmatrix
  pars$time_traveling = list()
  pars$time_traveling[[1]] = rep(0, pars$nStrata)

  # Time at Risk (TaR): For each TiSp,
  # one for each Vector Species, s
  # TaR[[s]][[i]]
  pars$TaR = list()
  pars$TaR[[1]] = list()
  pars$TaR[[1]][[1]] = Jmatrix

  # Exogenous variables
  # Available Blood Hosts: Wi[[s]][[i]]
  pars$vars$Wi = list()
  pars$vars$Wi[[1]] = list()
  pars$vars$Wi[[1]][[1]] = W

  pars$vars$W = list()
  pars$vars$W[[1]] = W

  pars$vars$visitors = list()
  pars$vars$visitors[[1]] = 0*W

  pars$vars$other_blood = list()
  pars$vars$other_blood[[1]] = 0*W

  pars$vars$B = list()
  pars$vars$B[[1]] = W

  pars$rbr = list()
  pars$rbr[[1]] = wts

  return(pars)
}

#' @title Set static blood feeding search weights
#' @description Set the blood feeding search weights, \eqn{\omega}, for a set of host strata
#' @param pars an `xds` object
#' @param search_weights the blood feeding search weights
#' @param s the vector species index
#' @param i the host species index
#' @return an `xds` object
#' @export
change_blood_weights = function(pars, search_weights=1, s=1, i=1){
  search_weights = checkIt(search_weights, pars$nStrata)
  pars$BFpar$search_weights[[s]][[i]] = checkIt(search_weights, pars$nStrata[i], F)
  class(pars$BFpar) <- 'setup'
  return(pars)
}

#' @title Compute Host Availability for Blood Feeding
#' @description
#' Compute the availability of the population strata defined
#' in the model as hosts for blood feeding by mosquitoes
#' @details
#' Host availability to blood searching mosquitoes in patches is
#' the sum of search weights of the human strata, a vector \eqn{\omega},
#' weighted by time at risk, defined by a matrix \eqn{\Psi}
#' that is \eqn{n_p \times n_h}. The search weight is a *per-capita* measure
#' so we weight it by human population density, \eqn{H}. Availability, \eqn{W}
#' is computed as \deqn{\Psi \cdot (\omega H).}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density
#' @param TaR the host species index
#' @return host availability, a [vector]
#' @seealso Availability of all vertebrate hosts for blood feeding is computed by [compute_B()]
#' @export
compute_W = function(search_weights, H, TaR){
  W = TaR %*% (search_weights*H)
  return(as.vector(W))
}

#' @title Compute Vertebrate Host Availability for Blood Feeding
#' @description
#' Compute the availability all vertebrate hosts for blood feeding by mosquitoes
#' @details
#' The availability of other vertebrate hosts is a sum of available local
#' hosts \eqn{W}, and visitors \eqn{W_\delta}, and other available vertebrate hosts, \eqn{O}.
#' Total availability is a simple sum: \deqn{B = W + W_\delta + O}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param W availability of the parasite's / pathogen' local hosts
#' @param visitors availability of *visitors,* or non-resident host populations
#' @param other_blood availability of other vertebrate hosts
#' @return host availability, a [vector]
#' @export
compute_B = function(W, visitors, other_blood){
  B = W + visitors + other_blood
  return(B)
}

#' @title Compute availability blood hosts of the i^th species
#' @description Compute the availability for the pathogen's hosts for blood feeding
#' @param t the time
#' @param pars an `xds` object
#' @param y state vector
#' @return an `xds` object
#' @export
make_WB <- function(t, pars, y=0){
  y = as.vector(unlist(y))
  for(s in 1:pars$nVectorSpecies){
    H = F_H(t, y, pars, 1)
    TaR = pars$TaR[[s]][[1]]
    wts = pars$BFpar$search_weights[[s]][[1]]
    Wi = compute_W(wts, H, TaR)

    pars$vars$Wi[[s]][[1]] = Wi
    pars$vars$W[[s]] = Wi

    if(pars$nHostSpecies > 1){
      for(i in 2:pars$nHostSpecies){
        H = F_H(t, y, pars, i)
        TaR = pars$TaR[[s]][[i]]
        wts = pars$BFpar$search_weights[[s]][[i]]
        Wi = compute_W(wts, H, TaR)

        pars$vars$Wi[[s]][[1]] = Wi
        pars$vars$W[[s]] = pars$vars$W[[s]] + Wi
      }
    }
    pars$vars$W[[s]] = as.vector(pars$vars$W[[s]])
    pars$vars$B[[s]] = with(pars$vars, compute_B(W[[s]], visitors[[s]], other_blood[[s]]))
  }
  return(pars)
}

#' @title Compute relative biting rates
#' @description Relative biting rates translate biting weights into a
#' frailty parameter -- a multiplicative term.
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density
#' @return host availability, a [vector]
#' @export
compute_RBR = function(search_weights, H){
  rbr = search_weights*sum(H)/sum(search_weights*H)
  return(as.vector(rbr))
}

#' @title Compute and attach the relative biting rates
#' @description Compute the availability for the pathogen's hosts for blood feeding
#' @param t the time
#' @param pars an `xds` object
#' @param y state vector
#' @return an `xds` object
#' @export
make_RBR = function(t, pars, y){
  y = as.vector(unlist(y))
  for(i in 1:pars$nHostSpecies){
    H = F_H(t, y, pars, i)
    wts = pars$BFpar$search_weights[[1]][[i]]
    pars$rbr[[i]] = compute_RBR(wts, H)
  }
  return(pars)
}

#' @title Compute blood feeding availability of all vertebrate hosts
#' @description
#' Computes availability all vertebrate hosts to blood feeding
#' mosquitoes
#' @details
#' The availability of other vertebrate hosts is a sum of available local
#' hosts \eqn{W}, and visitors \eqn{W_\delta}, and other available vertebrate hosts, \eqn{O}.
#' Total availability is a simple sum: \deqn{B = W + W_\delta + O}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param t the time
#' @param TiSp a time spent matrix
#' @param F_circadian a function to compute relative activity rates by time of day
#' @param time_traveling the fraction of time spent outside the spatial domain
#' @return a TaR [matrix]
#' @export
compute_TaR = function(t, TiSp, F_circadian, time_traveling){
  d = 24*floor(t%%1)
  TaR = F_circadian(d)*TiSp %*% diag(1-time_traveling)
  return(TaR)
}

#' @title Make TaR
#' @description Make a time at risk matrix (TaR) from a time spent matrix and a circadian function
#' @param pars an `xds` object
#' @param t the time
#' @return an `xds` object
#' @export
make_TaR <- function(pars, t=0){
  for(s in 1:pars$nVectorSpecies)
    for(i in 1:pars$nHostSpecies){
      pars = traveling(t, pars, i)
      pars$TaR[[i]][[s]] = compute_TaR(t, pars$TimeSpent[[i]], pars$BFpar$F_circadian[[s]], pars$time_traveling[[i]])
    }
  return(pars)
}

#' @title Blood feeding
#' @description Compute and store host availability, \eqn{W},
#' total blood host availability, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi},
#' @param t the time
#' @param y the state variables
#' @param pars an `xds` object
#' @return an `xds` object
#' @export
BloodFeeding = function(t, y, pars){
  UseMethod("BloodFeeding", pars$BFpar)
}

#' @title Compute blood feeding objects: setup for static models
#' @description This sets up host availability, \eqn{W},
#' total blood host availability, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
#' for static models.
#' @details The mixing matrix, \eqn{\beta}, depends on
#' blood feeding terms, so the class of `pars$beta` must also
#' be updated, if they are not dynamic, so [trigger_setup] is called.
#' @inheritParams BloodFeeding
#' @return an `xds` object
#' @export
BloodFeeding.setup = function(t, y, pars){
  class(pars$BFpar) <- 'dynamic'
  pars <- BloodFeeding(t,y,pars)
  class(pars$BFpar) <- 'static'
  pars$beta <- trigger_setup(pars$beta)
  return(return(pars))
}

#' @title Compute blood feeding objects: static models
#' @description Return the blood feeding objects unmodified
#' @inheritParams BloodFeeding
#' @return the unmodified `xds` object
#' @export
BloodFeeding.static = function(t, y, pars){
  return(pars)
}

#' @title Compute blood feeding objects dynamically
#' @description Compute host availability, \eqn{W},
#' total blood host availability, \eqn{B},
#' the time spent matrix \eqn{(\Theta)}, and the time-at-risk matrix \eqn{(\Psi)}
#' for static models.
#' @inheritParams BloodFeeding
#' @return an `xds` object
#' @export
BloodFeeding.dynamic = function(t, y, pars){
  pars <- make_TaR(pars, t)
  pars <- make_WB(t, pars, y)
  pars <- make_RBR(t, pars, y)
  return(pars)
}

#' @title View residence membership
#' @description Shows the residence membership information (from the residence matrix) 
#' @param pars an `xds` object
#' @param i the host species index
#' @return a named [list]
#' @seealso [create_residence_matrix]
#' @export
view_residence_matrix = function(pars, i=1){
  which(t(pars$residence_matrix[[i]])==1, arr.ind=TRUE) -> residence
  res <- list(stratum = as.vector(residence[1,]), residence_patch = as.vector(residence[2,]))
  return(res)
}


