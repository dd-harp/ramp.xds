
#' @title Create the residence matrix, \eqn{\cal J}
#' @description The residence matrix, \eqn{\cal J}, holds
#' information about the location of home for each human population stratum.
#' If \eqn{w} is any vector describing a quantity in strata (*i.e.*, \eqn{\left|w\right|=} `nStrata`), then
#' \eqn{W={\cal J}\cdot w} is a vector that has summed \eqn{w} by residency for the strata, and \eqn{\left|W\right|=} `nPatches`.
#' @details Information about residence in a patch location for each stratum
#' is passed as the residence vector, an ordered list of patch locations. If
#' the \eqn{i^{th}} stratum lives in the \eqn{j^{th}} patch, then
#' \eqn{{\cal J}[j,i]=1.} other_bloodwise, \eqn{{\cal J}[j,i]=0.}
#' @param nPatches the number of patches
#' @param residence a vector describing the patch index for each habitat
#' @return the residence [matrix], denoted \eqn{\cal J} where \eqn{\left|\cal J\right|=}`nPatches` \eqn{\times} `nStrata`
#' @seealso to extract habitat residence information, see [view_residence_matrix]
#' @export
create_residence_matrix = function(nPatches, residence){
  nStrata = length(residence)
  calJ = matrix(0, nPatches, nStrata)
  calJ[cbind(residence, 1:nStrata)]=1
  return(calJ)
}

#' @title Set up the interface for blood feeding
#' @description Sets up the object that defines the blood feeding interface
#' for an `xds` model object.
#' @details Blood feeding is modeled as an interaction among humans and mosquitoes.
#' `ramp.xds` uses the concept of host availability for blood feeding that is based
#' on a notion of *searching* for hosts. Each hosts is assigned search weights, \eqn{\omega}
#' and availability is the sum of those search weights. The total availability of all
#' vertebrate hosts for blood feeding is \eqn{B} and the availability of humans
#' (or a pathogen's host species) is \eqn{W}.
#' These can be used to compute mosquito bionomic parameters. For mosquitoes, blood feeding rates (\eqn{f})
#' can be computed using *functional responses.* The human fraction is \eqn{W/B}. Availability can
#' also be used to model mosquito movement.
#' In models with multiple host species, with availability \eqn{W_i}, the fraction on
#' each host species would be \eqn{W_i/B}.
#' In models with multiple vector species, each species could have different search habits and preferences,
#' so blood feeding availability is indexed for each species: \eqn{B_s} and \eqn{W_{s}}.
#' In models with multiple host species, \eqn{W_{i,s}} is the availability of the
#' \eqn{i^{th}} host species to the \eqn{s^{th}} vector species.
#' For hosts, availability is based on *time spent* in each patch, and *time at risk,* or
#' time spent by time of day weighted by mosquito species-specific *search weights* reflecting different preferences
#' and a circadian function describing relative mosquito blood feeding rates by time of day.
#' @param pars an `xds` object
#' @return a [list]
#' @seealso [setup_TRANSMISSION]
#' @export
setup_BLOOD_FEEDING <- function(pars){

  up = list()
  class(up) <- "setup"

  H = F_H(0,pars,1)
  wts = rep(1, pars$nStrata)
  calJ = pars$residence_matrix[[1]]
  W = compute_W(wts, H, calJ)

  # There is a set of search weights for
  # each combination of host and vector species:
  # search_weights[[s]][[i]]
  up$search_weights = list()
  up$search_weights[[1]] = list()
  up$search_weights[[1]][[1]] = wts


  # Relative activity rates for mosquitoes
  up$F_circadian = list()
  up$F_circadian[[1]] = F_1

  # Mosquito
  pars$BFpar <- up

  # Time Spent (TiSp): For each Host Species, i
  pars$TimeSpent = list()
  pars$TimeSpent[[1]] = calJ

  # Time at Risk (TaR): For each TiSp,
  # one for each Vector Species, s
  # TaR[[s]][[i]]
  pars$TaR = list()
  pars$TaR[[1]] = list()
  pars$TaR[[1]][[1]] = calJ

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
#' @return the modified `xds` object
#' @export
change_blood_weights = function(pars, search_weights=1, s=1, i=1){
  search_weights = checkIt(search_weights, pars$nStrata)
  pars$BFpar$search_weights[[s]][[i]] = checkIt(search_weights, pars$nStrata[i], F)
  class(pars$BFpar) <- 'setup'
  return(pars)
}

#' @title Compute blood feeding availability for hosts
#' @description
#' Computes availability of the parasite's / pathogen's hosts to blood feeding
#' mosquitoes using the concept of search weights and time-at-risk
#' @details
#' Host availability to blood searching mosquitoes in patches is
#' the sum of search weights of the human strata, a vector \eqn{\omega},
#' weighted by time at risk, defined by a matrix \eqn{\Psi}
#' that is \eqn{N_p \times \N_h}. The search weight is a *per-capita* measure
#' so we weight it by human population density, \eqn{H}. Availability, \eqn{W}
#' is computed as \deqn{\Psi \cdot (\omega H).}
#' @references{This function implements equation 3
#' from \insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density
#' @param TaR the host species index
#' @return host availability, a [vector]
#' @export
compute_W = function(search_weights, H, TaR){
  W=TaR %*% (search_weights*H)
  return(as.vector(W))
}

#' @title Compute blood feeding availability of all vertebrate hosts
#' @description
#' Computes availability all vertebrate hosts to blood feeding
#' mosquitoes
#' @details
#' The availability of other vertebrate hosts is a sum of available local
#' hosts \eqn{W}, and visitors \eqn{W_\delta}, and other available vertebrate hosts, \eqn{O}.
#' Total availability is a simple sum: \deqn{B = W + W_\delta + O}
#' @references{This function implements equation 3
#' from \insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
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
#' @param pars an `xds` object
#' @param y state vector
#' @return the modified `xds` object
#' @export
make_WB <- function(pars, y=0){
  y = as.vector(unlist(y))
  for(s in 1:pars$nVectors){
    H = F_H(y, pars, 1)
    TaR = pars$TaR[[s]][[1]]
    wts = pars$BFpar$search_weights[[s]][[1]]
    Wi = compute_W(wts, H, TaR)

    pars$vars$Wi[[s]][[1]] = Wi
    pars$vars$W[[s]] = Wi

    if(pars$nHosts > 1){
      for(i in 2:pars$nHosts){
        H = F_H(y, pars, i)
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
#' @param pars an `xds` object
#' @param y state vector
#' @return the modified `xds` object
#' @export
make_RBR = function(pars, y){
  y = as.vector(unlist(y))
  for(i in 1:pars$nHosts){
    H = F_H(y, pars, i)
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
#' @references{This function implements equation 3
#' from \insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param t the time
#' @param TiSp a time spent matrix
#' @param F_circadian a function to compute relative activity rates by time of day
#' @return a TaR [matrix]
#' @export
compute_TaR = function(t, TiSp, F_circadian){
  d = 24*floor(t%%1)
  TaR = F_circadian(d)*TiSp
  return(TaR)
}

#' @title Make TaR
#' @description Make a time at risk matrix (TaR) from a time spent matrix and a circadian function
#' @param pars an `xds` object
#' @param t the time
#' @return the modified `xds` object
#' @export
make_TaR <- function(pars, t=0){
  for(s in 1:pars$nVectors)
    for(i in 1:pars$nHosts)
      pars$TaR[[i]][[s]] = compute_TaR(t, pars$TimeSpent[[i]], pars$BFpar$F_circadian[[s]])
  return(pars)
}

#' @title Blood feeding
#' @description Compute and store host availability, \eqn{W},
#' total blood host availability, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi},
#' @param t the time
#' @param y the state variables
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
BloodFeeding = function(t, y, pars){
  UseMethod("BloodFeeding", pars$BFpar)
}

#' @title Compute blood feeding objects: setup for static models
#' @description This sets up host availability, \eqn{W},
#' total blood host availability, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
#' for static models.
#' @inheritParams BloodFeeding
#' @return the modified `xds` object
#' @export
BloodFeeding.setup = function(t, y, pars){
  class(pars$BFpar) <- 'dynamic'
  pars <- BloodFeeding(t,y,pars)
  class(pars$BFpar) <- 'static'
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
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
#' for static models.
#' @inheritParams BloodFeeding
#' @return the modified `xds` object
#' @export
BloodFeeding.dynamic = function(t, y, pars){
  pars <- make_TaR(pars)
  pars <- make_WB(pars, y)
  pars <- make_RBR(pars, y)
  return(pars)
}

#' @title View residence membership
#' @description Shows the residence membership information from \eqn{\cal J}
#' @param pars an `xds` object
#' @param i the host species index
#' @return a named [list]
#' @seealso [create_residence_matrix]
#' @export
view_residence_matrix = function(pars, i=1){
  which(t(pars$residence_matrix[[i]])==1, arr.ind=TRUE) -> residence
  res <- list(stratum = residence[,1], residence_patch = residence[,2])
  return(res)
}


