

#' @title Blood Feeding 
#' 
#' @description
#' Blood feeding is an interaction among humans and mosquitoes: 
#' in this framework, humans spend time and 
#' mosquitoes search. To model exposure, each human (or host) 
#' population stratum is assigned a *search weight,* a  
#' number used to weigh *time spent* and get a measure of 
#' *availability.* 
#'  
#' @name xds_info_blood_feeding 
NULL

#' @title Get residence vector 
#' @description Get the residence vector 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a named [list]
#' @export
get_residence = function(xds_obj, i=1){
  return(xds_obj$residence[[i]])
}

#' @title Create the residence Matrix
#'
#' @description This function creates the residence matrix that is used to sum
#' quantities describing human (or host) populations at the level of a patch.
#'
#' It is created
#' from the residence vector (`residence`), an ordered list
#' of the patch index for each stratum.
#'
#' The structural
#' parameter `nPatches` to handle cases where some patches
#' have no residents.
#'
#' @details
#' The residence matrix, herein denoted \eqn{J},  holds
#' information about residence for each human (or host) population stratum.
#'
#' Information about residence in a patch location for each stratum
#' is passed as the residence vector, an ordered list of patch locations. If
#' the \eqn{i^{th}} stratum lives in the \eqn{j^{th}} patch, then
#' \eqn{{J}_{j,i}=1.} Otherwise, \eqn{{J}_{j,i}=0.}
#'
#' Let:
#' - \eqn{N_h = } `nStrata`, the number of population strata;
#' - \eqn{N_p = } `nPatches`, the number of patches.
#'
#' \eqn{J} is an \eqn{N_p \times N_h} matrix that is used to map information about
#' human (or host) populations onto patches.
#'
#' If \eqn{w} is any vector describing a quantity in strata (*i.e.*, \eqn{\left|w\right|=N_h}), then
#' \deqn{W={J}\cdot w} computes a vector that sums \eqn{w} by residence for the strata, and \eqn{\left|W\right|= N_p}.
#'
#' It is a template for the time spent and time at risk matrices, making it possible
#' to compute mosquito parameters describing blood feeding, the mixing matrix,
#' and terms describing transmission.
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a `nPatches` \eqn{\times} `nStrata` matrix
#'
#' @seealso see [setup_XY_interface]
#'
#' @export
get_residence_matrix = function(xds_obj, i=1){
  with(xds_obj,{
    mat <- make_residence_matrix(nPatches, residence[[i]])
    return(mat)
})}


#' @title Make a residence Matrix
#'
#' @description This function creates the residence matrix (see [get_residence_matrix]).
#'
#' @param nPatches the number of patches 
#' @param residence the residence matrix 
#'
#' @return a `nPatches` \eqn{\times} `nStrata` matrix
#' @keywords internal
#'
#' @export
make_residence_matrix = function(nPatches, residence){
  nStrata = length(residence)
  residence_matrix = matrix(0, nPatches, nStrata)
  residence_matrix[cbind(residence, 1:nStrata)]=1
  return(residence_matrix)
}

#' @title Check the XY Interface
#'
#' @description Run a set of consistency checks for the `XY_interface`
#'
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
#' @keywords internal
#'
check_XY_interface = function(xds_obj){
  return(xds_obj)
}


#' @title Setup the Blood Feeding Interface
#'
#' @description
#' This function, called by [make_xds_object_template], sets up
#' the blood feeding interface, `xds_obj$XY_interface.`
#'
#' @details
#' This implements a framework to model blood feeding
#' described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in **`ramp.xds`** requires a rigid interface to
#' guarantee mathematical consistency in computing quantites related to blood feeding and transmission.
#'
#' The interface for blood feeding is defined by an object called
#' `XY_interface`,
#' attached to the **`xds`** model object as `xds_obj$XY_interface.`
#'
#' The blood feeding interface sets up several objects:
#' - a time spent (TiSp) matrix \eqn{\Theta} (see [xds_info_time_spent])
#' - a circadian function `F_circadian` for each vector species
#' - a time at risk (TaR) matrix \eqn{\Psi,} the product of the TiSp matrix and the circadian function
#' - blood feeding search weights \eqn{\omega}
#' - a vector describing the available of humans (or hosts), \eqn{W} (see [F_W_available])
#' - a vector describing the available of visitors, \eqn{V}
#' - a vector describing the available of other blood hosts, \eqn{O}
#' - a vector describing the total available of blood hosts, \eqn{B} (see [F_B_available])
#'
#' These quantities are used to compute the transmission matrix, \eqn{\beta}, to models transmission (see [setup_transmission()]).
#'
#' Mosquito bionomic parameters *ought* to be constrained. If bionomic parameters are
#' assigned, there's no guarantee they are internally mathematically consistent or sensible.
#' To guarantee internal consistency, the the concept of resource available should be
#' used to compute the blood feeding rates (\eqn{f})
#' using *functional responses.* The human fraction ought to be \eqn{q=W/B}.
#' available can also be used to model mosquito movement.
#'
#' **Mulit-Host Models**
#'
#' In models with multiple host species, let \eqn{W_i} denote the available of the \eqn{i^{th}} host species.
#' Total availablity of blood hosts is \deqn{B = \sum_i W_i + O + V,}
#' so the the fraction of bites on each host species is \eqn{W_i/B}.
#'
#' In models with multiple vector species and one host species, each vector species could have different search habits and preferences.
#' so blood feeding available is computed for each species, denoted \eqn{B_s} and \eqn{W_{s}}.
#'
#' In models with multiple vector and multiple host species, \eqn{W_{i,s}}
#' is the available of the \eqn{i^{th}} host species to the \eqn{s^{th}} vector species.
#'
#' For hosts, available is based on *time spent* in each patch, and *time at risk,* or
#' time spent by time of day weighted by mosquito species-specific *search weights* reflecting different preferences
#' and a circadian function describing relative mosquito blood feeding rates by time of day.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#'
#' @seealso [setup_transmission]
#'
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @keywords internal
#'
#' @export
#' @keywords internal
setup_XY_interface <- function(xds_obj){
  with(xds_obj,{

    interface = list()
    class(interface) <- "setup"

    residence_matrix <- get_residence_matrix(xds_obj, 1)

    H = rep(1, nStrata)
    wts = rep(1, nStrata)
    W = F_W_available(wts, H, residence_matrix)

    # Relative activity rates for mosquitoes
    interface$F_circadian = list()
    interface$F_circadian[[1]] = F_flat

    # Time at Risk (TaR): computed from XH_obj[[i]]$timespent
    # TaR[[s]][[i]]
    interface$TaR = list()
    interface$TaR[[1]] = list()
    interface$TaR[[1]][[1]] = residence_matrix

    # Exogenous variables
    # Available Blood Hosts: Wi[[s]][[i]]
    interface$Wi = list()
    interface$Wi[[1]] = list()
    interface$Wi[[1]][[1]] = W

    interface$W = list()
    interface$W[[1]] = W

    interface$B = list()
    interface$B[[1]] = W

    interface$visitors = list()
    interface$visitors[[1]] =  rep(0, nPatches) 
    
    interface$vis_kappa = list()
    interface$vis_kappa[[1]] =  rep(0, nPatches) 

    # Static defaults for ports managed by Resources
    interface$other_blood_hosts = list()
    interface$other_blood_hosts[[1]] = rep(0, nPatches)

    interface$traps = list()
    interface$traps[[1]] = rep(0, nPatches)

    # Mosquito
    xds_obj$XY_interface <- interface



    return(xds_obj)
})}

#' @title Compute Host available for Blood Feeding
#' @description
#' Compute the available of the population strata defined
#' in the model as hosts for blood feeding by mosquitoes
#' @details
#' Host available to blood searching mosquitoes in patches is
#' the sum of search weights of the human strata, a vector \eqn{\omega},
#' weighted by time at risk, defined by a matrix \eqn{\Psi}
#' that is \eqn{N_p \times N_h}. The search weight is a *per-capita* measure
#' so we weight it by human population density, \eqn{H}. available, \eqn{W}
#' is computed as \deqn{\Psi \cdot (\omega H).}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density
#' @param TaR the host species index
#' @return host available, a [vector]
#' @seealso available of all vertebrate hosts for blood feeding is computed by [F_B_available()]
#' @export
#' @keywords internal
F_W_available = function(search_weights, H, TaR){
  W = TaR %*% (search_weights*H)
  return(as.vector(W))
}

#' @title Compute Vertebrate Host available for Blood Feeding
#' @description
#' Compute the available all vertebrate hosts for blood feeding by mosquitoes
#' @details
#' The available of other vertebrate hosts is a sum of available local
#' hosts \eqn{W}, and visitors \eqn{W_\delta}, and other available vertebrate hosts, \eqn{O}.
#' Total available is a simple sum: \deqn{B = W + W_\delta + O}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param W available of the parasite's / pathogen' local hosts
#' @param visitors available of *visitors,* or non-resident host populations
#' @param other_blood available of other vertebrate hosts
#' @param traps blood feeding trap availability
#' @return host available, a [vector]
#' @export
#' @keywords internal
F_B_available = function(W, visitors, other_blood, traps){
  B = W + visitors + other_blood + traps
  return(B)
}

#' @title Compute available blood hosts of the i^th species
#' @description Compute the available for the pathogen's hosts for blood feeding
#' @param t the time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
#' @keywords internal
compute_WB <- function(t, y, xds_obj){
  y = as.vector(unlist(y))
  with(xds_obj$XY_interface,{
    for(s in 1:xds_obj$nVectorSpecies){
      W = 0
      for(i in 1:xds_obj$nHostSpecies){
        H = F_H(t, y, xds_obj, i)
        tar = TaR[[s]][[i]]
        wts = xds_obj$XH_obj[[i]]$search_weights[[s]]
        Wi = F_W_available(wts, H, tar)
        xds_obj$XY_interface$Wi[[s]][[i]] = Wi
        W = W + Wi
      }
      xds_obj$XY_interface$W[[s]] = W
      xds_obj$XY_interface$B[[s]] = F_B_available(W, visitors[[s]], other_blood_hosts[[s]], traps[[s]])
    }
    return(xds_obj)
})}

#' @title Compute relative biting rates
#' @description Relative biting rates translate biting weights into a
#' frailty parameter -- a multiplicative term.
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density
#' @return host available, a [vector]
#' @export
#' @keywords internal
F_rbr = function(search_weights, H){
  rbr = search_weights*sum(H)/sum(search_weights*H)
  return(as.vector(rbr))
}

#' @title Compute and attach the relative biting rates
#' @description Compute the available for the pathogen's hosts for blood feeding
#' @param t the time
#' @param xds_obj an **`xds`** model object
#' @param y state vector
#' @return an **`xds`** object
#' @export
#' @keywords internal
compute_RBR = function(t, xds_obj, y){
  y = as.vector(unlist(y))
  for(i in 1:xds_obj$nHostSpecies){
    H = F_H(t, y, xds_obj, i)
    wts = xds_obj$XY_interface$search_weights[[1]][[i]]
    xds_obj$rbr[[i]] = F_rbr(wts, H)
  }
  return(xds_obj)
}

#' @title Compute blood feeding available of all vertebrate hosts
#' @description
#' Computes available all vertebrate hosts to blood feeding
#' mosquitoes
#' @details
#' The available of other vertebrate hosts is a sum of available local
#' hosts \eqn{W}, and visitors \eqn{W_\delta}, and other available vertebrate hosts, \eqn{O}.
#' Total available is a simple sum: \deqn{B = W + W_\delta + O}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param t the time
#' @param TiSp a time spent matrix
#' @param F_circadian a function to compute relative activity rates by time of day
#' @param time_away the fraction of time spent at home
#' @return a TaR [matrix]
#' @export
#' @keywords internal
F_TaR = function(t, TiSp, F_circadian, time_away){
  d = 24*floor(t%%1)
  TaR = F_circadian(d)*(TiSp %*% diag(1-time_away))
  return(TaR)
}

#' @title Make TaR
#' @description Make a time at risk matrix (TaR) from a time spent matrix and a circadian function
#' @param xds_obj an **`xds`** model object
#' @param t the time
#' @return an **`xds`** object
#' @export
#' @keywords internal
compute_TaR <- function(xds_obj, t=0){
  for(s in 1:xds_obj$nVectorSpecies)
    for(i in 1:xds_obj$nHostSpecies){
      TiSp <- get_timespent_matrix(xds_obj, i)
      F_circ <-  xds_obj$XY_interface$F_circadian[[s]]
      time_away <- get_time_away(xds_obj, i)
      TaR = F_TaR(t, TiSp, F_circ, time_away)
      xds_obj$XY_interface$TaR[[i]][[s]] = TaR
    }
  return(xds_obj)
}

#' @title Blood feeding
#' @description Compute and store host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi},
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
#' @keywords internal
BloodFeeding = function(t, y, xds_obj){
  UseMethod("BloodFeeding", xds_obj$XY_interface)
}

#' @title Compute blood feeding objects: setup for static models
#' @description This sets up host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
#' for static models.
#' @details The mixing matrix, \eqn{\beta}, depends on
#' blood feeding terms, so the class of `xds_obj$beta` must also
#' be updated, if they are not dynamic, so [trigger_setup] is called.
#' @inheritParams BloodFeeding
#' @return an **`xds`** object
#' @export
#' @keywords internal
BloodFeeding.setup = function(t, y, xds_obj){
  class(xds_obj$XY_interface) <- 'static'
  xds_obj$terms$beta <- trigger_setup(xds_obj$terms$beta)
  xds_obj <- blood_feeding_dynamics(t, y, xds_obj)
  return(return(xds_obj))
}

#' @title Compute blood feeding objects: static models
#' @description Return the blood feeding objects unmodified
#' @inheritParams BloodFeeding
#' @return an **`xds`** object
#' @export
#' @keywords internal
BloodFeeding.static = function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Compute blood feeding objects dynamically
#' @description Compute host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{(\Theta)}, and the time-at-risk matrix \eqn{(\Psi)}
#' for static models.
#' @inheritParams BloodFeeding
#' @return an **`xds`** object
#' @export
#' @keywords internal
BloodFeeding.dynamic = function(t, y, xds_obj){
  return(blood_feeding_dynamics(t, y, xds_obj))
}

#' @title Compute blood feeding objects dynamically
#' @description Compute host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{(\Theta)}, and the time-at-risk matrix \eqn{(\Psi)}
#' for static models.
#' @inheritParams BloodFeeding
#' @return an **`xds`** object
#' @export
#' @keywords internal
blood_feeding_dynamics = function(t, y, xds_obj){
  xds_obj <- timespent(t, y, xds_obj)
  for(i in 1:xds_obj$nHostSpecies){
    xds_obj <- update_timespent(xds_obj, i)
    xds_obj <- update_time_away(xds_obj, i)
    for(s in 1:xds_obj$nVectorSpecies)
      xds_obj <- update_blood_search_weights(xds_obj, s, i)
  }
  xds_obj <- compute_TaR(xds_obj, t)
  xds_obj <- compute_WB(t, y, xds_obj)
  return(xds_obj)
}


#' @title Set static blood feeding search weights
#' @description Set the blood feeding search weights, \eqn{\omega}, for a set of host strata
#' @param other_blood_hosts availability of other blood hosts
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
change_other_blood_hosts = function(other_blood_hosts, xds_obj, s){
  other_blood_hosts = checkIt(other_blood_hosts, xds_obj$nPatches, fixit=TRUE) 
  xds_obj$XY_interface$other_blood_hosts[[s]] = other_blood_hosts
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

