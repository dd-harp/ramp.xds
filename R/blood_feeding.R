
#' @title Get residence
#' 
#' @description Get the residence vector for host species `i`.
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return a numeric vector of patch indices, one per population stratum
#' @export
get_residence = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$residence)
}

#' @title Get the residence matrix
#'
#' @description Get the residence matrix for host species `i`.
#' @inheritSection residence_matrix Residence Matrix
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a `nPatches` \eqn{\times} `nStrata` matrix
#'
#' @seealso [residence_matrix], [get_residence], [setup_blood_feeding_interface]
#'
#' @export
get_residence_matrix = function(xds_obj, i=1){
  with(xds_obj,{
    mat <- make_residence_matrix(nPatches, get_residence(xds_obj, i))
    return(mat)
})}

#' @title Make the residence matrix
#'
#' @description Create the residence matrix (see [residence_matrix]).
#'
#' @param nPatches the number of patches 
#' @param residence a residence vector of patch indices, one per population stratum
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

#' @title Check the blood-feeding interface
#'
#' @description Currently returns `xds_obj` unchanged; no consistency checks
#' are performed.
#'
#' @param xds_obj an **`xds`** model object
#' @param t a vector of times
#' @return an **`xds`** model object
#' @export
#' @keywords internal
#'
check_blood_feeding = function(xds_obj, t=0){
  return(xds_obj)
}


#' @title Setup the Blood Feeding Interface
#'
#' @description
#' This function, called by [make_xds_object_template], initializes storage
#' for blood-feeding terms and patch-level resources.
#'
#' @details
#' This implements a framework to model blood feeding
#' described by Wu SL, *et al.*, (2023).
#'
#' The initializer creates `xds_obj$blood_feeding_obj`, initializes the first
#' time-at-risk and host-availability term slots, and creates zero-valued
#' visitor, other-host, and blood-trap vectors with static port objects.
#' Time-spent matrices, circadian functions, and blood search weights are
#' configured separately. These terms are used by [setup_transmission].
#'
#' @param xds_obj an **`xds`** model object
#' @param residence a residence vector for the first host species
#'
#' @return an **`xds`** model object
#'
#' @seealso [setup_transmission], [blood_feeding], [time_at_risk],
#' [available_blood_hosts]
#'
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @keywords internal
#'
#' @export
#' @keywords internal
setup_blood_feeding_interface <- function(xds_obj, residence){
  with(xds_obj,{
    
    xds_obj$blood_feeding_obj <- make_static_obj()

    residence_matrix <- make_residence_matrix(xds_obj$nPatches, residence)

    # Time at Risk (TaR): computed from XH_obj[[i]]$timespent
    # TaR[[s]][[i]]
    xds_obj$terms$TaR = list()
    xds_obj$terms$TaR[[1]] = list()
    xds_obj$terms$TaR[[1]][[1]] = residence_matrix

    # Exogenous variables
    # Available Blood Hosts: Wi[[s]][[i]]
    xds_obj$terms$Wi = list()
    xds_obj$terms$Wi[[1]] = list()
    xds_obj$terms$Wi[[1]][[1]] = rep(0, nPatches)

    xds_obj$terms$W = list()
    xds_obj$terms$W[[1]] = rep(0, nPatches)

    xds_obj$terms$B = list()
    xds_obj$terms$B[[1]] = rep(0, nPatches)

    xds_obj$patches$visitors_obj = list()
    xds_obj$patches$visitors_obj[[1]] = make_static_obj()
    
    xds_obj$patches$visitors = list()
    xds_obj$patches$visitors[[1]] =  rep(0, nPatches) 

    xds_obj$patches$vis_kappa_obj = list()
    xds_obj$patches$vis_kappa_obj[[1]] = make_static_obj()
        
    xds_obj$patches$vis_kappa = list()
    xds_obj$patches$vis_kappa[[1]] =  rep(0, nPatches) 

    # Static defaults for ports managed by Resources
    xds_obj$patches$other_blood_hosts_obj = list()
    xds_obj$patches$other_blood_hosts_obj[[1]] = make_static_obj()
    
    xds_obj$patches$other_blood_hosts = list()
    xds_obj$patches$other_blood_hosts[[1]] = rep(0, nPatches)
  
    xds_obj$patches$blood_traps_obj = list()
    xds_obj$patches$blood_traps_obj[[1]] = make_static_obj()
    
    xds_obj$patches$blood_traps = list()
    xds_obj$patches$blood_traps[[1]] = rep(0, nPatches)

    return(xds_obj)
})}

#' @title Compute Host available for Blood Feeding
#'
#' @description Compute local human/host availability in the patches.
#' @inheritSection human_availability Human Availability
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density, adjusted for the fraction of time not traveling
#' @param TaR the time-at-risk matrix for this vector-host species pair
#' @return a numeric vector of local availability, one value per patch
#' @seealso [available_blood_hosts], [F_B_available]
#' @export
#' @keywords internal
F_W_available = function(search_weights, H, TaR){
  W = TaR %*% (search_weights*H)
  return(as.vector(W))
}

#' @title Compute total blood host availability
#' @description Compute total availability to blood-seeking mosquitoes.
#' @details Total availability sums local human/host availability, visitors,
#' other blood hosts, and blood traps: \deqn{B = W_H + W_V + W_O + W_T.}
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param W local human/host availability (\eqn{W_H})
#' @param visitors visitor availability (\eqn{W_V})
#' @param other_blood availability of other vertebrate hosts (\eqn{W_O})
#' @param traps blood trap availability (\eqn{W_T})
#' @return a numeric vector of total availability, one value per patch
#' @export
#' @keywords internal
F_B_available = function(W, visitors, other_blood, traps){
  B = W + visitors + other_blood + traps
  return(B)
}

#' @title Compute blood host availability
#' @description Compute local and total blood host availability for each
#' vector species.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal
compute_WB <- function(t, y, xds_obj){
  y = as.vector(unlist(y))
  with(xds_obj,{
    for(s in 1:nVectorSpecies){
      W = 0
      for(i in 1:nHostSpecies){
        H = F_H(t, y, xds_obj, i)
        time_away <- get_time_away(xds_obj, i)
        tar = terms$TaR[[s]][[i]]
        wts = xds_obj$XH_obj[[i]]$search_weights
        Wi = F_W_available(wts, H*(1-time_away), tar)
        xds_obj$terms$Wi[[s]][[i]] = Wi
        W = W + Wi
      }
      xds_obj$terms$W[[s]] = W
      xds_obj$terms$B[[s]] = with(patches, F_B_available(W, visitors[[s]], other_blood_hosts[[s]], blood_traps[[s]]))
    }
    return(xds_obj)
})}

#' @title Compute relative biting rates
#' @description Relative biting rates scale the blood search weights so their
#' population-weighted mean is one.
#' @param search_weights blood feeding search weights for the host strata
#' @param H host density
#' @return a numeric vector of relative biting rates
#' @export
#' @keywords internal
F_rbr = function(search_weights, H){
  rbr = search_weights*sum(H)/sum(search_weights*H)
  return(as.vector(rbr))
}

#' @title Compute and attach relative biting rates
#' @description Compute and store relative biting rates for each host species.
#' @param t time
#' @param xds_obj an **`xds`** model object
#' @param y dependent variables
#' @return an **`xds`** model object
#' @export
#' @keywords internal
compute_RBR = function(t, xds_obj, y){
  y = as.vector(unlist(y))
  for(i in 1:xds_obj$nHostSpecies){
    H = F_H(t, y, xds_obj, i)
    wts = xds_obj$XH_obj[[i]]$search_weights
    xds_obj$rbr[[i]] = F_rbr(wts, H)
  }
  return(xds_obj)
}

#' @title Compute time at risk
#'
#' @description
#' Computes time at risk from time spent \eqn{\Theta} and a circadian
#' pattern function evaluated at simulation time \eqn{t}.
#'
#' @section Time at Risk:
#'
#' The time at risk matrix describes time spent in the patches weighted by
#' relative mosquito activity rates.
#'
#' The time at risk matrix \eqn{(\Psi)} is computed from the time spent
#' matrix \eqn{(\Theta)} and the circadian mosquito activity function
#' \eqn{F_c(t)}:
#' \deqn{\Psi = F_c(t) \Theta}
#'
#' Both \eqn{\Theta} and \eqn{\Psi} have dimensions `nPatches` \eqn{\times}
#' `nStrata` for a host species. Time at risk is computed for each
#' vector-host species pair.
#' By default, \eqn{F_c(t)=1}, so \eqn{\Psi=\Theta}.
#'
#' The time-at-risk matrix is used to compute human availability \eqn{(W_H)} and the
#' biting distribution matrix \eqn{(\beta)}.
#'
#' @seealso [time_spent_matrix], [setup_timespent], [setup_F_circadian], [human_availability]
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @param t simulation time in days, passed directly to the circadian function
#' @param TiSp a time spent matrix
#' @param F_circadian a function that returns relative mosquito activity at time \eqn{t}
#' @return a time-at-risk matrix
#' @export
#' @keywords internal
F_TaR = function(t, TiSp, F_circadian){
  TaR = F_circadian(t)*TiSp
  return(TaR)
}

#' @title Make TaR
#' @description Make a time-at-risk matrix from a time-spent matrix and a
#' circadian function.
#' @param xds_obj an **`xds`** model object
#' @param t time
#' @return an **`xds`** model object
#' @export
#' @keywords internal
compute_TaR <- function(xds_obj, t=0){
  for(s in 1:xds_obj$nVectorSpecies)
    for(i in 1:xds_obj$nHostSpecies){
      TiSp <- get_timespent_matrix(xds_obj, i)
      F_circ <- xds_obj$MY_obj[[s]]$F_circadian
      TaR = F_TaR(t, TiSp, F_circ)
      xds_obj$terms$TaR[[s]][[i]] = TaR
    }
  return(xds_obj)
}

#' @title Blood feeding
#' @description Compute and store host available, \eqn{W_H},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi},
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal
BloodFeeding = function(t, y, xds_obj){
  UseMethod("BloodFeeding", xds_obj$blood_feeding_obj)
}

#' @title Compute blood feeding objects: static models
#' @description Return the blood feeding objects unmodified
#' @inheritParams BloodFeeding
#' @return an **`xds`** model object
#' @export
#' @keywords internal
BloodFeeding.static = function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Compute blood feeding objects dynamically
#' @description Update time-at-risk matrices and compute local and total
#' blood host availability.
#' @inheritParams BloodFeeding
#' @return an **`xds`** model object
#' @export
#' @keywords internal
BloodFeeding.dynamic = function(t, y, xds_obj){
  return(blood_feeding_dynamics(t, y, xds_obj))
}

#' @title Compute blood-feeding terms dynamically
#' @inherit BloodFeeding.dynamic description
#' @inheritParams BloodFeeding
#' @return an **`xds`** model object
#' @export
#' @keywords internal
blood_feeding_dynamics = function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- update_other_blood_hosts(t, y, xds_obj, s)
    xds_obj <- update_blood_traps(t, y, xds_obj, s)
  }
  for(i in 1:xds_obj$nHostSpecies){
    xds_obj <- update_blood_search_weights(t, y, xds_obj, i)
    xds_obj <- update_timespent(t, y, xds_obj, i)
  }
  xds_obj <- compute_TaR(xds_obj, t)
  xds_obj <- compute_WB(t, y, xds_obj)
  return(xds_obj)
}

#' @title Update the XY interface 
#' @description Recompute and store 
#' all the terms 
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_XY_interface = function(xds_obj){
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- compute_TaR(xds_obj, 0)
  xds_obj <- compute_WB(0, y, xds_obj)
  xds_obj <- compute_local_frac(xds_obj)
  xds_obj <- compute_beta(0, y, xds_obj)
  xds_obj <- compute_EIR(0, y, xds_obj)
  xds_obj <- compute_kappa(0, y, xds_obj)
  return(xds_obj)
}