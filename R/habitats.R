#' @title Setup the aquatic habitats
#'
#' @description Set up a part of the `xds` object that defines the interface for egg laying
#' @details
#' This implements a model for egg laying described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in **`ramp.xds`** requires a rigid interface
#' to guarantee mathematical consistency for egg laying and emergence.
#' The interface is defined by an object called `egg_laying` that is
#' attached to the `xds` object `xds_obj` as `xds_obj$egg_laying`.
#' The interface includes
#' - a habitat membership matrix, \eqn{N} made by [make_habitat_matrix]
#' - the habitat search weights
#' - a quantity that is motivated by mosquito searching for resources, called
#' habitat availability \eqn{Q}, computed by [F_available_habitat];
#' - the availability of ovitraps
#' - the availability of unsuitable habitats
#' - the availability of anything that attracts egg laying mosquitoes, including ovitraps and unsuitable habitats
#' - the egg distribution matrix \eqn{O}, made by [make_U_matrix]
#' - a vector that stores eggs laid
#'
#' This function is called by `compute_xds_object_template` to set up `egg_laying` and the variables and parameters with all
#' the variables it might depend on.
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param xds_obj an **`xds`** model object
#' @param membership the membership vector
#' 
#' @return an **`xds`** object
#' 
#' @importFrom Rdpack reprompt
#' @seealso The habitat membership matrix is created by [make_habitat_matrix()]
#' @keywords internal
#' @export
setup_habitats = function(xds_obj, membership){

  # Habitat and Laying Matrices
  xds_obj$habitats = make_static_obj()
  xds_obj$habitats$membership = membership
  xds_obj$habitats$matrix = make_habitat_matrix(xds_obj$nPatches, membership)
  xds_obj$habitats$laying_matrix = list()

  return(xds_obj)
}

#' @title Create the habitat membership matrix, \eqn{N}
#' @description The habitat membership matrix, \eqn{N}, holds
#' information about the patch location of each habitat. It is part of
#' the egg-laying and emergence interface, making it possible
#' to compute egg laying from patches to habitats; and emergence from habitats to patches.
#' @details
#' Information about the patch location of each habitat
#' is passed as the membership vector, an ordered list of patch locations. If
#' the \eqn{i^{th}} habitat is in the \eqn{j^{th}} patch, then
#' \eqn{{N}_{j,i}=1.} Otherwise, \eqn{{N}_{j,i}=0.}
#'
#' Since \eqn{N} is a matrix, it is readily used for computation. Let:
#' - \eqn{n_q = } `nHabitats`, the number of habitats;
#' - \eqn{n_p = } `nPatches`, the number of patches.
#'
#' If \eqn{w} is any vector describing a quantity in
#' habitats (*i.e.*, \eqn{\left|w\right|= n_q}), then
#' \deqn{W={N}\cdot w} is a vector that has summed \eqn{w} by patch, and \eqn{\left|W\right|= n_p}.
#'
#' @param nPatches the number of patches, \eqn{n_p}
#' @param membership a vector describing the patch index for each habitat
#' @return the habitat membership [matrix], denoted \eqn{N} where \eqn{\left|N\right|= n_p \times n_q}
#' @seealso compute_habitat matrix is called by [make_xds_object_template()] and [setup_egg_laying()]
#' @seealso see [get_habitat_matrix()]
#' @examples
#' make_habitat_matrix(3, c(1,1,2,2,2))
#' @export
#' @keywords internal
make_habitat_matrix = function(nPatches, membership){
  nHabitats = length(membership)
  habitat_matrix = matrix(0, nPatches, nHabitats)
  habitat_matrix[cbind(membership, 1:nHabitats)]=1
  return(habitat_matrix)
}

#' @title Get the habitat membership vector 
#' @description Output the habitat membership information as a list
#' @param xds_obj an **`xds`** model object
#'  
#' @return the patch membership for aquatic habitats, a numeric vector
#' @seealso [make_habitat_matrix()]
#' @export
get_habitats = function(xds_obj){
  return(xds_obj$habitats$membership)
}


#' @title Get habitat matrix, \eqn{N}
#' @description Output the habitat membership information as a list
#' @param xds_obj an **`xds`** model object
#' @return a [matrix]
#' @seealso [make_habitat_matrix()]
#' @export
get_habitat_matrix = function(xds_obj){
  return(xds_obj$habitats$matrix)
}


#' @title Aquatic Habitats
#'
#' @description Compute eggs laid at time \eqn{t}
#'
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @seealso [setup_egg_laying()]
#' @export
#' @keywords internal
AquaticHabitats = function(t, y, xds_obj){
  UseMethod("AquaticHabitats", xds_obj$habitats)
}

#' @title Compute eggs laid
#' @description Computes eggs laid for an autonomous model
#' @inheritParams AquaticHabitats
#' @return an **`xds`** object
#' @export
#' @keywords internal
AquaticHabitats.static = function(t, y, xds_obj){
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams AquaticHabitats
#' @return an **`xds`** object
#' @export
#' @keywords internal
AquaticHabitats.dynamic = function(t, y, xds_obj){
  return(habitat_dynamics)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams EggLaying
#' @return an **`xds`** object
#' @export
#' @keywords internal
habitat_dynamics = function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj = update_habitat_search_weights(t, y, xds_obj, s) 
    xds_obj = update_bad_habitats(t, y, xds_obj, s)
    xds_obj = update_ovitraps(t, y, xds_obj, s)
  }
  xds_obj = compute_all_available_water(xds_obj)
  return(xds_obj)
}


#' @title Compute and store availability of egg-laying habitats
#' @description Set the value of a variable, \eqn{Q}, that describes the availability of any habitat
#' or device that would attract mosquitoes and induce them to lay eggs.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @seealso [F_available_habitat]
#' @export
#' @keywords internal
compute_all_available_water = function(xds_obj){with(xds_obj,{
  for(s in 1:xds_obj$nVectorSpecies){
    Q = F_available_habitat(habitats$matrix, L_obj[[s]]$search_weights)
    xds_obj$terms$Q[[s]] = Q
    Qall = F_all_available_water(Q, patches$bad_habitats[[s]], patches$ovitraps[[s]])
    xds_obj$terms$Qall[[s]] = Qall
  }
  return(xds_obj)
})}



#' @title Compute Habitat Availability
#'
#' @description Compute the availability of aquatic habitats.
#'
#' @details The availability of the habitats that we have defined in the model, denoted \eqn{Q}, sums search weights, \eqn{\omega_q}, by patch
#' using the habitat membership matrix, \eqn{N}, and we can compute
#' \deqn{Q = {N} \cdot \omega_q.}
#' @param habitat_matrix the membership matrix, \eqn{N}
#' @param search_weights the habitat search weights, \eqn{\omega_q}
#' @return a [vector] of describing habitat availability, \eqn{Q}, of length `nPatches`
#' @seealso This function is called by [compute_all_available_water]
#' @seealso [make_habitat_matrix] discusses \eqn{N}
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_egg_laying]
#' @export
#' @keywords internal
F_available_habitat = function(habitat_matrix, search_weights){
  Q <- habitat_matrix %*% search_weights
  return(as.vector(Q))
}

#' @title Compute Available Laying Sites, \eqn{O}
#'
#' @description Mosquitoes lay eggs in aquatic habitats, in
#' water bodies that are unsuitable for larval development
#' (bad habitats), and in ovitraps.
#'
#' @details Habitat availability denoted \eqn{Q},
#' sums habitat search weights, \eqn{\omega}, by patch
#' using the habitat membership matrix, \eqn{N}:
#' \deqn{Q = {N} \cdot \omega_h.}
#'
#' If some ovitraps (\eqn{Q_t}) and bad_habitats (\eqn{Q_b}) are
#' also available, then total availability of laying sites is
#' \deqn{O = Q + Q_t + Q_b.}
#'
#' The availability of habitats, ovitraps and bad_habitats are computed elsewhere and stored on `xds_obj$vars`.
#'
#' @param Q the availability of habitats
#' @param Q_traps the availability of traps
#' @param Q_bad the availability of unsuitable habitats
#'
#' @return  Availability laying sites, \eqn{O}
#'
#' @seealso This function is called by [compute_all_available_water]
#' @seealso [make_habitat_matrix] discusses \eqn{N}
#'
#' @seealso The availability of traps and bad habitats is setup in [setup_egg_laying]
#' @export
#' @keywords internal
F_all_available_water = function(Q, Q_traps, Q_bad){
  Qall <- Q + Q_traps + Q_bad
  return(as.vector(Qall))
} 



