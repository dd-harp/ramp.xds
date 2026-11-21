# X 

#' @title Set up the aquatic habitats
#'
#' @description Set up a part of the `xds` object that defines the interface for egg laying.
#' 
#' @details
#' Store the habitat vector, set up the habitat matrix, and initialize
#' the laying matrix list. 
#' 
#' @inheritSection habitat_vector Habitat Vector
#' @inheritSection habitat_matrix Habitat Matrix
#'  
#' @param xds_obj an **`xds`** model object
#' @param membership a vector of counting numbers
#' 
#' @return an **`xds`** model object
#' 
#' @importFrom Rdpack reprompt
#' @seealso The habitat membership matrix is created by [make_habitat_matrix()]
#' @keywords internal
#' @export
setup_habitats = function(xds_obj, membership){
  stopifnot("Each habitat index should be a positive integer."=
            membership>0 & 
            abs(membership - round(membership))<.Machine$double.eps^0.5)
  
  # Habitat and Laying Matrices
  xds_obj$habitats = make_static_obj()
  xds_obj$habitats$membership = membership
  xds_obj$habitats$matrix = make_habitat_matrix(xds_obj$nPatches, membership)
  xds_obj$habitats$laying_matrix = list()

  return(xds_obj)
}

#' @title Make the habitat matrix 
#' 
#' @description Make the habitat matrix \eqn{(\mathbf{N})} from the habitat vector.
#' 
#' @inheritSection habitat_vector Habitat Vector
#' 
#' @inheritSection habitat_matrix Habitat Matrix
#' 
#' @param nPatches the number of patches
#' @param membership a habitat vector (see [habitat_vector])
#' @return the habitat matrix
#' @seealso [get_habitat_matrix()]
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
#' 
#' @description Inspect the habitat membership vector.
#' 
#'  
#' @param xds_obj an **`xds`** model object
#'  
#' @return the patch membership for aquatic habitats, a numeric vector
#' @seealso [habitat_vector]
#' @export
get_habitats = function(xds_obj){
  return(xds_obj$habitats$membership)
}


#' @title Get habitat matrix, \eqn{N}
#' @description Inspect the habitat membership matrix.
#' @param xds_obj an **`xds`** model object
#' @return a [matrix]
#' @seealso [make_habitat_matrix()], [habitat_matrix]
#' @export
get_habitat_matrix = function(xds_obj){
  return(xds_obj$habitats$matrix)
}


#' @title Aquatic Habitat Dynamics
#'
#' @description Update habitat search weights, unsuitable habitats,
#' and ovitraps, and update resource availability for egg laying.
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#' @seealso [setup_egg_laying_interface()]
#' @export
#' @keywords internal
AquaticHabitats = function(t, y, xds_obj){
  UseMethod("AquaticHabitats", xds_obj$habitats)
}

#' @title Aquatic Habitat Dynamics
#' @description Computes eggs laid leaving the 
#' habitat interface unmodified. 
#' @inheritParams AquaticHabitats
#' @return an **`xds`** model object
#' @export
#' @keywords internal
AquaticHabitats.static = function(t, y, xds_obj){
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

#' @title Aquatic Habitat Dynamics
#' @description Updates habitat availability with exogenous forcing
#' on a parameter affecting habitat availability.
#' @inheritParams AquaticHabitats
#' @return an **`xds`** model object
#' @export
#' @keywords internal
AquaticHabitats.dynamic = function(t, y, xds_obj){
  return(habitat_dynamics(t,y,xds_obj))
}

#' @title Habitat Dynamics 
#' 
#' @description Update habitat search weights, unsuitable habitats,
#' and ovitraps, and update resource availability for egg laying.
#' 
#' @inheritParams EggLaying
#' @return an **`xds`** model object
#' @export
#' @keywords internal
habitat_dynamics = function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj = update_habitat_search_weights(t, y, xds_obj, s) 
    xds_obj = update_bad_habitats(t, y, xds_obj, s)
    xds_obj = update_ovitraps(t, y, xds_obj, s)
  }
  xds_obj = update_all_available_water(xds_obj)
  return(xds_obj)
}


#' @title Update availability of egg-laying habitats
#' @description Set the values of \eqn{Q} and \eqn{Q_{all}}, which describe the availability of any habitat
#' or device that would attract mosquitoes and induce them to lay eggs.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @seealso [F_available_habitat]
#' @export
#' @keywords internal
update_all_available_water = function(xds_obj){with(xds_obj,{
  for(s in 1:xds_obj$nVectorSpecies){
    Q = F_available_habitat(habitats$matrix, L_obj[[s]]$search_weights)
    xds_obj$terms$Q[[s]] = Q
    Qall = F_all_available_water(Q, patches$bad_habitats[[s]], patches$ovitraps[[s]])
    xds_obj$terms$Qall[[s]] = Qall
  }
  return(xds_obj)
})}



#' @title Compute habitat availability
#'
#' @description This function computes habitat availability.
#'
#' @details The availability of the habitats that we have defined in the model, denoted \eqn{Q}, sums search weights, \eqn{w}, by patch
#' using the habitat membership matrix, \eqn{N}, and we can compute
#' \deqn{Q = {N} \cdot w.}
#' @param habitat_matrix the membership matrix, \eqn{N}
#' @param search_weights the habitat search weights, \eqn{w}
#' @return a [vector] describing habitat availability, \eqn{Q}, of length `nPatches`
#' @seealso This function is called by [update_all_available_water]
#' @seealso [make_habitat_matrix] discusses \eqn{N}
#' @seealso The availability of ovitraps and bad habitats is set up in [setup_egg_laying_interface]
#' @export
#' @keywords internal
F_available_habitat = function(habitat_matrix, search_weights){
  Q <- habitat_matrix %*% search_weights
  return(as.vector(Q))
}

#' @title Compute Available Laying Sites, \eqn{Q_{all}}
#'
#' @description Mosquitoes lay eggs in aquatic habitats, in
#' water bodies that are unsuitable for larval development
#' (bad habitats), and in ovitraps.
#'
#' @details Habitat availability denoted \eqn{Q},
#' sums habitat search weights, \eqn{w}, by patch
#' using the habitat membership matrix, \eqn{N}:
#' \deqn{Q = {N} \cdot w.}
#'
#' If some ovitraps (\eqn{Q_t}) and bad_habitats (\eqn{Q_b}) are
#' also available, then total availability of laying sites is
#' \deqn{Q_{all} = Q + Q_t + Q_b.}
#'
#' Habitat availability is computed elsewhere and stored in `xds_obj$terms$Q`, ovitrap availability in `xds_obj$patches$ovitraps`, and bad-habitat availability in `xds_obj$patches$bad_habitats`.
#'
#' @param Q the availability of habitats
#' @param Q_traps the availability of traps
#' @param Q_bad the availability of unsuitable habitats
#'
#' @return Availability of laying sites, \eqn{Q_{all}}
#'
#' @seealso This function is called by [update_all_available_water]
#' @seealso [make_habitat_matrix] discusses \eqn{N}
#'
#' @seealso The availability of traps and bad habitats is set up in [setup_egg_laying_interface]
#' @export
#' @keywords internal
F_all_available_water = function(Q, Q_traps, Q_bad){
  Qall <- Q + Q_traps + Q_bad
  return(as.vector(Qall))
} 
