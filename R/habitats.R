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
#' - the egg distribution matrix \eqn{O}, made by [make_O_matrix]
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




