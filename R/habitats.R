# The ML-interface. Also see:
# + egg_laying.R 
# + emergence.R 

#' @title Check the ML Interface 
#' 
#' @description Run a set of consistency checks for the `ML_interface`
#' 
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal  
check_ML_interface = function(xds_obj){
  return(xds_obj) 
}


#' @title Setup the Habitat Interface for Egg Laying and Emergence  
#' 
#' @description Set up a part of the `xds` object that defines the interface for egg laying
#' @details
#' This implements a model for egg laying described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in **`ramp.xds`** requires a rigid interface
#' to guarantee mathematical consistency for egg laying and emergence.
#' The interface is defined by an object called `ML_interface` that is
#' attached to the `xds` object `xds_obj` as `xds_obj$ML_interface`.
#' The interface includes
#' - a habitat membership matrix, \eqn{N} made by [make_habitat_matrix]
#' - the habitat search weights
#' - a quantity that is motivated by mosquito searching for resources, called
#' habitat availability \eqn{Q}, computed by [F_Q];
#' - the availability of ovitraps
#' - the availability of unsuitable habitats
#' - the availability of anything that attracts egg laying mosquitoes, including ovitraps and unsuitable habitats
#' - the egg distribution matrix \eqn{O}, made by [make_O_matrix]
#' - a vector that stores eggs laid
#'
#' This function is called by `compute_xds_object_template` to set up `ML_interface` and the variables and parameters with all
#' the variables it might depend on.
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param xds_obj an **`xds`** model object
#' @param membership is the habitat membership vector
#' @return an `xds` object
#' @importFrom Rdpack reprompt
#' @seealso The habitat membership matrix is created by [make_habitat_matrix()]
#' @keywords internal
#' @export
setup_ML_interface = function(xds_obj, membership){
  #Egg Laying Terms 
  xds_obj$terms$G <- list() 
  xds_obj$terms$G[[1]] <- rep(0, xds_obj$nPatches) 
  xds_obj$terms$eta <- list() 
  xds_obj$terms$eta[[1]] <- rep(0, xds_obj$nHabitats) 
  
  #Emergence Terms 
  xds_obj$terms$alpha <- list() 
  xds_obj$terms$alpha[[1]] <- rep(0, xds_obj$nHabitats) 
  xds_obj$terms$Lambda <- list() 
  xds_obj$terms$Lambda[[1]] <- rep(0, xds_obj$nPatches) 
  
  interface <- list()
  class(interface) <- "setup"
  
  habitat_matrix = make_habitat_matrix(xds_obj$nPatches, membership)
  
  interface$habitat_matrix <- habitat_matrix
  
  wts <- rep(1, xds_obj$nHabitats)
  interface$search_weights = list()
  interface$search_weights[[1]] <- wts
  
  Q = F_Q(habitat_matrix, wts)
  
  interface$laying_matrix = list()
  interface$laying_matrix[[1]] <- make_O_matrix(wts, habitat_matrix, Q)
  
  interface$Q = list()
  interface$Q[[1]] <- Q
  
  interface$Qbad = list() 
  interface$Qbad[[1]] = rep(0, xds_obj$nPatches)
  
  interface$Qall=list()
  interface$Qall[[1]] <- Q
  
  
  xds_obj$ML_interface <- interface



  
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
#' @seealso compute_habitat matrix is called by [make_xds_object_template()] and [setup_ML_interface()]
#' @seealso see [view_habitat_matrix()]
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

#' @title View habitat membership, \eqn{N}
#' @description Output the habitat membership information as a list
#' @param xds_obj an **`xds`** model object
#' @return a [list]
#' @seealso [make_habitat_matrix()]
#' @export
view_habitat_matrix = function(xds_obj){
  which(t(xds_obj$ML_interface$habitat_matrix)==1, arr.ind=TRUE) -> membership
  member <- list(habitat_index = as.vector(membership[,1]), patch_membership = as.vector(membership[,2]))
  return(member)
}


