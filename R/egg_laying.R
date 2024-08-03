# Egg laying

#' @title Create the habitat membership matrix, \eqn{\cal N}
#' @description The habitat membership matrix, \eqn{\cal N}, holds
#' information about the patch location of each habitat. It is part of
#' the egg-laying and emergence interface, making it possible
#' to compute egg laying from patches to habitats; and emergence from habitats to patches.
#' @details
#' Information about the patch location of each habitat
#' is passed as the membership vector, an ordered list of patch locations. If
#' the \eqn{i^{th}} habitat is in the \eqn{j^{th}} patch, then
#' \eqn{{\cal N}_{j,i}=1.} Otherwise, \eqn{{\cal N}_{j,i}=0.}
#'
#' Since \eqn{\cal N} is a matrix, it is readily used for computation. Let:
#' - \eqn{n_q = } `nHabitats`, the number of habitats;
#' - \eqn{n_p = } `nPatches`, the number of patches.
#'
#' If \eqn{w} is any vector describing a quantity in
#' habitats (*i.e.*, \eqn{\left|w\right|= n_q}), then
#' \deqn{W={\cal N}\cdot w} is a vector that has summed \eqn{w} by patch, and \eqn{\left|W\right|= N_p}.
#'
#' @param nPatches the number of patches, \eqn{N_p}
#' @param membership a vector describing the patch index for each habitat
#' @return the habitat membership [matrix], denoted \eqn{\cal N} where \eqn{\left|\cal N\right|= N_p \times N_q}
#' @seealso to extract habitat membership information, see [view_habitat_matrix()]
#' @examples
#' create_habitat_matrix(3, c(1,1,2,2,2))
#' @export
create_habitat_matrix = function(nPatches, membership){
  nHabitats = length(membership)
  habitat_matrix = matrix(0, nPatches, nHabitats)
  habitat_matrix[cbind(membership, 1:nHabitats)]=1
  return(habitat_matrix)
}

#' @title Setup Egg Laying
#' @description Set up a part of the `xds` object that defines the interface for egg laying
#' @details Modular computation in **`ramp.xds`** requires a rigid interface
#' to guarantee mathematical consistency for egg laying and emergence.
#' The interface is defined by an object called `EGGpar` that is
#' attached to the `xds` object `pars` as `pars$EGGpar`.
#' The interface includes
#' - a habitat membership matrix, \eqn{\cal N} made by [create_habitat_matrix]
#' - a quantity that is motivated by mosquito searching for resources, called
#' habitat availability \eqn{Q} made by [compute_Q];
#' - the egg distribution matrix \eqn{\cal U}, made by [compute_calU].
#'
#' This function is called by `make_xds_object` to set up `EGGpar` and the variables and parameters with all
#' the variables it might depend on.
#' @references{ This implements an enhanced version of the egg laying model in Equations 14-15 from \insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param pars an `xds` object
#' @param membership is the habitat membership vector
#' @return the modified `xds` object
#' @importFrom Rdpack reprompt
#' @seealso For a discussion of habitat availability, see [compute_Q()]
#' @seealso The habitat membership matrix is created by [create_habitat_matrix()]
#' @export
setup_EGG_LAYING = function(pars, membership){
  up <- list()
  class(up) <- "setup"

  wts <- rep(1, pars$nHabitats)
  calN <- pars$habitat_matrix
  up$search_weights = list()
  up$search_weights[[1]] <- wts

  Q = compute_Q(calN, wts)
  pars$vars$Q=list()
  pars$vars$Q[[1]] <- Q

  pars$calU = list()
  pars$calU[[1]] <- compute_calU(wts, calN, Q)

  pars$vars$Qtot=list()
  pars$vars$Qtot[[1]] <- Q

  pars$vars$ovitraps <- 0
  up$ovitrap_weights <- list()
  up$ovitrap_weights[[1]] = 1

  pars$vars$unsuitable_habitats <- 0
  up$bad_habitat_weights = list()
  up$bad_habitat_weights[[1]] = 1

  pars$eggs_laid = list()
  pars$eggs_laid[[1]] = 0

  pars$EGGpar <- up

  return(pars)
}

#' @title Change the habitat search weights
#' @description Set the search weights, \eqn{\omega}, for a set of aquatic habitats
#' @param pars an `xds` object
#' @param searchQ the habitat search weights
#' @param s the vector species index
#' @return the modified `xds` object
#' @export
change_habitat_weights = function(pars, searchQ=1, s=1){
  searchQ = checkIt(searchQ, pars$nHabitats)
  pars$EGGpar$search_weights[[s]] = checkIt(searchQ, pars$nHabitats, F)
  class(pars$EGGpar) <- 'setup'
  return(pars)
}

#' @title Compute the total availability of egg-laying habitats, \eqn{Q}
#' @description Compute the availability of aquatic habitats.
#' @details The availability of the habitats that we have defined in the model, denoted \eqn{Q}, sums search weights, \eqn{\omega_q}, by patch
#' using the habitat membership matrix, \eqn{\cal N}, and we can compute
#' \deqn{Q = {\cal N} \cdot \omega_q.}
#' @param habitat_matrix the membership matrix, \eqn{\cal N}
#' @param search_weights the habitat search weights, \eqn{\omega_q}
#' @return a [vector] of describing habitat availability, \eqn{Q}, of length `nPatches`
#' @seealso This function is called by [make_Q]
#' @seealso [create_habitat_matrix] discusses \eqn{\cal N}
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_EGG_LAYING]
#' @export
compute_Q = function(habitat_matrix, search_weights){
  Q <- habitat_matrix %*% search_weights
  return(as.vector(Q))
}

#' @title Compute the total availability of egg-laying habitats, \eqn{Q}
#' @description The sum of aquatic habitats and any other place
#' a mosquito might lay eggs, including ovitraps and unsuitable habitats.
#' @details The availability of the habitats that we have defined in the model, denoted \eqn{Q},
#' sums search weights, \eqn{\omega_h}, by patch
#' using the habitat membership matrix, \eqn{\cal N}, and we can compute
#' \deqn{Q = {\cal N} \cdot \omega_h.}
#' If some ovitraps and bad_habitats are also available, with values \eqn{Q_o} and \eqn{Q_b} respectively, then
#' \deqn{Q = Q_h + Q_o + Q_b.}
#' The availability of habitats, ovitraps and bad_habitats are computed elsewhere and stored on `pars$vars`.
#' @param Q the availability of ovitraps
#' @param Q_ovitraps the availability of ovitraps
#' @param Q_bad_habitats the availability of unsuitable habitats
#' @return a [vector] of describing habitat availability, \eqn{Q}, of length `nPatches`
#' @seealso This function is called by [make_Q]
#' @seealso [create_habitat_matrix] discusses \eqn{\cal N}
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_EGG_LAYING]
#' @export
compute_Qtot = function(Q, Q_ovitraps, Q_bad_habitats){
  Qtot <- Q + Q_ovitraps + Q_bad_habitats
  return(as.vector(Qtot))
}

#' @title Compute and store availability of egg-laying habitats
#' @description Set the value of a variable, \eqn{Q}, that describes the availability of any habitat
#' or device that would attract mosquitoes and induce them to lay eggs.
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @seealso [compute_Q]
#' @export
make_Q = function(pars){with(pars,{
  for(s in 1:nVectors){
    # Q describes available habitats
    w = EGGpar$search_weights[[s]]
    Q = compute_Q(habitat_matrix, w)
    pars$vars$Q[[s]] = Q
    # Qtot includes all
    Q_ovi = vars$ovitraps*EGGpar$ovitrap_weights[[s]]
    Q_bad_hab = vars$unsuitable_habitats*EGGpar$bad_habitat_weights[[s]]
    pars$vars$Qtot[[s]] = compute_Qtot(Q, Q_ovi, Q_bad_hab)
  }
  return(pars)
})}

#' @title Compute the egg distribution matrix - \eqn{\cal U}
#' @description The egg distribution matrix, \eqn{\cal U}, allocates a portion of eggs laid
#' by adult mosquitoes in each patch to each one of the aquatic habitats in the patch.
#' @details The algorithm is motivated by
#' the notion of mosquito *searching* for aquatic habitat
#' and a quantity we call *habitat availability*, \eqn{Q}.
#' Each habitat is assigned a *search weight,* \eqn{\omega}.
#' In the simplest case, habitat availability is the sum of \eqn{\omega} by patch, which uses
#' the habitat membership matrix, \eqn{\cal N}. If the habitats in the model got
#' all the eggs, then \deqn{Q = \cal N \cdot \omega.}
#' The fraction of eggs in the \eqn{j^{th}} patch laid
#' in the \eqn{i^{th}} habitat is \eqn{\omega_i / Q_j.}
#' To make the model extensible, habitat availability sums all the places where
#' mosquitoes might lay eggs, including ovitraps and unsuitable habitats.
#' If every element in \eqn{Q} were positive, the egg
#' distribution matrix  would be:
#' \deqn{{\cal U} = \mbox{diag}\left(\omega \right) \cdot {\cal N}^T \cdot \mbox{diag} \left(\frac{1}{Q}\right).}
#' To avoid dividing by zero, the zero elements in \eqn{Q} are set to an arbitrary positive value.
#' @param search_weights the habitat search weights
#' @param habitat_matrix the habitat membership matrix
#' @param Q the total availability of egg-laying habitats
#' @return a `nHabitats` \eqn{\times} `nPatches` [matrix] describing egg distribution, \eqn{\cal U}
#' @seealso The membership matrix \eqn{\cal N} is computed by [create_habitat_matrix]
#' @seealso Total habitat availability, \eqn{\cal Q}, is computed by [compute_Q]
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_EGG_LAYING]
#' @export
compute_calU = function(search_weights, habitat_matrix, Q){
  ix = which(Q == 0)
  if(length(ix)>0) Q[ix]=1
  calU = diag(search_weights) %*% t(habitat_matrix) %*% diag(1/as.vector(Q))
  return(calU)
}

#' @title Compute and store \eqn{\cal U}
#' @description Compute and store the egg distribution matrices
#' for all the vector species
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
make_calU = function(pars){
  for(s in 1:pars$nVectors)
    pars$calU[[s]] = compute_calU(pars$EGGpar$search_weights[[s]], pars$habitat_matrix, pars$vars$Q[[s]])
  return(pars)
}

#' @title Compute eggs laid
#' @description Computes egg distribution for the aquatic habitats
#' @param eggs_laid the number of eggs laid in each patch, a vector of length `nPatches`
#' @param calU the egg distribution matrix
#' @return a [vector], \eqn{\eta} where \eqn{\left|\eta\right|=}`nHabitats`
#' @seealso The egg distribution matrix [compute_calU]
#' @export
compute_eggs_laid = function(eggs_laid, calU){
  return(calU %*% eggs_laid)
}

#' @title Compute eggs laid
#' @description Computes eggs laid in aquatic habitats
#' @param t the time
#' @param y the state variables
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @seealso [compute_eggs_laid]
#' @export
make_eggs_laid = function(t, y, pars){
  for(s in 1:pars$nVectors)
    pars$eggs_laid[[s]] = compute_eggs_laid(F_eggs(t, y, pars, s), pars$calU[[s]])
  return(pars)
}

#' @title Compute eggs laid
#' @description Compute eggs laid at time \eqn{t}
#' @param t the time
#' @param y the state variables
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @seealso [setup_EGG_LAYING()]
#' @export
EggLaying = function(t, y, pars){
  UseMethod("EggLaying", pars$EGGpar)
}

#' @title Compute eggs laid, the first time
#' @description In autonomous models, this function gets called once to set up a variable describing host availability (\eqn{Q}) and
#' the egg distribution matrix (\eqn{\cal U}).
#' @details If conditions are time, invariant,
#' then [EggLaying.static] computes eggs laid. The functions that
#' compute \eqn{Q} and \eqn{\cal U} are called *once* after setup.
#' The class of `EGGpar` is set to `static` and the function
#' is never called again. If any parameters are changed that would
#' affect egg laying, then the class of `EGGpar` should get reset to `setup`
#' to reconfigure \eqn{Q} and \eqn{\cal U}.
#' @inheritParams EggLaying
#' @return the modified `xds` object
#' @seealso For \eqn{Q}, see [compute_Q]
#' @seealso For \eqn{\cal U}, see [compute_calU]
#' @export
EggLaying.setup = function(t, y, pars){
  class(pars$EGGpar) <- 'dynamic'
  pars <- EggLaying(t, y, pars)
  class(pars$EGGpar) <- 'static'
  return(pars)
}

#' @title Compute eggs laid
#' @description Computes eggs laid for an autonomous model
#' @inheritParams EggLaying
#' @return the modified `xds` object
#' @export
EggLaying.static = function(t, y, pars){
  pars = make_eggs_laid(t, y, pars)
  return(pars)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams EggLaying
#' @return the modified `xds` object
#' @export
EggLaying.dynamic = function(t, y, pars){
  pars = make_Q(pars)
  pars = make_calU(pars)
  pars = make_eggs_laid(t, y, pars)
  return(pars)
}

#' @title View habitat membership, \eqn{\cal N}
#' @description Output the habitat membership information as a list
#' @param pars an `xds` object
#' @return a [list]
#' @seealso [create_habitat_matrix()]
#' @export
view_habitat_matrix = function(pars){
  which(t(pars$habitat_matrix)==1, arr.ind=TRUE) -> membership
  member <- list(habitat_index = as.vector(membership[,1]), patch_membership = as.vector(membership[,2]))
  return(member)
}

