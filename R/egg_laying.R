# Egg laying

#' @title Setup the interface for egg laying
#' @description Sets up the object that defines the egg laying interface for an `xds` model object.
#' @details Modular computation `ramp.xds` defines a rigid interface
#' to compute egg laying and emergence for models of mosquito ecology.
#' Implementation is defined by an object called `EGGpar` found
#' attached to the `xds` model object `pars` as `pars$EGGpar`.
#' The interface was developed around a habitat
#' membership matrix, \eqn{\cal N}, a quantity called
#' habitat availability \eqn{Q}, and an egg deposition matrix \eqn{\cal U}.
#' This function is called by `make_xds_object` to set up `EGGpar` and the variables and parameters with all
#' the variables it might depend on.
#' @param pars the model object
#' @return the modified xds model object, as a compound [list]
#' @export
setup_EGG_LAYING = function(pars){

  up <- list()
  class(up) <- "setup"
  pars$EGGpar <- up

  pars$EGGpar$search_weights = list()
  pars$EGGpar$search_weights[[1]] <- 1

  pars$calU = list()
  pars$calU[[1]] <- diag(1)

  pars$vars$Q=list()
  pars$vars$Q[[1]] <- 1

  pars$vars$ovitraps <- 0
  pars$EGGpar$ovitrap_weights <- list()
  pars$EGGpar$ovitrap_weights[[1]] = 1

  pars$vars$unsuitable_habitats <- 0
  pars$EGGpar$bad_habitat_weights = list()
  pars$EGGpar$bad_habitat_weights[[1]] = 1

  pars$eggs_laid = list()
  pars$eggs_laid[[1]] = 0

  return(pars)
}

#' @title Setup egg laying for most models
#' @description Sets up the egg-deposition matrix calU for the s^th species
#' @param pars the model object
#' @param searchQ the membership matrix
#' @param s the vector species index
#' @param Lopts a [list] of options to override defaults
#' @return a [list] vector
#' @export
setup_egg_laying_static = function(pars, searchQ=1, s=1, Lopts=list()){with(Lopts,{
  # Habitat search weights
  searchQ = checkIt(searchQ, pars$nHabitats)
  pars$EGGpar$search_weights[[s]] = checkIt(searchQ, pars$nHabitats, F)
  return(pars)
})}

#' @title Create the habitat membership matrix, \eqn{\cal N}
#' @description The habitat membership matrix, \eqn{\cal N}, holds
#' information about the patch location for each habitats.
#' If \eqn{w} is any vector describing a quantity in habitats (*i.e.*, \eqn{\left|w\right|=} `nHabitats`), then
#' \eqn{W={\cal N}\cdot w} is a vector that has summed \eqn{w} by patch, and \eqn{\left|W\right|=} `nPatches`.
#' @details Information about the patch location of each habitat
#' is passed as the membership vector, an ordered list of patch locations. If
#' the \eqn{i^{th}} habitat is in the \eqn{j^{th}} patch, then
#' \eqn{{\cal N}[j,i]=1.} Otherwise, \eqn{{\cal N}[j,i]=0.}
#' @param nPatches the number of patches
#' @param membership a vector describing the patch index for each habitat
#' @return the membership [matrix], denoted \eqn{\cal N} where \eqn{\left|\cal N\right|=}`nPatches` \eqn{\times} `nHabitats`
#' @seealso to extract habitat membership information, see [view_membership_matrix]
#' @export
create_membership_matrix = function(nPatches, membership){
  nHabitats = length(membership)
  membership_matrix = matrix(0, nPatches, nHabitats)
  membership_matrix[cbind(membership, 1:nHabitats)]=1
  return(membership_matrix)
}

#' @title Compute the total availability of egg-laying habitats, \eqn{Q}
#' @description The sum of all available aquatic habitats and any other place
#' where a mosquito might lay eggs, including ovitraps and unsuitable habitats.
#' @details The availability of the habitats that we have defined in the model, denoted \eqn{Q_h}, sums search weights, \eqn{\omega}, by patch
#' using the habitat membership matrix, \eqn{\cal N}, and we can compute
#' \deqn{Q_h = {\cal N} \cdot \omega.}
#' If some ovitraps and bad_habitats are also available, with values \eqn{Q_o} and \eqn{Q_b} respectively, then
#' \deqn{Q = Q_h + Q_o + Q_b.}
#' The availability of habitats, ovitraps and bad_habitats are computed elsewhere and stored on `pars$vars`.
#' @param membership_matrix the membership matrix
#' @param search_weights the habitat search weights
#' @param Q_ovitraps the availability of ovitraps
#' @param Q_bad_habitats the availability of unsuitable habitats
#' @return a [vector] of length `nPatches`
#' @seealso This function is called by [make_Q]
#' @seealso [create_membership_matrix] discusses \eqn{\cal N}
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_EGG_LAYING]
#' @export
compute_Q = function(membership_matrix, search_weights, Q_ovitraps, Q_bad_habitats){
  Q <- membership_matrix %*% search_weights + Q_ovitraps + Q_bad_habitats
  return(Q)
}

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
#' deposition matrix  would be:
#' \deqn{{\cal U} = \mbox{diag}\left(\omega \right) \cdot {\cal N}^T \cdot \mbox{diag} \left(\frac{1}{Q}\right).}
#' To avoid dividing by zero, the zero elements in \eqn{Q} are set to an arbitrary positive value.
#' @param search_weights the habitat search weights
#' @param membership_matrix the habitat membership matrix
#' @param Q the total availability of egg-laying habitats
#' @return a `nHabitats` \eqn{\times} `nPatches` [matrix] describing egg deposition, \eqn{\cal U}
#' @seealso The membership matrix \eqn{\cal N} is computed by [create_membership_matrix]
#' @seealso Total habitat availability, \eqn{\cal Q}, is computed by [compute_Q]
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_EGG_LAYING]
#' @export
compute_calU = function(search_weights, membership_matrix, Q){
  ix = which(Q == 0)
  if(length(ix)>0) Q[ix]=1
  calU = diag(search_weights) %*% t(membership_matrix) %*% diag(1/as.vector(Q))
  return(calU)
}

#' @title Compute eggs laid
#' @description Computes egg deposition for the aquatic habitats
#' @param eggs_laid the number of eggs laid in each patch, a vector of length `nPatches`
#' @param calU the egg deposition matrix
#' @return a [vector], \eqn{\eta} where \eqn{\left|\eta\right|=}`nHabitats`
#' @seealso The egg deposition matrix [compute_calU]
compute_eggs_laid = function(eggs_laid, calU){
  return(calU %*% eggs_laid)
}


#' @title Compute eggs laid
#' @description Compute eggs laid at time \eqn{t}
#' @param t the time
#' @param y the state variables
#' @param pars the model object
#' @return a [list]
#' @export
EggLaying = function(t, y, pars){
  UseMethod("EggLaying", pars$EGGpar)
}

#' @title Compute eggs laid, the first time
#' @description In autonomous models, this function gets called once to set up a variable describing host availability (\eqn{Q}) and
#' the egg deposition matrix (\eqn{\cal U}).
#' @details If conditions are time, invariant,
#' then [EggLaying.static] computes eggs laid. The functions that
#' compute \eqn{Q} and \eqn{\cal U} are called *once* after setup.
#' The class of `EGGpar` is set to `static` and the function
#' is never called again. If any parameters are changed that would
#' affect egg laying, then the class of `EGGpar` should get reset to `setup`
#' to reconfigure \eqn{Q} and \eqn{\cal U}.
#' @inheritParams EggLaying
#' @return a [list]
#' @seealso For \eqn{Q}, see [compute_Q]
#' @seealso For \eqn{\cal U}, see [compute_calU]
#' @export
EggLaying.setup = function(t, y, pars){
  pars = make_Q(pars)
  pars = make_calU(pars)
  class(pars$EGGpar) <- 'static'
  EggLaying(t, y, pars)
}

#' @title Compute eggs laid
#' @description Computes eggs laid for an autonomous model
#' @inheritParams EggLaying
#' @return a [list]
#' @export
EggLaying.static = function(t, y, pars){
  pars = make_eggs_laid(t, y, pars)
  return(pars)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams EggLaying
#' @return a [list]
#' @export
EggLaying.dynamic = function(t, y, pars){
  pars = make_Q(pars)
  pars = make_calU(pars)
  pars = make_eggs_laid(t, y, pars)
  return(pars)
}

#' @title Compute and store \eqn{\cal U}
#' @description Compute and store the egg deposition matrices
#' for all the vector species
#' @param pars the model object
#' @return a [list]
make_calU = function(pars){
  for(s in 1:pars$nVectors)
    pars$calU[[s]] = compute_calU(pars$EGGpar$search_weights[[s]], pars$membership_matrix, pars$vars$Q[[s]])
  return(pars)
}

#' @title Compute and store availability of egg-laying habitats
#' @description Set the value of a variable, \eqn{Q}, that describes the availability of any habitat
#' or device that would attract mosquitoes and induce them to lay eggs.
#' @param pars the model object
#' @return a [list]
make_Q = function(pars){with(pars,{
  for(s in 1:nVectors){
    w = EGGpar$search_weights[[s]]
    Q_ovi = vars$ovitraps*EGGpar$ovitrap_weights[[s]]
    Q_bad_hab = vars$unsuitable_habitats*EGGpar$bad_habitat_weights[[s]]
    pars$vars$Q[[s]] = compute_Q(membership_matrix, w, Q_ovi, Q_bad_hab)
  }
  return(pars)
})}


#' @title Compute eggs laid
#' @description Computes eggs laid for the s^th species
#' @param t the time
#' @param y the state variables
#' @param pars the model object
#' @return a [list]
make_eggs_laid = function(t, y, pars){
  for(s in 1:pars$nVectors)
    pars$eggs_laid[[s]] = compute_eggs_laid(F_eggs(t, y, pars, s), pars$calU[[s]])
  return(pars)
}


#' @title View habitat membership
#' @description Shows the habitat membership information from \eqn{\cal N}
#' @param pars an xds model object
#' @return a `nHabitats` \eqn{\times 2} [matrix]: col 1 is the habitat index, and col 2 is the patch index
#' @seealso [create_membership_matrix]
view_membership_matrix = function(pars){
  which(t(pars$membership_matrix)==1, arr.ind=TRUE)[,2] -> membership
  colnames(membership) <- c("H_i", "P_j")[,2]
  return(membership)
}

