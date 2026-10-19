



#' @title Compute eggs laid
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
EggLaying = function(t, y, xds_obj){
  UseMethod("EggLaying", xds_obj$habitats)
}

#' @title Compute eggs laid
#' @description Computes eggs laid for an autonomous model
#' @inheritParams EggLaying
#' @return an **`xds`** object
#' @export
#' @keywords internal
EggLaying.static = function(t, y, xds_obj){
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams EggLaying
#' @return an **`xds`** object
#' @export
#' @keywords internal
EggLaying.dynamic = function(t, y, xds_obj){
  return(egg_laying_dynamics)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams EggLaying
#' @return an **`xds`** object
#' @export
#' @keywords internal
egg_laying_dynamics = function(t, y, xds_obj){
  xds_obj = compute_U_matrix(xds_obj)
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute eggs laid
#' @description Computes eggs laid in aquatic habitats
#'
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @seealso [compute_eggs_laid]
#' @export
#' @keywords internal
compute_eggs_laid = function(t, y, xds_obj){
  with(xds_obj, {
    for(s in 1:nVectorSpecies){
      G = F_eggs(t, y, xds_obj, s)
      xds_obj$terms$G[[s]] = G
      Omatrix = habitats$laying_matrix[[s]]
      eta = F_eta(G, Omatrix, terms$Q[[s]], terms$Qall[[s]])
      xds_obj$terms$eta[[s]] = eta
      return(xds_obj)
    }
  })
}

#' @title Compute the Laying Matrix
#'
#' @description The egg laying matrix, \eqn{O}, allocates
#' a portion of eggs laid
#' by adult mosquitoes in a patch to each
#' one of the aquatic habitats in the patch
#'
#' @details The algorithm is motivated by
#' the notion of mosquito *searching* for aquatic habitat
#' and a quantity we call *habitat availability*, \eqn{Q}.
#' Each habitat is assigned a *search weight,* \eqn{\omega}.
#' In the simplest case, habitat availability is the sum of \eqn{\omega} by patch, which uses
#' the habitat membership matrix, \eqn{N}. If the habitats in the model got
#' all the eggs, then \deqn{Q = N \cdot \omega.}
#'
#' The fraction of eggs in the \eqn{j^{th}} patch laid
#' in the \eqn{i^{th}} habitat is \eqn{\omega_i / Q_j.}
#'
#' To make the model extensible, habitat availability sums all the places where
#' mosquitoes might lay eggs, including ovitraps and unsuitable habitats.
#' If every element in \eqn{Q} were positive, the egg
#' distribution matrix  would be:
#' \deqn{{O} = \mbox{diag}\left(\omega \right) \cdot {N}^T \cdot \mbox{diag} \left(\frac{1}{Q}\right).}
#' To avoid dividing by zero, the zero elements in \eqn{Q} are set to an arbitrary positive value.
#' @param search_weights the habitat search weights
#' @param habitat_matrix the habitat membership matrix
#' @param Q the availability of egg-laying habitats
#' @return a `nHabitats` \eqn{\times} `nPatches` [matrix] describing egg distribution, \eqn{O}
#' @seealso The membership matrix \eqn{N} is computed by [make_habitat_matrix]
#' @seealso Total habitat availability, \eqn{\cal Q}, is computed by [F_available_habitat]
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_egg_laying]
#' @export
#' @keywords internal
make_U_matrix= function(search_weights, habitat_matrix, Q){
  wts <- diag(as.vector(search_weights), length(search_weights))
  Qinv <- diag(1/as.vector(Q+1e-14), length(Q)) 
  lay_matrix = t(habitat_matrix %*% wts) %*% Qinv
  return(lay_matrix)
}



#' @title Compute the Egg Laying Matrix
#'
#' @description Compute the egg laying matrices
#' for all the vector species. The matrix determines
#' how all the eggs laid by adult mosquitoes are apportioned
#' among larval habitats
#'
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
#' @keywords internal
compute_U_matrix = function(xds_obj){with(xds_obj,{
  for(s in 1:nVectorSpecies){
    wts = L_obj[[s]]$search_weights
    N = habitats$matrix
    Q = terms$Q[[s]]
    xds_obj$habitats$laying_matrix[[s]] = make_U_matrix(wts, N, Q)
  }
  return(xds_obj)
})}

#' @title Eggs Laying in Habitats
#'
#' @description Computes egg distribution for the aquatic habitats
#'
#' @param eggs_laid the number of eggs laid in each patch, a vector of length `nPatches`
#' @param U_matrix the egg laying matrix
#' @param Q larval habitat availability
#' @param Qall total availability
#'
#' @return a [vector], \eqn{\eta} where \eqn{\left|\eta\right|=}`nHabitats`
#'
#' @seealso [compute_U_matrix]
#' @export
#' @keywords internal
F_eta = function(eggs_laid, U_matrix, Q, Qall){
  ix = which(Qall == 0)
  if(length(ix) > 0) Qall[ix]=1
  return(as.vector(U_matrix %*% (eggs_laid*Q/Qall)))
}
