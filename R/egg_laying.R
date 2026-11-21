# X 

#' @title Compute eggs laid in aquatic habitats
#'
#' @description Compute the number of 
#' eggs laid at time \eqn{t} in each habitat.
#' 
#' @inheritSection habitat_matrix Habitat Matrix 
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#' @seealso [setup_egg_laying_interface()]
#' @export
#' @keywords internal
EggLaying = function(t, y, xds_obj){
  UseMethod("EggLaying", xds_obj$habitats)
}

#' @title Compute eggs laid in aquatic habitats
#' @description 
#' Compute egg laying when the laying matrix doesn't change.
#' 
#' @inheritParams EggLaying
#' @return an **`xds`** model object
#' @export
#' @keywords internal
EggLaying.static = function(t, y, xds_obj){
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on a parameter affecting habitat availability.
#' @inheritParams EggLaying
#' @return an **`xds`** model object
#' @export
#' @keywords internal
EggLaying.dynamic = function(t, y, xds_obj){
  return(egg_laying_dynamics(t, y, xds_obj))
}

#' @title Egg laying dynamics 
#' 
#' @description 
#' Update the laying matrix and compute eggs laid.
#' 
#' @seealso [laying_matrix], [egg_laying]
#' 
#' @inheritParams EggLaying
#' @return an **`xds`** model object
#' @export
#' @keywords internal
egg_laying_dynamics = function(t, y, xds_obj){
  xds_obj = update_U_matrix(xds_obj)
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute egg laying
#' 
#' @description Computes eggs laid in aquatic habitats.
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
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
    }
    return(xds_obj)
  })
}

#' @title Make the laying matrix
#'
#' @description 
#' 
#' Make the egg laying matrix \eqn{(\mathbf {U})}.
#' 
#' @inheritSection habitat_matrix Habitat Matrix
#' @inheritSection habitat_availability Habitat Availability
#' @inheritSection laying_matrix Laying Matrix 
#' 
#' @param search_weights the habitat search weights
#' @param habitat_matrix the habitat membership matrix
#' @param Q the availability of egg-laying habitats
#' @return an `nHabitats` \eqn{\times} `nPatches` [matrix] describing egg distribution, \eqn{U}
#' @seealso The membership matrix \eqn{N} is computed by [make_habitat_matrix]
#' @seealso Total habitat availability, \eqn{Q}, is computed by [F_available_habitat]
#' @seealso The availability of ovitraps and bad habitats is set up in [setup_egg_laying_interface]
#' 
#' 
#' @export
#' @keywords internal
make_U_matrix= function(search_weights, habitat_matrix, Q){
  wts <- diag(as.vector(search_weights), length(search_weights))
  Qinv <- diag(1/as.vector(Q+1e-14), length(Q)) 
  lay_matrix = t(habitat_matrix %*% wts) %*% Qinv
  return(lay_matrix)
}

#' @title Update the laying matrices
#'
#' @description Update the stored egg laying matrices
#' for all the vector species. 
#' 
#' @seealso [habitat_matrix], [habitat_availability], [make_U_matrix]
#'
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_U_matrix = function(xds_obj){with(xds_obj,{
  for(s in 1:nVectorSpecies){
    wts = L_obj[[s]]$search_weights
    N = habitats$matrix
    Q = terms$Q[[s]]
    xds_obj$habitats$laying_matrix[[s]] = make_U_matrix(wts, N, Q)
  }
  return(xds_obj)
})}

#' @title Egg Laying in Habitats
#'
#' @description Computes egg distribution for the aquatic habitats.
#'
#' @param eggs_laid the number of eggs laid in each patch, a vector of length `nPatches`
#' @param U_matrix the egg laying matrix
#' @param Q larval habitat availability
#' @param Qall total availability
#'
#' @return a [vector], \eqn{\eta} where \eqn{\left|\eta\right|=}`nHabitats`
#'
#' @seealso [update_U_matrix]
#' @export
#' @keywords internal
F_eta = function(eggs_laid, U_matrix, Q, Qall){
  ix = which(Qall == 0)
  if(length(ix) > 0) Qall[ix]=1
  return(as.vector(U_matrix %*% (eggs_laid*Q/Qall)))
}
