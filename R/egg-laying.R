# Habitat Availability and Egg Laying. Also see  
# + habitats.R 
# + ovitraps.R 

#' @title Compute eggs laid
#' 
#' @description Compute eggs laid at time \eqn{t}
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' @seealso [setup_ML_interface()]
#' @export
#' @keywords internal
EggLaying = function(t, y, xds_obj){
  UseMethod("EggLaying", xds_obj$ML_interface)
}

#' @title Compute eggs laid, the first time
#' 
#' @description In autonomous models, this 
#' function gets called after the `ML_interface` 
#' is set up, or after any structural element in
#' the ML_interface changes 
#' 
#' describing host availability (\eqn{Q}) and
#' the egg distribution matrix (\eqn{O}).
#' 
#' @details If conditions are time, invariant,
#' then [EggLaying.static] computes eggs laid. The functions that
#' compute \eqn{N} and \eqn{O} are called *once* after setup.
#' The class of `ML_interface` is set to `static` and the function
#' is never called again. If any parameters are changed that would
#' affect egg laying, then the class of `ML_interface` should 
#' get reset to `setup` to reconfigure \eqn{N} and \eqn{O}. 
#' 
#' @inheritParams EggLaying
#' 
#' @return an **`xds`** model object
#' 
#' @seealso For \eqn{Q}, see [compute_Qall]
#' @seealso For \eqn{N}, see [make_habitat_matrix]
#' @export
#' @keywords internal
EggLaying.setup = function(t, y, xds_obj){
  xds_obj <- egg_laying_dynamics(t, y, xds_obj)
  class(xds_obj$ML_interface) <- 'static'
  return(xds_obj)
} 

#' @title Compute eggs laid
#' @description Computes eggs laid for an autonomous model
#' @inheritParams EggLaying
#' @return an `xds` object
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
#' @return an `xds` object
#' @export
#' @keywords internal
EggLaying.dynamic = function(t, y, xds_obj){
  return(egg_laying_dynamics)
}

#' @title Compute eggs laid
#' @description Computes eggs laid with exogenous forcing
#' on parameter affecting host availability.
#' @inheritParams EggLaying
#' @return an `xds` object
#' @export
#' @keywords internal
egg_laying_dynamics = function(t, y, xds_obj){
  xds_obj = compute_Qall(xds_obj)
  xds_obj = compute_O_matrix(xds_obj)
  xds_obj = compute_eggs_laid(t, y, xds_obj)
  return(xds_obj)
}

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
#' @seealso This function is called by [compute_Qall]
#' @seealso [make_habitat_matrix] discusses \eqn{N}
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_ML_interface]
#' @export
#' @keywords internal
F_Q = function(habitat_matrix, search_weights){
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
#' @seealso This function is called by [compute_Qall]
#' @seealso [make_habitat_matrix] discusses \eqn{N}
#' 
#' @seealso The availability of traps and bad habitats is setup in [setup_ML_interface]
#' @export
#' @keywords internal
F_Qall = function(Q, Q_traps, Q_bad){
  Qall <- Q + Q_traps + Q_bad
  return(as.vector(Qall))
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
#' @seealso Total habitat availability, \eqn{\cal Q}, is computed by [F_Q]
#' @seealso The availability of ovitraps and bad habitats is setup in [setup_ML_interface]
#' @export
#' @keywords internal
make_O_matrix= function(search_weights, habitat_matrix, Q){
  ix = which(Q == 0)
  if(length(ix) > 0) Q[ix]=1
  lay_matrix = diag(search_weights) %*% t(habitat_matrix) %*% diag(1/as.vector(Q))
  return(lay_matrix)
}

#' @title Compute and store availability of egg-laying habitats
#' @description Set the value of a variable, \eqn{Q}, that describes the availability of any habitat
#' or device that would attract mosquitoes and induce them to lay eggs.
#' @param xds_obj an `xds` object
#' @return an `xds` object
#' @seealso [F_Q]
#' @export
#' @keywords internal
compute_Qall = function(xds_obj){with(xds_obj$ML_interface,{
  for(s in 1:xds_obj$nVectorSpecies){
    Q = F_Q(habitat_matrix, search_weights[[s]]) 
    xds_obj$ML_interface$Q[[s]] = Q
    Qall = F_Qall(Q, Qbad[[s]], Qtraps[[s]])
    xds_obj$ML_interface$Qall[[s]] = Qall 
  }
  return(xds_obj)
})}

#' @title Compute the Egg Laying Matrix 
#' 
#' @description Compute the egg laying matrices
#' for all the vector species. The matrix determines
#' how all the eggs laid by adult mosquitoes are apportioned
#' among larval habitats
#' 
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal
compute_O_matrix = function(xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    wts = xds_obj$ML_interface$search_weights[[s]]
    N = xds_obj$ML_interface$habitat_matrix
    Q = xds_obj$ML_interface$Q[[s]]
    xds_obj$ML_interface$laying_matrix[[s]] = make_O_matrix(wts, N, Q)
  }
  return(xds_obj)
}

#' @title Eggs Laying in Habitats 
#' 
#' @description Computes egg distribution for the aquatic habitats
#' 
#' @param eggs_laid the number of eggs laid in each patch, a vector of length `nPatches`
#' @param O_matrix the egg laying matrix
#' @param Q larval habitat availability 
#' @param Qall total availability 
#' 
#' @return a [vector], \eqn{\eta} where \eqn{\left|\eta\right|=}`nHabitats`
#' 
#' @seealso [compute_O_matrix] 
#' @export
#' @keywords internal
F_eta = function(eggs_laid, O_matrix, Q, Qall){
  ix = which(Qall == 0)
  if(length(ix) > 0) Qall[ix]=1
  return(as.vector(O_matrix %*% (eggs_laid*Q/Qall)))
}

#' @title Compute eggs laid
#' @description Computes eggs laid in aquatic habitats
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an `xds` object
#' @return an `xds` object
#' @seealso [compute_eggs_laid]
#' @export
#' @keywords internal
compute_eggs_laid = function(t, y, xds_obj){
  with(xds_obj$ML_interface,{
    for(s in 1:xds_obj$nVectorSpecies){
      G = F_eggs(t, y, xds_obj, s)
      xds_obj$terms$G[[s]] = G 
      eta = F_eta(G, laying_matrix[[s]], Q[[s]], Qall[[s]])
      xds_obj$terms$eta[[s]] = eta 
      return(xds_obj)
    }
  })
}
