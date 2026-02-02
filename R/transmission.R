# Methods to compute mixing and parasite / pathogen transmission during bloood feeding

#' @title Setup the interface for parasite / pathogen transmission
#' @description Sets up the object that defines the transmission interface for an `xds` model object.
#' @details
#' This implements a model for egg laying described by Wu SL, *et al.*, (2023).
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @seealso [make_xds_object_template]
#' @seealso [setup_XY_interface]
#' @keywords internal
#' @export
setup_transmission <- function(xds_obj){

  # Mixing Matrix: beta[[s]][[i]]
  xds_obj$terms$beta = list()
  xds_obj$terms$beta[[1]] = list()
  xds_obj$terms$beta[[1]][[1]] = diag(1)
  class(xds_obj$terms$beta) <- 'setup'

  # Entomological Inoculation Rate: EIR[[i]]
  # eir[[s]][[i]]
  xds_obj$terms$eir = list()
  xds_obj$terms$eir[[1]] = list()
  xds_obj$terms$eir[[1]][[1]] = diag(1)
  xds_obj$terms$EIR = list()
  xds_obj$terms$EIR[[1]] = diag(1)

  # Net Infectiousness: ni[[s]][[i]]
  xds_obj$terms$ni = list()
  xds_obj$terms$ni[[1]] = list()
  xds_obj$terms$ni[[1]][[1]] = diag(1)
  xds_obj$terms$kappa = list()

  # Local Fraction: local_frac[[s]]
  xds_obj$terms$local_frac = list()
  xds_obj$terms$local_frac[[1]] = rep(1, xds_obj$nPatches)

  return(xds_obj)
}



#' @title Compute beta, the biting distribution matrix
#' @param H human / host population density
#' @param W human / host availability in the patches
#' @param wts_f the blood feeding search weights
#' @param TaR (time at risk), a [matrix]  dimensions `nPatches` by `nStrata`
#' @return a [matrix] of dimensions `nStrata` by `nPatches`
#' @keywords internal
#' @export
F_beta = function(H, W, wts_f, TaR){
  ix = which(W==0)
  if(length(ix)>0) W[ix]=1
  beta = diag(wts_f, length(H)) %*% t(TaR) %*% diag(1/W, length(W))
  return(beta)
}

#' @title Compute beta
#' @description This function computes the mixing matrix, beta
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @keywords internal
#' @export
compute_beta <- function(t, y, xds_obj){
  for(i in 1:xds_obj$nHostSpecies){
    H = F_H(t, y, xds_obj, i)
    for(s in 1:xds_obj$nVectorSpecies){
      W = xds_obj$XY_interface$W[[s]]
      wts = xds_obj$XY_interface$search_weights[[s]][[i]]
      TaR = xds_obj$XY_interface$TaR[[s]][[i]]
      xds_obj$terms$beta[[s]][[i]] <- F_beta(H, W, wts, TaR)
    }
  }

  return(xds_obj)
}

#' @title Compute the daily Entomological Inoculation Rate (EIR)
#' @description Compute the daily EIR for a set of human population
#' strata given the biting density of infective mosquitoes in each patch
#' @param fqZ the infective biting density
#' @param beta the mixing matrix
#' @param local_frac is the fraction of bites occurring on residents
#' @return [numeric] vector of length `nStrata`
#' @export
#' @keywords internal
F_eir <- function(fqZ, beta, local_frac){
  eir = beta %*% (fqZ*local_frac)
  return(as.vector(eir))
}


#' @title Compute EIR
#' @description This function computes the EIR for each stratum of each host species
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @export
#' @keywords internal
compute_EIR <- function(t, y, xds_obj){

  for(i in 1:xds_obj$nHostSpecies){
    fqZ <- F_fqZ(t, y, xds_obj, 1)
    beta <- xds_obj$terms$beta[[1]][[i]]
    lf <- xds_obj$terms$local_frac[[1]]

    xds_obj$terms$EIR[[i]] <- F_eir(fqZ, beta, lf)
    
    if(xds_obj$nVectorSpecies > 1)
      for(s in 2:xds_obj$nVectorSpecies){
        fqZ <- F_fqZ(t, y, xds_obj, 1)
        beta <- xds_obj$terms$beta[[s]][[i]]
        lf <- xds_obj$terms$local_frac[[s]]
        xds_obj$terms$EIR[[i]] <- xds_obj$terms$EIR[[i]] + F_eir(fqZ, beta, lf)
      }
  }

  return(xds_obj)
}

#' @title Compute EIR for each vector-host pair
#' @description This function computes the EIR from each vector species for each stratum of each host species
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @export
#' @keywords internal
compute_EIR_full <- function(t, y, xds_obj){

  for(i in 1:xds_obj$nHostSpecies){
    fqZ <- F_fqZ(t, y, xds_obj, 1)
    beta <- xds_obj$terms$beta[[1]][[i]]
    lf <- xds_obj$terms$local_frac[[1]]
    eir <- F_eir(fqZ, beta, lf)
    xds_obj$terms$eir[[1]][[i]] <- eir
    xds_obj$terms$EIR[[i]] <- eir
    s = length(xds_obj$MY_obj)
    if(s>1)
      for(s in 2:xds_obj$nVectorSpecies){
        fqZ <- F_fqZ(t, y, xds_obj, s)
        beta <- xds_obj$terms$beta[[s]][[i]]
        lf <- xds_obj$terms$local_frac[[s]]
        eir <- F_eir(fqZ, beta, lf)
        xds_obj$terms$eir[[s]][[i]] <- eir
        xds_obj$terms$EIR[[i]] <- xds_obj$terms$EIR[[i]] + eir
      }
  }

  return(xds_obj)
}


#' @title Net infectiousness of human population to mosquitoes
#' @description Compute the net infectiousness of humans in each patch at time `t`
#' @param Wi availability of this host
#' @param W availability of all hosts
#' @param beta the mixing matrix
#' @param X the infectious density of the strata
#' @return a [numeric] vector of length `nPatches`
#' @export
#' @keywords internal
F_kappa <- function(Wi, W, beta, X) {
  ix = which(W==0)
  if(length(ix)>0) W[ix]=1
  kappa = Wi/W*(as.vector(t(beta) %*% X)) 
  return(as.vector(kappa))
}

#' @title Compute kappa
#' @description This function computes kappa for each vector species in each patch
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @export
#' @keywords internal
compute_kappa <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    Wi = xds_obj$XY_interface$Wi[[s]][[1]]
    W = xds_obj$XY_interface$W[[s]]
    beta = xds_obj$terms$beta[[s]][[1]]
    X = F_X(t, y, xds_obj, 1)
    

    kappa <- F_kappa(Wi, W, beta, X)

    if(xds_obj$nHostSpecies>1)
      for(i in 2:xds_obj$nHostSpecies){
        beta = xds_obj$terms$beta[[s]][[i]]
        Wi = xds_obj$XY_interface$Wi[[s]][[i]]
        W = xds_obj$XY_interface$W[[s]]
        kappa <- kappa + F_kappa(Wi, W, beta, X)
      }

    lf = xds_obj$terms$local_frac[[s]]
    kappa_visitors = xds_obj$XY_interface$vis_kappa[[s]]
    kappa = lf*kappa + (1-lf)*kappa_visitors
    xds_obj$terms$kappa[[s]] = kappa
  }

  return(xds_obj)
}

#' @title Compute the local fraction
#' @description Compute the availability for the pathogen's hosts for blood feeding
#' @param W availability of blood hosts
#' @param Visitors availability of visitors
#' @return the fraction of bites
#' @export
#' @keywords internal
F_local_frac <- function(W, Visitors){
  local_frac = W/(W+Visitors)
  ix = which(W+Visitors == 0)
  if(length(ix>0)) local_frac[ix] = 1
  return(local_frac)
}

#' @title Compute the local fraction
#' @description Compute the availability for the pathogen's hosts for blood feeding
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @export
#' @keywords internal
compute_local_frac <- function(xds_obj){with(xds_obj$XY_interface,{
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj$vars$local_frac[[s]] = F_local_frac(W[[s]], visitors[[s]])
  }
  return(xds_obj)
})}

#' @title Compute the mixing matrix and transmission terms
#' @description This method dispatches on the type of `xds_obj$beta`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @export
#' @keywords internal
Transmission <- function(t, y, xds_obj){
  UseMethod('Transmission', xds_obj$terms$beta)
}

#' @title Compute transmission terms with a static mixing matrix
#' @description In the static case, \eqn{\beta} is not updated
#' @inheritParams Transmission
#' @return an `xds` object
#' @export
#' @keywords internal
Transmission.static <- function(t, y, xds_obj){
  xds_obj = compute_EIR(t, y, xds_obj)
  xds_obj = compute_kappa(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute transmission, the dynamic case
#' @description Compute transmission terms with a dynamic mixing matrix
#' @inheritParams Transmission
#' @return an `xds` object
#' @export
#' @keywords internal
Transmission.dynamic <- function(t, y, xds_obj){
  return(transmission_dynamics(t, y, xds_obj)) 
}

#' @title Compute transmission, the static case
#' @description Set up and compute transmission terms with a static mixing matrix
#' @details The `setup` case is called whenever any parameter affecting the mixing matrix
#' in a static model is changed
#' @inheritParams Transmission
#' @return an `xds` object
#' @export
#' @keywords internal
Transmission.setup <- function(t, y, xds_obj){
  class(xds_obj$XY_interface) <- 'static'
  xds_obj <- transmission_dynamics(t, y, xds_obj)
  return(xds_obj)
}

#' @title Compute transmission, the dynamic case
#' @description Compute transmission terms with a dynamic mixing matrix
#' @inheritParams Transmission
#' @return an `xds` object
#' @export
#' @keywords internal
transmission_dynamics <- function(t, y, xds_obj){
  xds_obj = compute_local_frac(xds_obj)
  xds_obj = compute_beta(t, y, xds_obj)
  xds_obj = compute_EIR(t, y, xds_obj)
  xds_obj = compute_kappa(t, y, xds_obj)
  return(xds_obj)
}


