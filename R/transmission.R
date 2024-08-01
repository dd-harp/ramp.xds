# Methods to compute mixing and parasite / pathogen transmission during bloood feeding

#' @title Setup the interface for parasite / pathogen transmission
#' @description Sets up the object that defines the transmission interface for an `xds` model object.
#' @details Transmission
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @seealso [make_xds_object]
#' @seealso [setup_BLOOD_FEEDING]
#' @export
setup_TRANSMISSION <- function(pars){

  # Mixing Matrix: beta[[s]][[i]]
  pars$beta = list()
  pars$beta[[1]] = list()
  pars$beta[[1]][[1]] = diag(1)
  class(pars$beta) <- 'setup'

  # Entomological Inoculation Rate: EIR[[i]]
  # eir[[s]][[i]]
  pars$eir = list()
  pars$eir[[1]] = list()
  pars$eir[[1]][[1]] = diag(1)
  pars$EIR = list()
  pars$EIR[[1]] = diag(1)

  # Net Infectiousness: ni[[s]][[i]]
  pars$ni = list()
  pars$ni[[1]] = list()
  pars$ni[[1]][[1]] = diag(1)
  pars$kappa = list()

  # Local Fraction: local_frac[[s]]
  pars$local_frac = list()
  pars$local_frac[[1]] = rep(1, pars$nPatches)

  # x_visitors: x_visitors[[s]]
  pars$vars$x_visitors = list()
  pars$vars$x_visitors[[1]] = rep(0, pars$nPatches)

  return(pars)
}



#' @title Compute beta, the biting distribution matrix
#' @param H human / host population density
#' @param W human / host availability in the patches
#' @param wts_f the blood feeding search weights
#' @param TaR (time at risk), a [matrix]  dimensions `nPatches` by `nStrata`
#' @return a [matrix] of dimensions `nStrata` by `nPatches`
#' @export
compute_beta = function(H, W, wts_f, TaR){
  ix = which(W==0)
  if(length(ix)>0) W[ix]=1
  beta = diag(wts_f, length(H)) %*% t(TaR) %*% diag(1/W, length(W))
  return(beta)
}

#' @title Compute beta
#' @description This function computes the mixing matrix, beta
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
make_beta <- function(t, y, pars){
  for(i in 1:pars$nHosts){
    H = F_H(y, pars, i)
    for(s in 1:pars$nVectors){
      W = pars$vars$W[[s]]
      wts = pars$BFpar$search_weights[[s]][[i]]
      TaR = pars$TaR[[s]][[i]]
      pars$beta[[s]][[i]] <- compute_beta(H, W, wts, TaR)
    }
  }

  return(pars)
}

#' @title Entomological inoculation rate on human strata
#' @description Compute the daily EIR for a set of human population
#' strata given the biting density of infective mosquitoes in each patch
#' @param fqZ the infective biting density
#' @param beta the mixing matrix
#' @param local_frac is the fraction of bites occurring on residents
#' @return [numeric] vector of length `nStrata`
#' @export
compute_EIR <- function(fqZ, beta, local_frac) {
  eir = beta %*% (fqZ*local_frac)
  return(as.vector(eir))
}


#' @title Compute EIR
#' @description This function computes the EIR for each stratum of each host species
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
make_EIR <- function(t, y, pars){

  for(i in 1:pars$nHosts){
    fqZ <- F_fqZ(t, y, pars, 1)
    beta <- pars$beta[[1]][[i]]
    lf <- pars$vars$local_frac[[1]]

    pars$EIR[[i]] <- compute_EIR(fqZ, beta, lf)

    if(pars$nVectors > 1)
      for(s in 2:pars$nVectors){
        fqZ <- F_fqZ(t, y, pars, 1)
        beta <- pars$beta[[s]][[i]]
        lf <- pars$vars$local_frac[[s]]
        pars$EIR[[i]] <- pars$EIR[[i]] + compute_EIR(fqZ, beta, lf)
      }
  }

  return(pars)
}

#' @title Compute EIR for each vector-host pair
#' @description This function computes the EIR from each vector species for each stratum of each host species
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
make_EIR_full <- function(t, y, pars){

  for(i in 1:pars$nHosts){
    fqZ <- F_fqZ(t, y, pars, 1)
    beta <- pars$beta[[1]][[i]]
    lf <- pars$vars$local_frac[[1]]
    eir <- compute_EIR(fqZ, beta, lf)
    pars$eir[[1]][[i]] <- eir
    pars$EIR[[i]] <- eir
    if(s>1)
      for(s in 2:pars$nVectors){
        fqZ <- F_fqZ(t, y, pars, s)
        beta <- pars$beta[[s]][[i]]
        lf <- pars$vars$local_frac[[s]]
        eir <- compute_EIR(fqZ, beta, lf)
        pars$eir[[s]][[i]] <- eir
        pars$EIR[[i]] <- pars$EIR[[i]] + eir
      }
  }

  return(pars)
}


#' @title Net infectiousness of human population to mosquitoes
#' @description Compute the net infectiousness of humans in each patch at time `t`
#' @param Wi availability of this host
#' @param W availability of all hosts
#' @param beta the mixing matrix
#' @param X the infectious density of the strata
#' @return a [numeric] vector of length `nPatches`
#' @export
compute_kappa <- function(Wi, W, beta, X) {
  ix = which(W==0)
  if(length(ix)>0) W[ix]=1
  kappa = Wi/W*(as.vector(t(beta) %*% X))
  return(as.vector(kappa))
}

#' @title Compute kappa
#' @description This function computes kappa for each vector species in each patch
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
make_kappa <- function(t, y, pars){
  for(s in 1:pars$nVectors){
    Wi = pars$vars$Wi[[s]][[1]]
    W = pars$vars$W[[s]]
    beta = pars$beta[[s]][[1]]
    X = F_X(y, pars, 1)

    kappa <- compute_kappa(Wi, W, beta, X)

    if(pars$nHosts>1)
      for(i in 2:pars$nHosts){
        beta = pars$beta[[s]][[i]]
        Wi = pars$vars$Wi[[s]][[i]]
        W = pars$vars$W[[s]]
        kappa <- kappa + compute_kappa(Wi, W, beta, X)
      }

    lf = pars$local_frac[[s]]
    kappa = lf*kappa + (1-lf)*pars$vars$x_visitors[[s]]
    pars$kappa[[s]] = kappa
  }

  return(pars)
}

#' @title Compute the local fraction
#' @description Compute the availability for the pathogen's hosts for blood feeding
#' @param W availability of blood hosts
#' @param Visitors availability of visitors
#' @return the fraction of bites
#' @export
compute_local_frac <- function(W, Visitors){
  local_frac = W/(W+Visitors)
  ix = which(W+Visitors == 0)
  if(length(ix>0)) local_frac[ix] = 1
  return(local_frac)
}

#' @title Compute the local fraction
#' @description Compute the availability for the pathogen's hosts for blood feeding
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
make_local_frac <- function(pars){with(pars$vars,{
  for(s in 1:pars$nVectors){
    pars$vars$local_frac[[s]] = compute_local_frac(W[[s]], visitors[[s]])
  }
  return(pars)
})}

#' @title Compute the mixing matrix and transmission terms
#' @description This method dispatches on the type of `pars$beta`
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
Transmission <- function(t, y, pars){
  UseMethod('Transmission', pars$beta)
}

#' @title Compute transmission terms with a static mixing matrix
#' @description In the static case, \eqn{\beta} is not updated
#' @inheritParams Transmission
#' @return the modified `xds` object
#' @export
Transmission.static <- function(t, y, pars){
  pars = make_EIR(t, y, pars)
  pars = make_kappa(t, y, pars)
  return(pars)
}

#' @title Compute transmission, the dynamic case
#' @description Compute transmission terms with a dynamic mixing matrix
#' @inheritParams Transmission
#' @return the modified `xds` object
#' @export
Transmission.dynamic <- function(t, y, pars){
  pars = make_local_frac(pars)
  pars = make_beta(t, y, pars)
  pars = make_EIR(t, y, pars)
  pars = make_kappa(t, y, pars)
  return(pars)
}

#' @title Compute transmission, the static case
#' @description Set up and compute transmission terms with a static mixing matrix
#' @details The `setup` case is called whenever any parameter affecting the mixing matrix
#' in a static model is changed
#' @inheritParams Transmission
#' @return the modified `xds` object
#' @export
Transmission.setup <- function(t, y, pars){
  class(pars$beta) <- 'dynamic'
  pars <- Transmission(t, y, pars)
  class(pars$BFpar) <- 'static'
  return(pars)
}




