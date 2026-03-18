# specialized methods for the adult mosquito GeM model

#' @title The `GeM` Module for the MY Component
#' @description
#' `GeM` is a system of delay differential equation model for mosquito ecology
#' and infection dynamics. The core dynamics trace back to Macdonald's 
#' mosquito model (1952). Macdonald's model was extended by Aron & May (1982) to
#' include a variable for mosquito population density and a term for 
#' emergence (\eqn{\Lambda}). Aron & May's model was extended by Wu *et al.* (2023) to include
#' spatial dynamics. In this version, a new method is introduced to non-autonomous
#' dynamics: an accessory variable computes time-varying survival and dispersal 
#' through a time varying EIP. 
#'
#' @section State Variables:
#' \describe{
#'   \item{`M`}{density of adult mosquitoes}
#'   \item{`P`}{density of parous adult mosquitoes}
#'   \item{`Y`}{density of infected adult mosquitoes}
#'   \item{`Z`}{density of infectious adult mosquitoes}
#' }
#'
#' @section Parameters:
#' \describe{
#'   \item{`f`}{blood feeding rate}
#'   \item{`q`}{human blood fraction}
#'   \item{`nu`}{egg batch laying rate} 
#'   \item{`eip`}{extrinsic incubation period (\eqn{\tau})}
#'   \item{`g`}{mosquito mortality rate}
#'   \item{`sigma`}{emigration rate (\eqn{\sigma})}
#'   \item{`mu`}{emigration loss rate (\eqn{\mu})}
#'   \item{`K`}{mosquito dispersal matrix}
#'   \item{`eggs_per_patch`}{# eggs per batch} 
#' }
#' 
#' @section Mosquito Demography:
#'  
#' The *demographic matrix* \eqn{\Omega} is: 
#' \deqn{
#'  \Omega(t) = \mbox{diag} \left( g(t) + \sigma(t) \mu(t) \right) - K \cdot \mbox{diag} \left( \sigma(t) \left(1-\mu(t)\right) \right)
#' } 
#'  
#' @section EIP: 
#' Let \eqn{\tau(t)} denote the EIP for a mosquito, if infected at time \eqn{t}: 
#' the mosquito would become infectious at time \eqn{t+\tau(t).} 
#' Similarly, let \eqn{\tau'(t)} denote the lag for a mosquito that became *infectious* at time \eqn{t}.
#' it was infected at time \eqn{t-\tau'(t).}  The two are related by the identities 
#' \deqn{\tau(t) = \tau'(t+\tau(t))} and 
#' \deqn{\tau'(t) = \tau(t-\tau'(t)).}
#' In the implementation, a function `F_eip` returns \eqn{\tau'(t)}. Implementation of dynamically
#' changing EIP also requires a function to compute
#' the derivative `dF_eip` (below).
#' 
#' @section The Accessory Variables, \eqn{\Upsilon}: 
#' 
#' To compute time-varying survival and dispersal through a time-varying EIP, 
#' we a set of accessory variables, called \eqn{\Upsilon}. To motivate the 
#' algorithm used to compute it, we introduce a new variable, 
#' \eqn{U} that integrates \eqn{\Omega} over time: 
#' \deqn{dU/dt = \Omega(t).}
#'
#' Cumulative mortality from time \eqn{t} to time \eqn{t+s} is: 
#' \deqn{\gamma(t,s)  = U(t+s) - U(t).} 
#' Survival and dispersal for the mosquitoes that became infectious at time \eqn{t} is: 
#' \deqn{\Upsilon(t) = e^{-\gamma\left(t_\tau, t\right)} = e^{-\left(U\left(t\right) - U\left(t_\tau \right)\right)}.}
#' where \eqn{t_\tau = t-\tau'(t)}. 
#' 
#' In `GeM`, \eqn{\Upsilon} is computed as an accessory variable, with derivatives: 
#' \deqn{d\Upsilon/dt = \left(\Omega\left(t_\tau\right)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right) \cdot \Upsilon(t)}
#' Initial conditions are set to: 
#' \deqn{\Upsilon(t_0) = e^{-\Omega(t_0)}}
#' 
#' @section Inputs:
#' 
#' **Emergence** -- `Lambda` or \eqn{\Lambda}, is computed 
#' by the **ML**-Interface using outputs of the **L** Component
#' 
#' 
#' **Net Infectiousness** -- `kappa` or \eqn{\kappa}, is computed 
#' by the **XY**-Interface using outputs of the **XH** Component   
#' @section Dynamics:
#' In the following we use the subscript \eqn{\tau} to mean the value of 
#' a parameter, term, or variable at time \eqn{t-\tau'(t)}: *e.g.*, \eqn{M_\tau = M(t-\tau'(t)}.
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda - \Omega \cdot M \\
#' dP/dt &=  f(M-P) - \Omega \cdot P \\
#' dY/dt &= fq\kappa(M-Y) - \Omega \cdot Y \\
#' dZ/dt &= \Upsilon \cdot (fq\kappa)_\tau(M_\tau-Y_\tau) - \Omega \cdot Z \\
#' \end{array}}
#' 
#' The function [dMYdt.GeM] also computes the variables: \eqn{\Upsilon}; and an accessory variable that internalizes
#' computation of \eqn{(fq\kappa)_\tau}.
#' 
#' @section Egg Laying:
#' 
#' The number of egg batches laid, per patch, per day is \eqn{\nu M}. The total
#' of eggs laid is \eqn{\nu M \times} `eggs_per_batch`
#' 
#' @section Infectious Biting:
#' 
#' The number of bites on humans, per patch, per day is \eqn{fqM}, and the number of 
#' infectious bites, per patch, per day is \eqn{fqZ}.  
#' 
#' @references 
#' + Macdonald G (1952) The analysis of the sporozoite rate. Tropical Diseases Bulletin 49:569-586. 
#' + Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5 in *The Population Dynamics of Infectious Diseases: Theory and Applications,* Springer, Boston, MA.  
#' + Wu SL, *et al.* (2023) Spatial dynamics of malaria transmission. PLoS Computational Biology 19(6): e1010684. [https://doi.org/10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
#' @name GeM
#' @rdname GeM
NULL


#' @title The **GeM** Module Skill Set
#'
#' @description The **MY** skill set is a list of
#' an module's capabilities:
#'
#' + `demography` is
#'
#' @inheritParams skill_set_MY
#'
#' @return *MY* module skill set, as a list
#'
#' @keywords internal
#' @export
skill_set_MY.GeM = function(MYname){
  return(list())
}

#' Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_MY.GeM = function(xds_obj, s){

  return(xds_obj)
}


#' @title Compute Derivatives for **MY** module `GeM`
#'
#' @description Compute the derivatives for the generalized, non-autonomous Macdonald model
#' for mosquito ecology and infection dynamics. See [GeM].
#' @inheritParams dMYdt
#' @return a [numeric] vector
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
#' @keywords internal
#' @export
dMYdt.GeM <- function(t, y, xds_obj, s){

  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      deip_dt = dF_eip(t, xds_obj, s)

      if (t <= eip) {
        M_eip <- xds_obj$MYinits[[s]]$M
        Y_eip <- xds_obj$MYinits[[s]]$Y
        fqkappa_eip <- kappa*f*q
        g_eip <- g
        sigma_eip <- sigma
      } else {
        M_eip <- lagvalue(t=t-eip, nr = xds_obj$ix$MY[[s]]$M_ix)
        Y_eip <- lagvalue(t=t-eip, nr = xds_obj$ix$MY[[s]]$Y_ix)
        fqkappa_eip <- lagderiv(t=t-eip, nr = xds_obj$ix$MY[[s]]$fqkappa_ix)
        g_eip <- lagderiv(t=t-eip, nr = xds_obj$ix$MY[[s]]$g_ix)
        sigma_eip <- lagderiv(t=t-eip, nr = xds_obj$ix$MY[[s]]$sigma_ix)
      }

      Omega_eip <- compute_Omega_xde(g_eip, sigma_eip, mu, calK)

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      dYdt <- f*q*kappa*(M - Y) - (Omega %*% Y)
      dZdt <- U %*% (fqkappa_eip * (M_eip - Y_eip)) - (Omega %*% Z)
      dUdt <- ((1-deip_dt)*Omega_eip - Omega) %*% U

      return(c(dMdt, dPdt, dYdt, dZdt, as.vector(dUdt), f*q*kappa, g, sigma))
    })
  })
}


#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the GeM model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.GeM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$ix$MY[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the GeM model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.GeM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the GeM model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.GeM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggs_per_batch)
  })
}



#' @title Setup MY_obj for the GeM model
#' @description Implements [setup_MY_obj] for the GeM model
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @keywords internal
#' @export
setup_MY_obj.GeM = function(MYname, xds_obj, s, options=list()){
  xds_obj = ode_to_dde(xds_obj)
  MY_obj <- make_MY_obj_GeM(xds_obj$nPatches, options)
  class(MY_obj) <- 'GeM'
  xds_obj$MY_obj[[s]] = MY_obj
  return(xds_obj)
}

#' @title Make parameters for GeM ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param options a [list] of values that overwrites the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggs_per_batch eggs laid per oviposition
#' @return a [list]
#' @keywords internal
#' @export
make_MY_obj_GeM = function(nPatches, options=list(), eip =12,
                              g=1/12,  sigma=1/8,  mu=0,
                              f=0.3,  q=0.95,
                              nu=1,  eggs_per_batch=60){

  with(options,{

    MY_obj <- list()

    MY_obj$nPatches <- nPatches

    MY_obj <- setup_eip_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_feeding_rate(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_human_frac(checkIt(q, nPatches), MY_obj)
    MY_obj <- setup_mozy_mort(checkIt(g, nPatches), MY_obj)
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj)
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj)
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj)

    MY_obj <- setup_K_obj(nPatches, MY_obj)

    Omega <- diag(g, nPatches)
    MY_obj$Omega <- Omega
    MY_obj$Upsilon <- expm::expm(-Omega*eip)
    MY_obj$nPatches <- nPatches

    Omega <- diag(g, nPatches)
    MY_obj$Omega <- Omega
    MY_obj$Upsilon <- expm::expm(-Omega*eip)
    MY_obj$nPatches <- nPatches

    MY_obj$eggs_per_batch <- eggs_per_batch

    MY_obj$baseline <- MY_obj
    class(MY_obj$baseline) <- 'GeM'

    return(MY_obj)
  })}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the GeM model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @importFrom utils tail
#' @keywords internal
#' @export
setup_MY_ix.GeM <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  U_ix <- seq(from = max_ix+1, length.out = nPatches^2)
  max_ix <- tail(U_ix, 1)

  fqkappa_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(fqkappa_ix, 1)

  g_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(g_ix, 1)

  sigma_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(sigma_ix, 1)

  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix, Y_ix=Y_ix, Z_ix=Z_ix,
                         U_ix = U_ix, fqkappa_ix=fqkappa_ix,
                         g_ix=g_ix, sigma_ix=sigma_ix)
  return(xds_obj)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.GeM <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix, return(list(
    M = y[M_ix],
    P = y[P_ix],
    Y = y[Y_ix],
    Z = y[Z_ix],
    U = matrix(y[U_ix], xds_obj$nPatches, xds_obj$nPatches)
)))}

#' @title parse the output of deSolve and return variables for the GeM model
#' @description Implements [parse_MY_orbits] for the GeM model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits.GeM <- function(outputs, xds_obj, s) {with(xds_obj$MY_obj[[s]]$ix,{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  Y = outputs[,Y_ix]
  Z = outputs[,Z_ix]
  f = get_ft(xds_obj, s)
  q = get_qt(xds_obj, s)
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parous=parous, fqZ=f*q*Z, fqM=f*q*M))
})}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
get_MY_pars.GeM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggs_per_batch=eggs_per_batch, calK=calK
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_pars.GeM <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_par <- f_par
    xds_obj$MY_obj[[s]]$q_par <- q_par
    xds_obj$MY_obj[[s]]$g_par <- g_par
    xds_obj$MY_obj[[s]]$sigma_par <- sigma_par
    xds_obj$MY_obj[[s]]$mu_par <- mu_par
    xds_obj$MY_obj[[s]]$nu_par <- nu_par
    xds_obj$MY_obj[[s]]$eip_par <- eip_par
    xds_obj$MY_obj[[s]]$calK_par <- calK_par
    xds_obj$MY_obj[[s]]$eggs_per_batch <- eggs_per_batch
    return(xds_obj)
  }))}


#' @title Set mosquito bionomics to baseline
#' @description Implements [MBaseline] for models with no forcing on the baseline
#' @inheritParams MBaseline
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBaseline.GeM <- function(t, y, xds_obj, s){with(xds_obj$MY_obj[[s]],{
  vars = xds_obj$vars
  # Baseline parameters
  xds_obj$MY_obj[[s]]$f_t      <- F_feeding_rate(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t      <- F_human_frac(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t      <- F_mozy_mort(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t  <- F_emigrate(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu       <- F_dispersal_loss(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu       <- F_batch_rate(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$eip      <- F_eip(t, xds_obj, s)
  xds_obj                      <- F_K_matrix(t, xds_obj, s)

  # Reset Effect Sizes
  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  return(xds_obj)
})}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBionomics.GeM <- function(t, y, xds_obj, s) {with(xds_obj$MY_obj[[s]],{
  xds_obj$MY_obj[[s]]$f <- es_f*f_t
  xds_obj$MY_obj[[s]]$q <- es_q*q_t
  xds_obj$MY_obj[[s]]$g <- es_g*g_t
  xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
  xds_obj <- make_Omega(xds_obj, s)
  return(xds_obj)
})}

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the GeM_dts model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_MYt.GeM <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$Lambda[[s]]*xds_obj$MYday
  kappa = xds_obj$kappa[[s]]
  xds_obj <- update_Omega(xds_obj, s)

  with(get_MY_vars(y, xds_obj, s),{
    with(MY_obj,{

      eip_day_ix = (t %% max_eip) + 1
      eip_yday_ix = ((t-1) %% max_eip) + 1
      Gix = c(t-1:max_eip) %% max_eip + 1
      Gt <- G[Gix]

      Mt <- Lambda + Omega %*% M
      Pt <- f*(M-P) + Omega %*% P
      Ut <- Lambda + Omega %*% (exp(-f*q*kappa)*U)
      Yt <- Omega %*% (Y %*% diag(1-Gt))
      Zt <- Omega %*% (Y%*%Gt)  + (Omega %*% Z)

      Yt[,eip_yday_ix]  <- Yt[,eip_yday_ix] + Yt[,eip_day_ix]
      Yt[,eip_day_ix] <- Omega %*% ((1-exp(-f*q*kappa))*U)

      return(list(M=unname(Mt), P=unname(Pt), U=unname(Ut), Y=unname(as.vector(Yt)), Z=unname(Zt)))
    })
  })
}


#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the GeM_dts model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_MYt.GeM <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$Lambda[[s]]*xds_obj$MYday
  kappa = xds_obj$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      eip_day_ix = (t %% max_eip) + 1
      eip_yday_ix = ((t-1) %% max_eip) + 1
      Gix = c(t-1:max_eip) %% max_eip + 1
      Gt <- G[Gix]

      Mt <- Lambda + Omega %*% M
      Pt <- ff*(M-P) + Omega %*% P
      Ut <- Lambda + Omega %*% (exp(-ff*q*kappa)*U)
      Yt <- Omega %*% (Y %*% diag(1-Gt))
      Zt <- Omega %*% (Y%*%Gt)  + (Omega %*% Z)

      Yt[,eip_yday_ix]  <- Yt[,eip_yday_ix] + Yt[,eip_day_ix]
      Yt[,eip_day_ix] <- Omega %*% ((1-exp(-f*q*kappa))*U)

      return(list(M=unname(Mt), P=unname(Pt), U=unname(Ut), Y=unname(as.vector(Yt)), Z=unname(Zt)))
    })
  })
}



#' @title Setup initial values for the GeM model
#' @description Implements [setup_MY_inits] for the GeM model
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @keywords internal
#' @export
setup_MY_inits.GeM = function(xds_obj, s, options=list()){with(xds_obj$MY_obj[[s]], {
  Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)
  Upsilon = expm(-Omega*eip)
  xds_obj$MYinits[[s]] = make_MY_inits_GeM(nPatches, Upsilon, options)
  return(xds_obj)
})}


#' @title Make inits for GeM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param Upsilon a matrix describing survival and dispersal through the EIP
#' @param options a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @keywords internal
#' @export
make_MY_inits_GeM = function(nPatches, Upsilon, options = list(),
                             M=5, P=1, Y=1, Z=1){
  stopifnot(dim(Upsilon) == c(nPatches,nPatches))
  with(options,{
    M = unname(checkIt(M, nPatches))
    P = unname(checkIt(P, nPatches))
    Y = unname(checkIt(Y, nPatches))
    Z = unname(checkIt(Z, nPatches))
    U = unname(Upsilon)
    dd = rep(0, nPatches)
    return(list(M=M, P=P, Y=Y, Z=Z, U=U, d1=dd, d2=dd, d3=dd))
  })
}

#' @title Set new MY parameter values
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_inits.GeM <- function(xds_obj, s=1, options=list()) {
  with(xds_obj$MY_obj[[s]]$inits, with(options,{
    xds_obj$MYinits[[s]]$M = M
    xds_obj$MYinits[[s]]$Y = Y
    xds_obj$MYinits[[s]]$P = P
    xds_obj$MYinits[[s]]$Z = Z
    return(xds_obj)
  }))}




#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritParams steady_state_MY
#' @return none
#' @importFrom MASS ginv
#' @keywords internal
#' @export
steady_state_MY.GeM = function(Lambda, kappa, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}



#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_f.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_q.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_g.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_sigma.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}
