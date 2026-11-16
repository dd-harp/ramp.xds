# specialized methods for the adult mosquito GeM model

#' @title The `GeM` module for the **MY** component
#' @description
#' `GeM` is a generalized, non-autonomous Macdonald model for mosquito
#' ecology and infection dynamics. It includes adult mosquito density and
#' emergence, spatial dynamics, and time-varying survival and dispersal through
#' a potentially time-varying extrinsic incubation period (EIP).
#'
#' @section State Variables:
#' \describe{
#'   \item{`M`}{density of adult mosquitoes}
#'   \item{`P`}{density of parous adult mosquitoes}
#'   \item{`Y`}{density of infected adult mosquitoes}
#'   \item{`Z`}{density of infectious adult mosquitoes}
#' }
#'
#' @section Dynamical Terms:
#' \describe{
#'   \item{`Lambda` or \eqn{\Lambda}}{adult mosquito emergence rate, computed by [Emergence]}
#'   \item{`kappa` or \eqn{\kappa}}{net infectiousness, computed by [Transmission]}
#' }
#'
#' @section Parameters:
#' \describe{
#'   \item{`f`}{blood feeding rate --- use [setup_f] or [setup_blood_feeding_rates]}
#'   \item{`q`}{human fraction --- use [setup_q] or [setup_human_fractions]}
#'   \item{`g`}{mosquito mortality rate --- use [setup_g] or [setup_mosquito_mortality]}
#'   \item{`eip` or \eqn{\tau}}{extrinsic incubation period --- use [setup_eip]}
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_emigration_rates]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss fraction --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K` or `K_matrix`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{egg batch rate in batches per mosquito per day --- use [setup_nu] or [setup_egg_batch_rates]}
#'   \item{`eggs_per_batch`}{number of eggs per batch}
#' }
#'
#' After basic setup, [change_MY_pars.GeM] can be used to change static
#' parameter values. By default, bionomic parameters are configured as static
#' baselines; advanced setup can be used to define dynamic ports.
#'
#' @section Survival and Dispersal:
#' Mosquito survival and dispersal are described by the demographic matrix
#' \eqn{\Omega(t)}:
#' \deqn{\Omega(t)=\mbox{diag}(g(t)+\sigma(t)\mu(t))-K\cdot\mbox{diag}(\sigma(t)(1-\mu(t))).}
#' The parameter \eqn{g} is the per-capita mortality rate while mosquitoes
#' are in a patch. Mosquitoes emigrate from patches at per-capita rates
#' \eqn{\sigma}, and a fraction \eqn{\mu} of emigrating mosquitoes is lost.
#' The columns of the dispersal matrix \eqn{K} sum to zero. For non-trivial
#' dispersal matrices, \eqn{\mbox{diag}(K)=-1}; the off-diagonal elements
#' describe the fractions of surviving emigrants that arrive in other patches.
#'
#' @section EIP:
#' Let \eqn{\tau(t)} be the EIP for a mosquito infected at time \eqn{t}; it
#' becomes infectious at time \eqn{t+\tau(t)}. Let \eqn{\tau'(t)} be the
#' elapsed time since infection for a mosquito that becomes infectious at
#' time \eqn{t}. These quantities satisfy
#' \deqn{\tau(t)=\tau'(t+\tau(t)) \qquad \tau'(t)=\tau(t-\tau'(t)).}
#' The `F_eip` function returns the lag \eqn{\tau'(t)} used by the delay
#' equations. A time-varying EIP also requires its derivative, computed by
#' `dF_eip`.
#'
#' @section Accessory Variable:
#' The matrix \eqn{\Upsilon(t)} represents survival and dispersal through the
#' time-varying EIP, from \eqn{t_\tau=t-\tau'(t)} to \eqn{t}. In the
#' autonomous case it is \eqn{e^{-\Omega\tau}}. For time-varying demography
#' and EIP, it is computed as an accessory variable:
#' \deqn{\frac{d\Upsilon}{dt}=\left(\Omega(t_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).}
#' The initial matrix is set to \eqn{\exp(-\Omega eip)}. This variable is
#' internal to the model and is not an output orbit.
#'
#' @section Inputs:
#' The **ML** interface supplies `Lambda` or \eqn{\Lambda}, the adult
#' mosquito emergence rate, using outputs of the **L** component. The **XY**
#' interface supplies `kappa` or \eqn{\kappa}, the net infectiousness, using
#' outputs of the **XH** component.
#' @section Dynamics:
#' A subscript \eqn{\tau} denotes the value of a parameter, term, or variable
#' at time \eqn{t-\tau'(t)}: \deqn{X_\tau=X(t-\tau'(t)).}
#' The derivatives of the state variables are:
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda-\Omega\cdot M \\\\
#' dP/dt &= f(M-P)-\Omega\cdot P \\\\
#' dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y \\\\
#' dZ/dt &= \Upsilon\cdot(fq\kappa)_\tau(M_\tau-Y_\tau)-\Omega\cdot Z
#' \end{array}}
#' The implementation also evolves \eqn{\Upsilon} and accessory variables
#' used to evaluate the delayed values of \eqn{fq\kappa}, \eqn{g}, and
#' \eqn{\sigma}.
#'
#' @section Egg Laying:
#' The egg batch rate \eqn{\nu} and the number of eggs per batch,
#' `eggs_per_batch`, determine the egg production rate per patch:
#' \deqn{G=\nu M\cdot\texttt{eggs\_per\_batch}.}
#' [F_eggs.GeM] computes \eqn{G}, which is passed to [EggLaying] for
#' distribution among aquatic habitats.
#'
#' @section Infective Biting:
#' The human biting density, `fqM`, is the number of human bites per patch per
#' day; the infective biting density, `fqZ`, is the number of infectious human
#' bites per patch per day. [F_fqM.GeM] and [F_fqZ.GeM] compute these
#' quantities as \eqn{f q M} and \eqn{f q Z}, respectively. [compute_EIR]
#' combines infective biting density with the biting distribution matrix and
#' resident-bite fraction to compute stratum-level `EIR`. [F_hbr] uses `fqM`,
#' the same matrix, and resident-bite fraction to compute stratum-level `HBR`.
#'
#' @seealso
#' + Basic setup: [make_MY_obj_GeM], [setup_MY_obj.GeM]
#' + Dynamics: [dMYdt.GeM]
#' + Biting and eggs: [F_fqZ.GeM], [F_fqM.GeM], [F_eggs.GeM]
#' + Parameters: [get_MY_pars.GeM], [change_MY_pars.GeM]
#' + Variables: [make_MY_inits_GeM], [setup_MY_inits.GeM], [parse_MY_orbits.GeM]
#'
#' @references 
#' + Macdonald G (1952) The analysis of the sporozoite rate. Tropical Diseases Bulletin 49:569-586. 
#' + Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5 in *The Population Dynamics of Infectious Diseases: Theory and Applications,* Springer, Boston, MA.  
#' + Wu SL, *et al.* (2023) Spatial dynamics of malaria transmission. PLoS Computational Biology 19(6): e1010684. [https://doi.org/10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
#' @name GeM
#' @rdname GeM
NULL


#' @title The skill set
#'
#' @inheritParams setup_skillset_MY
#'
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
setup_skillset_MY.GeM = function(xds_obj, s){
  skills = list(
    not_implemented = TRUE
  )
  xds_obj$MY_obj[[s]]$skill_set = skills
  return(xds_obj) 
}

#' Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.GeM = function(xds_obj, s){

  return(xds_obj)
}


#' @title Compute derivatives for `GeM` (**MY**)
#'
#' @description Compute the derivatives for the generalized, non-autonomous Macdonald model
#' for mosquito ecology and infection dynamics. See [GeM].
#' @inheritSection GeM State Variables
#' @inheritSection GeM Dynamical Terms
#' @inheritSection GeM Parameters
#' @inheritSection GeM EIP
#' @inheritSection GeM Accessory Variable
#' @inheritSection GeM Dynamics
#' @inheritParams dMYdt
#' @seealso [GeM], [F_fqZ.GeM], [F_eggs.GeM]
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

      deip_dt = eip_obj$dF_eip(t) 

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

      Omega_eip <- F_Omega_xde(g_eip, sigma_eip, mu, calK)

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      dYdt <- f*q*kappa*(M - Y) - (Omega %*% Y)
      dZdt <- U %*% (fqkappa_eip * (M_eip - Y_eip)) - (Omega %*% Z)
      dUdt <- ((1-deip_dt)*Omega_eip - Omega) %*% U

      return(c(dMdt, dPdt, dYdt, dZdt, as.vector(dUdt), f*q*kappa, g, sigma))
    })
  })
}


#' @title Infective mosquito biting rate for `GeM`
#' @description Implements [F_fqZ] for the GeM model.
#' @inheritSection GeM Infective Biting
#' @inheritParams F_fqZ
#' @seealso [GeM], [dMYdt.GeM]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.GeM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$ix$MY[[s]]$Z_ix]
}

#' @title Mosquito biting rate for `GeM`
#' @description Returns `fqM`, the daily mosquito biting density on humans
#' for each patch in the GeM model.
#' @inheritSection GeM Infective Biting
#' @inheritParams F_fqM
#' @seealso [GeM], [F_hbr]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.GeM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title Egg production rate for `GeM`
#' @description Implements [F_eggs] for the GeM model.
#' @inheritSection GeM Egg Laying
#' @inheritParams F_eggs
#' @seealso [GeM], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.GeM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggs_per_batch)
  })
}



#' @title Set up `GeM` (**MY**)
#' @description
#'
#' This sets up a `GeM` model object for the **MY** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_MY_obj_GeM]
#' + the initial conditions are set by [setup_MY_inits]
#' + `options` is passed to both to override the defaults
#'
#' @inheritSection GeM State Variables
#' @inheritSection GeM Parameters
#' @inheritSection GeM EIP
#' @inheritSection GeM Accessory Variable
#' @seealso [GeM], [make_MY_obj_GeM], [setup_MY_inits.GeM]
#'
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#'
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @keywords internal
#' @export
setup_MY_obj.GeM = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MYname = "GeM"
  xds_obj = ode_to_dde(xds_obj)
  MY_obj <- make_MY_obj_GeM(xds_obj$nPatches, options)
  class(MY_obj) <- 'GeM'
  xds_obj$MY_obj[[s]] = MY_obj
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, TRUE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  return(xds_obj)
}

#' @title Make parameters for `GeM` (**MY**)
#' @param nPatches is the number of patches, an integer
#' @param options a [list] of values that overrides the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggs_per_batch eggs laid per oviposition
#' @seealso [GeM], [setup_MY_obj.GeM]
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
    MY_obj <- setup_f_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_q_obj(checkIt(q, nPatches), MY_obj)
    MY_obj <- setup_g_obj(checkIt(g, nPatches), MY_obj)
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj)
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj)
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj)

    MY_obj$eggs_per_batch <- eggs_per_batch

    MY_obj$baseline <- MY_obj
    class(MY_obj$baseline) <- 'GeM'

    return(MY_obj)
  })}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the GeM model.
#' @inheritSection GeM State Variables
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

#' @title List variables for `GeM` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritSection GeM State Variables
#' @inheritSection GeM Accessory Variable
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

#' @title Parse outputs for `GeM` (**MY**)
#' @description Implements [parse_MY_orbits] for the GeM model
#' @inheritSection GeM State Variables
#' @inheritSection GeM Infective Biting
#' @inheritParams parse_MY_orbits
#' @seealso [GeM], [get_MY_vars.GeM]
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

#' @title Get parameters for `GeM` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritSection GeM Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @seealso [GeM], [change_MY_pars.GeM]
#' @keywords internal
#' @export
get_MY_pars.GeM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggs_per_batch=eggs_per_batch, calK=calK
  ))
}

#' @title Change parameters for `GeM` (**MY**)
#' @description For the `GeM` module (**MY**) for the \eqn{s^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#' @inheritSection GeM Parameters
#' @inheritParams change_MY_pars
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_pars.GeM <- function(xds_obj, options=list(), s=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_obj$f = f
    xds_obj$MY_obj[[s]]$q_obj$q = q
    xds_obj$MY_obj[[s]]$g_obj$g = g
    xds_obj$MY_obj[[s]]$sigma_obj$sigma = sigma
    xds_obj$MY_obj[[s]]$eip_obj$eip = eip
    xds_obj$MY_obj[[s]]$mu_obj$mu = mu
    xds_obj$MY_obj[[s]]$nu_obj$nu = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- MBionomics(0, y, xds_obj, s)
    xds_obj <- MEffectSizes(0, y, xds_obj, s)
    return(xds_obj)
  }))}


#' @title Mosquito bionomics for `GeM` (**MY**)
#' @description Implements [MBionomics] for models with no exogenous forcing
#' @inheritSection GeM Parameters
#' @inheritSection GeM Survival and Dispersal
#' @inheritSection GeM EIP
#' @inheritParams MBionomics
#' @seealso [GeM], [MEffectSizes.GeM]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.GeM <- function(t, y, xds_obj, s){with(xds_obj$MY_obj[[s]],{
  vars = xds_obj$vars
  # Baseline parameters
  xds_obj$MY_obj[[s]]$f_t      <- F_f(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t      <- F_q(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t      <- F_g(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t  <- F_sigma(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu       <- F_mu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu       <- F_nu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$eip      <- F_eip(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$K_matrix <- F_K_matrix(t, xds_obj, s)

  # Reset Effect Sizes
  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  return(xds_obj)
})}

#' @title Apply effect sizes for `GeM` (**MY**)
#' @description Implements [MEffectSizes] for models with no exogenous forcing
#' @inheritSection GeM Parameters
#' @inheritSection GeM Survival and Dispersal
#' @inheritParams MEffectSizes
#' @seealso [GeM], [MBionomics.GeM]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.GeM <- function(t, y, xds_obj, s) {with(xds_obj$MY_obj[[s]],{
  xds_obj$MY_obj[[s]]$f <- es_f*f_t
  xds_obj$MY_obj[[s]]$q <- es_q*q_t
  xds_obj$MY_obj[[s]]$g <- es_g*g_t
  xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
  xds_obj <- update_Omega(xds_obj, s)
  return(xds_obj)
})}

#' @title Setup initial values for `GeM` (**MY**)
#' @description Set initial values for all the variables in the `GeM` module
#' (**MY**) for the \eqn{s^{th}} species.
#' + [make_MY_inits_GeM] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or
#' see the inline module documentation.
#' @inheritSection GeM State Variables
#' @inheritSection GeM Accessory Variable
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @keywords internal
#' @export
setup_MY_inits.GeM = function(xds_obj, options=list(), s=1){with(xds_obj$MY_obj[[s]], {
  Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)
  Upsilon = expm(-Omega*eip)
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_GeM(nPatches, Upsilon, options)
  return(xds_obj)
})}


#' @title Make inits for GeM adult mosquito model
#' @param nPatches the number of patches
#' @param Upsilon a matrix describing survival and dispersal through the initial EIP
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @inheritSection GeM State Variables
#' @inheritSection GeM Accessory Variable
#' @seealso [GeM], [setup_MY_inits.GeM]
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

#' @title Change initial values for `GeM` (**MY**)
#' @description For the `GeM` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#' @inheritSection GeM State Variables
#' @inheritParams change_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_inits.GeM <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]]$inits, with(options,{
    xds_obj$MYinits[[s]]$M = M
    xds_obj$MYinits[[s]]$Y = Y
    xds_obj$MYinits[[s]]$P = P
    xds_obj$MYinits[[s]]$Z = Z
    return(xds_obj)
  }))}




#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritSection GeM State Variables
#' @inheritSection GeM Parameters
#' @inheritSection GeM Dynamics
#' @inheritParams steady_state_MY
#' @seealso [GeM]
#' @return a [list]
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



#' @title Get the feeding rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector
#' @keywords internal
#' @export
get_f.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector
#' @keywords internal
#' @export
get_q.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector
#' @keywords internal
#' @export
get_g.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector
#' @keywords internal
#' @export
get_sigma.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Get the initial mosquito population density 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial mosquito population density 
#' @keywords internal
#' @export
get_M.GeM = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial infective mosquito population density 
#' @keywords internal
#' @export
get_Z.GeM = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}
