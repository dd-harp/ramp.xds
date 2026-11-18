# specialized methods for the adult mosquito macdonald model

#' @title The `macdonald` module for the **MY** component
#'
#' @description
#' This implements a Macdonald-style delay differential equation model for
#' adult mosquito ecology and infection dynamics, consistent with the model
#' published by George Macdonald in 1952. This formulation is closer to one
#' published by Aron and May (1982). Unlike the `SI` module, it includes
#' infectious mosquitoes as an explicit state variable and uses delayed values
#' to represent the extrinsic incubation period.
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
#'   \item{`nu` or \eqn{\nu}}{egg batch rate --- use [setup_nu] or [setup_egg_batch_rates]}
#'   \item{`eggsPerBatch` or \eqn{\Gamma}}{number of eggs per batch}
#' }
#'
#' After basic setup, [change_MY_pars.macdonald] can be used to change static
#' parameter values. This module does not support dynamic bionomic ports.
#'
#' @section Survival and Dispersal:
#' Mosquito survival and dispersal are described by the demographic matrix
#' \eqn{\Omega}. The parameter \eqn{g} is the per-capita mortality rate while
#' mosquitoes are in a patch. Mosquitoes emigrate from patches at per-capita
#' rates \eqn{\sigma}, and a fraction \eqn{\mu} of emigrating mosquitoes is
#' lost. The total rate of loss from a patch is \deqn{g+\mu\sigma.}
#'
#' The columns of the mosquito dispersal matrix \eqn{K} sum to zero. For
#' non-trivial dispersal matrices, \eqn{\mbox{diag}(K)=-1}; the off-diagonal
#' elements describe the fractions of surviving emigrants that arrive in
#' other patches. The demographic matrix is
#' \deqn{\Omega=\mbox{diag}(g+\sigma\mu)-K\cdot\mbox{diag}(\sigma(1-\mu)).}
#' Survival and dispersal through the EIP are represented by
#' \deqn{\Upsilon=e^{-\Omega\tau}.}
#'
#' @section Blood Feeding:
#' The blood feeding rate \eqn{f} and human fraction \eqn{q} are parameters of
#' the module. At baseline they are static. They should satisfy natural
#' constraints: \eqn{f=0} if no blood hosts are available, \eqn{q=0} if no
#' humans are available, and \eqn{q=1} if humans are the only available hosts.
#' This module does not support the dynamic blood-feeding ports used by `SI`
#' and `GeM`.
#'
#' @section Dynamics:
#' The system is described by coupled differential equations with a delay.
#' A subscript \eqn{\tau} denotes a lagged value, for example
#' \eqn{M_\tau=M(t-\tau)}.
#'
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda-\Omega\cdot M \\\\
#' dP/dt &= f(M-P)-\Omega\cdot P \\\\
#' dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y \\\\
#' dZ/dt &= \Upsilon\cdot(fq\kappa)_\tau(M_\tau-Y_\tau)-\Omega\cdot Z
#' \end{array}
#' }
#'
#' Here, \eqn{P} is the density of mosquitoes that have blood-fed at least
#' once. The infected mosquito density \eqn{Y} generates infectious mosquitoes
#' after the EIP, represented by the delayed term in the \eqn{Z} equation.
#'
#' @section Infective Biting:
#' Infective biting density is the number of infectious bites by mosquitoes
#' on humans in each patch per day. The density of infectious mosquitoes
#' \eqn{Z} is computed from \eqn{Y} after survival and dispersal through the
#' EIP.
#'
#' \describe{
#'   \item{`fqZ`}{infective biting density, the number of infectious bites per patch per day}
#'   \item{`fqM`}{human biting density, the number of bites on humans per patch per day}
#' }
#'
#' [F_fqZ.macdonald] and [F_fqM.macdonald] compute these patch-level
#' quantities. [compute_EIR] combines `fqZ` with the biting distribution
#' matrix and resident-bite fraction to compute stratum-level EIR; [F_hbr]
#' computes HBR from `fqM` in the same way. The resulting `EIR` and `HBR`
#' orbits are attached to the XH output during parsing.
#'
#' @section Egg Laying:
#' The egg batch rate \eqn{\nu} and the number of eggs per batch
#' \eqn{\Gamma} determine the egg production rate per patch:
#' \deqn{G=\nu\Gamma M.}
#' [F_eggs.macdonald] computes \eqn{G}, which is passed to [EggLaying] for
#' distribution among aquatic habitats.
#'
#' @section Note:
#' This model is not capable of handling exogenous forcing by weather or
#' vector control. Use the `GeM` module instead.
#'
#' @seealso [make_MY_obj_macdonald], [setup_MY_obj.macdonald], [dMYdt.macdonald]
#'
#' @references
#' + Macdonald G (1952) The analysis of the sporozoite rate. Tropical Diseases Bulletin 49:569-586.
#' + Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5 in *The Population Dynamics of Infectious Diseases: Theory and Applications,* Springer, Boston, MA.
#' @name macdonald
NULL


#' @title The skill set for `macdonald` (**MY**)
#'
#' @inheritParams get_skillset_MY
#'
#' @return the **MY** module skill set, as a list
#' @seealso [macdonald]
#'
#' @keywords internal
#' @export
get_skillset_MY.macdonald = function(xds_obj, s){
  skills = list(
    dts = FALSE,
    xde = TRUE,
    delay = TRUE,
    not_implemented = TRUE
  )
  return(skills)
}

#' @title Check `macdonald` (**MY**) before solving
#'
#' @description This method currently performs no additional checks.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @seealso [macdonald]
#' @keywords internal
#' @export
check_MY.macdonald = function(xds_obj, s){
  return(xds_obj)
}

#' @title Compute derivatives for `macdonald` (**MY**)
#' @description Implements the delay differential equations for the
#' `macdonald` module.
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Dynamical Terms
#' @inheritSection macdonald Parameters
#' @inheritSection macdonald Dynamics
#' @inheritParams dMYdt
#' @seealso [macdonald], [F_fqZ.macdonald], [F_eggs.macdonald]
#' @return a [numeric] vector
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
#' @keywords internal
#' @export
dMYdt.macdonald <- function(t, y, xds_obj, s){

  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      if (t <= eip) {
        M_eip <- inits$M
        Y_eip <- inits$Y
        kappa_eip <- kappa
      } else {
        M_eip <- lagvalue(t=t-eip, nr = ix$M_ix)
        Y_eip <- lagvalue(t=t-eip, nr = ix$Y_ix)
        kappa_eip <- lagderiv(t=t-eip, nr = ix$kappa_ix)
      }
      
      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      dYdt <- f*q*kappa*(M - Y) - (Omega %*% Y)
      dZdt <- Upsilon %*% (f*q*kappa_eip * (M_eip - Y_eip)) - (Omega %*% Z)
      
      return(c(dMdt, dPdt, dYdt, dZdt, kappa))
    })
  })
}




#' @title Infective mosquito biting rate for `macdonald`
#' @description Implements [F_fqZ] for the `macdonald` model.
#' @inheritSection macdonald Infective Biting
#' @inheritParams F_fqZ
#' @seealso [macdonald], [dMYdt.macdonald]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.macdonald <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$Z_ix]
}

#' @title Mosquito biting rate for `macdonald`
#' @description Returns `fqM`, the daily mosquito biting density on humans
#' for each patch in the `macdonald` model.
#' @inheritSection macdonald Infective Biting
#' @inheritParams F_fqM
#' @seealso [macdonald], [F_hbr]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.macdonald <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$M_ix]
}

#' @title Egg production rate for `macdonald`
#' @description Implements [F_eggs] for the `macdonald` model.
#' @inheritSection macdonald Egg Laying
#' @inheritParams F_eggs
#' @seealso [macdonald], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.macdonald <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Set up `macdonald` (**MY**)
#' @description
#'
#' This sets up a `macdonald` model object for the **MY** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_MY_obj_macdonald]
#' + the initial conditions are set by [setup_MY_inits]
#' + `options` is passed to both to override the defaults
#'
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Parameters
#'
#' @seealso [macdonald], [make_MY_obj_macdonald], [setup_MY_inits.macdonald]
#'
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#'
#' @inheritParams setup_MY_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_obj.macdonald = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MYname = "macdonald"
  xds_obj$MY_obj[[s]] = make_MY_obj_macdonald(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$skill_set <- get_skillset_MY(xds_obj, s)
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, TRUE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  return(xds_obj)
}

#' @title Make parameters for `macdonald` adult mosquito model
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Parameters
#' @param nPatches is the number of patches, an integer
#' @param options a [list] of values that overrides the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu egg batch rate, in batches per mosquito per day
#' @param eggsPerBatch number of eggs per batch
#' @return a [list]
#' @seealso [macdonald], [setup_MY_obj.macdonald]
#' @importFrom expm expm
#' @keywords internal
#' @export
make_MY_obj_macdonald = function(nPatches, options=list(), eip=12,
                                   g=1/12, sigma=1/8, mu=0, f=0.3, q=0.95,
                                   nu=1, eggsPerBatch=60){

  with(options,{
    MY_obj <- list()
    class(MY_obj) <- 'macdonald'
    MY_obj$nPatches <- nPatches

    MY_obj$eip    <- checkIt(eip, 1)
    MY_obj$f      <- checkIt(f, nPatches)
    MY_obj$q      <- checkIt(q, nPatches)
    MY_obj$g      <- checkIt(g, nPatches)
    MY_obj$sigma  <- checkIt(sigma, nPatches)
    MY_obj$mu     <- checkIt(mu, nPatches)
    MY_obj$nu     <- checkIt(nu, nPatches)
    MY_obj$eggsPerBatch <- eggsPerBatch
    
    return(MY_obj)
  })}

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the macdonald model.
#' @inheritParams setup_MY_ix
#' @inheritSection macdonald State Variables
#' @return a [list]
#' @seealso [macdonald]
#' @importFrom utils tail
#' @keywords internal
#' @export
setup_MY_ix.macdonald <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  kappa_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(kappa_ix, 1)

  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix, Y_ix=Y_ix, Z_ix=Z_ix,
                          kappa_ix=kappa_ix)
  return(xds_obj)
})}

#' @title List variables for `macdonald` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritSection macdonald State Variables
#' @inheritParams get_MY_vars
#' @return a [list]
#' @seealso [macdonald]
#' @keywords internal
#' @export
get_MY_vars.macdonald <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         Y = y[Y_ix],
         Z = y[Z_ix]
)))}

#' @title Parse outputs for `macdonald` (**MY**)
#' @description Implements [parse_MY_orbits] for the macdonald model
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Infective Biting
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @seealso [macdonald]
#' @keywords internal
#' @export
parse_MY_orbits.macdonald <- function(outputs, xds_obj, s) {with(xds_obj$MY_obj[[s]]$ix,{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  Y = outputs[,Y_ix]
  Z = outputs[,Z_ix]
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parous=parous))
})}



#' @title Get parameters for `macdonald` (**MY**)
#' @description Get the stored parameter values for the `macdonald` model.
#' @inheritSection macdonald Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @seealso [macdonald], [change_MY_pars.macdonald]
#' @keywords internal
#' @export
get_MY_pars.macdonald <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f, q=q, g=g, sigma=sigma, eip=eip, mu=mu,
    nu=nu, eggsPerBatch=eggsPerBatch, K_matrix=K_matrix
  ))
}

#' @title Change parameters for `macdonald` (**MY**)
#' @description For the `macdonald` module (**MY**) for the \eqn{s^{th}} species,
#' change static parameter values named in `options` leaving other values unchanged.
#' @inheritSection macdonald Parameters
#' @inheritParams change_MY_pars
#' @seealso [macdonald], [get_MY_pars.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_pars.macdonald <- function(xds_obj, options=list(), s=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f = f
    xds_obj$MY_obj[[s]]$q = q
    xds_obj$MY_obj[[s]]$g = g
    xds_obj$MY_obj[[s]]$sigma = sigma
    xds_obj$MY_obj[[s]]$eip = eip
    xds_obj$MY_obj[[s]]$mu = mu
    xds_obj$MY_obj[[s]]$nu = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    return(xds_obj)
  }))}


#' @title Setup initial values for `macdonald` (**MY**)
#' @description Set initial values for all the variables in the `macdonald` module
#' (**MY**) for the \eqn{s^{th}} species.
#' + [make_MY_inits_macdonald] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or
#' see the inline module documentation.
#' @inheritSection macdonald State Variables
#' @inheritParams setup_MY_inits
#' @seealso [macdonald], [make_MY_inits_macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_inits.macdonald = function(xds_obj, options=list(), s=1){with(xds_obj$MY_obj[[s]], {
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_macdonald(nPatches, options)
  return(xds_obj)
})}


#' @title Make inits for macdonald adult mosquito model
#' @inheritSection macdonald State Variables
#' @param nPatches the number of patches
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @seealso [macdonald], [setup_MY_inits.macdonald]
#' @keywords internal
#' @export
make_MY_inits_macdonald = function(nPatches, options = list(),
                                   M=5, P=1, Y=1, Z=1){
  with(options,{
    M = unname(checkIt(M, nPatches))
    P = unname(checkIt(P, nPatches))
    Y = unname(checkIt(Y, nPatches))
    Z = unname(checkIt(Z, nPatches))
    dk_ix = rep(0, nPatches)
    return(list(M=M, P=P, Y=Y, Z=Z, dk_ix=dk_ix))
})}

#' @title Change initial values for `macdonald` (**MY**)
#' @description For the `macdonald` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#'
#' @inheritSection macdonald State Variables
#' @inheritParams change_MY_inits
#'
#' @seealso [macdonald], [get_MY_inits], [make_MY_inits_macdonald], [setup_MY_inits.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_inits.macdonald = function(xds_obj, options=list(), s=1){
  inits = with(get_MY_inits(xds_obj, s),
            with(options,{
              list(M=M, P=P, Y=Y, Z=Z, dk_ix=dk_ix)
           }))
  xds_obj$MY_obj[[s]]$inits=inits
  return(xds_obj)
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Dynamics
#' @inheritParams steady_state_MY
#' @return a [list]
#' @seealso [macdonald], [dMYdt.macdonald]
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @keywords internal
#' @export
steady_state_MY.macdonald = function(Lambda, kappa, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  Upsilon = expm(-Omega*eip)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}



#' @title Mosquito bionomics for `macdonald` (**MY**)
#' @description This method leaves the `macdonald` bionomics parameters
#' unchanged; the module does not implement dynamic bionomic ports.
#' @inheritSection macdonald Parameters
#' @inheritParams MEffectSizes
#' @seealso [macdonald], [MEffectSizes.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.macdonald <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `macdonald` (**MY**)
#' @description This method leaves the `macdonald` parameters unchanged;
#' the module does not implement effect-size modifications.
#' @inheritSection macdonald Parameters
#' @inheritParams MEffectSizes
#' @seealso [macdonald], [MBionomics.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.macdonald <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Get the feeding rates
#' @inheritSection macdonald Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector 
#' @seealso [macdonald]
#' @keywords internal
#' @export
get_f.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions 
#' @inheritSection macdonald Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector 
#' @seealso [macdonald]
#' @keywords internal
#' @export
get_q.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @inheritSection macdonald Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector 
#' @seealso [macdonald]
#' @keywords internal
#' @export
get_g.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rates
#' @inheritSection macdonald Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector 
#' @seealso [macdonald]
#' @keywords internal
#' @export
get_sigma.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Get the initial mosquito population density 
#' @inheritSection macdonald State Variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial mosquito population density 
#' @seealso [macdonald], [get_MY_inits]
#' @keywords internal
#' @export
get_M.macdonald = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density 
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Infective Biting
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial infective mosquito population density 
#' @seealso [macdonald], [get_MY_inits]
#' @keywords internal
#' @export
get_Z.macdonald = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}
