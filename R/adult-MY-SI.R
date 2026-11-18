# specialized methods for the adult mosquito SI model

#' @title The `SI` module for the **MY** component
#' 
#' @description
#' 
#' This implements an `SI` 
#' compartmental model for mosquito infection dynamics.
#' Adult mosquitoes are either susceptible or infected and infectious,
#' and infected mosquitoes die rather than recover from infection.
#' In its simplest form, the dynamics describe 
#' adult mosquito ecology \eqn{(M)} and infection dynamics \eqn{(Y)}: 
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda - g M \\ 
#' dY/dt &= fq\kappa(M-Y) - g Y
#' \end{array}
#' }
#' It is equivalent to an S-I compartmental model under a change of variables \eqn{S = M-Y} and \eqn{I=Y.}
#' 
#' The `SI` module is a generalized implementation of the S-I model:
#' 
#' + the module adds a variable to compute the density of parous mosquitoes 
#' 
#' + mosquito ecology and infection dynamics are spatial -- a patch-based meta-population model -- so state variables and most parameters are vectors of length `nPatches.` The dispersal matrix has dimension `nPatches` \eqn{\times} `nPatches`
#' 
#' + biting by infectious mosquitoes is computed at time \eqn{t} as if the infected mosquitoes 
#' had survived through the EIP \eqn{(\tau)}.  
#' 
#' + parameters describe egg laying by the adult mosquito population
#' 
#' + several of the the core bionomic parameters are set up as *ports* and designed to be computed as a *changing baseline* that can be *modified by vector control* 
#' 
#' The [SI Vignette](https://dd-harp.github.io/ramp.xds/articles/adult_si.html) discusses the model family, and the module design and implementation. 
#'  
#' 
#' @section State Variables:
#' 
#' \describe{
#'   \item{`M`}{density of adult mosquitoes}
#'   \item{`P`}{density of parous adult mosquitoes}
#'   \item{`Y`}{density of infected adult mosquitoes}
#' }
#' 
#' 
#' @section Dynamical Terms: 
#' 
#' \describe{
#'   \item{`Lambda` or \eqn{\Lambda}}{the number of adult mosquitoes emerging, per patch, per day, computed by [Emergence]}
#'   \item{`kappa` or \eqn{\kappa}}{net infectiousness: the probability a mosquito becomes infected after blood feeding on a human, is computed by [Transmission]}
#' }
#' 
#' @section Parameters:
#' \describe{
#'   \item{`f`}{blood feeding rates --- use [setup_f] or [setup_blood_feeding_rates]}
#'   \item{`q`}{human fraction --- use [setup_q] or [setup_human_fractions]}
#'   \item{`g`}{mosquito mortality rate --- use [setup_g] or [setup_mosquito_mortality]}
#'   \item{`eip` or \eqn{\tau}}{extrinsic incubation period --- use [setup_eip]}
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_emigration_rates]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss rate --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{the egg batch rate, in batches per mosquito per day --- use [setup_nu] or [setup_egg_batch_rates]}
#'   \item{`eggsPerBatch` or \eqn{\Gamma}}{the number of eggs per batch}
#' }
#' 
#' After basic setup, [change_MY_pars] can be used for static parameters, or for 
#' for parameters that are set up as static ports (the default).
#' Use advanced setup to modify dynamic ports. 
#'  
#' @section Survival and Dispersal:
#' 
#' Mosquito survival and dispersal are described by a single matrix, \eqn{\Omega.}
#'
#' The parameter \eqn{g} is interpreted as the per-capita mortality rate for mosquitoes
#' while they are in a patch. Mosquitoes emigrate from patches at the per-capita rates \eqn{\sigma},  
#' and some fraction of emigrating mosquitoes is lost (\eqn{\mu}). The total rate of loss from
#' a patch is \deqn{g + \mu \sigma.}
#'  
#' The columns of the mosquito dispersal matrix \eqn{(K)} sum
#' to zero. The default matrix is all zeros, but for non-trivial dispersal matrices:  
#' \deqn{\mbox{diag}\left(K\right)=-1} 
#' and the non-diagonal elements are fraction of surviving mosquitoes that arrive at another patch after
#' a flight bout. (If there is any population loss associated with emigration, it goes in the term \eqn{\mu}.) 
#' 
#' The matrix \eqn{\Omega} is computed as
#' \deqn{\Omega = \mbox{diag}\left(g + \sigma \mu \right) - K \cdot \mbox{diag}\left(\sigma \left(1-\mu\right)\right)}
#' 
#' 
#'
#' @section Blood Feeding:
#' 
#' The overall blood feeding rate (\eqn{f}) and the human fraction (\eqn{q}) are configured as *ports.* 
#' By default, the baseline values are constant, but they can be modified by control.
#' At their baseline, the parameter values *ought* to be subject to some natural constraints:
#' 
#' + If no blood hosts are available, then it should be true that \eqn{f=0.} 
#' 
#' + If no humans are available, then it should be true that \eqn{q=0.}
#' 
#' + If only humans are available, then it should be true that \eqn{q=1.}
#' 
#' Using advanced setup options, the blood feeding parameters can be forced by variables 
#' describing resource availability or weather. 
#' 
#' @section Dynamics: 
#' 
#' **Ecology** 
#' 
#' Adult mosquitoes emerge from aquatic habitats \eqn{(\Lambda)} and disperse and die (\eqn{\Omega}):
#' \deqn{
#' dM/dt = \Lambda - \Omega \cdot M 
#' }
#' The model also computes the density of parous adult mosquitoes: those that have
#' blood fed at least once: 
#' \deqn{
#' dP/dt = fq(M-P) - \Omega \cdot P \\
#' }
#' Parity is computed during parsing as \eqn{P/M} and its orbit is stored as `parity.`
#' 
#' 
#' **Infection Dynamics**
#'  
#' Given the per-capita daily rate of human blood feeding \eqn{(fq)} and 
#' net infectiousness \eqn{(\kappa)}, the FoI for mosquitoes is \eqn{fq\kappa}. 
#' Changes in the density of infected mosquitoes
#' is given by: 
#' \deqn{
#' dY/dt = fq\kappa(M-Y) - \Omega \cdot Y \\
#' }
#' 
#' 
#' @section Infective Biting: 
#' 
#' **Infective biting density** is defined as the number of human infectious bites by the mosquito population in each patch, each day. 
#' In `SI`, infective biting density is computed if infected mosquitoes at time \eqn{t}, \eqn{Y(t),} had survived
#' and dispersed through an EIP of \eqn{\tau} days \eqn{e^{- \Omega \tau}}.  
#' 
#' \describe{
#'   \item{\eqn{Z}}{density of infectious mosquitoes, imputed from \eqn{Y}}
#'   \item{`fqZ`}{Infective biting density: the number of infectious bites, per patch, per day \deqn{fqZ = e^{-\Omega \tau} \cdot \left( f q Y \right) }}
#'   \item{`fqM`}{Human biting density: the number of human infectious bites, per patch, per day}
#' } 
#' 
#' Note that \eqn{e^{-\Omega \tau}} is computed with the matrix exponential function [expm]
#' 
#' **The EIR**  
#'
#' \eqn{fqZ} is the dynamical term computed by [F_fqZ.SI]. It is called 
#' by [compute_EIR] (in [Transmission]) to compute and store `EIR,` 
#' the local daily EIR \eqn{(E)}. Note that
#' `fqZ` is a vector of length `nPatches` but the
#' local EIR is a vector of length `nStrata.` 
#' The **XY** interface uses 
#' information about humans and mosquitoes to build the biting distribution matrix, \eqn{\beta,} and if 
#' there are no visitors, then 
#' \deqn{E = \beta \cdot fqZ}
#' The orbits of `fqZ` and `EIR` are computed and stored by [xds_solve] by the functions that parse the outputs. 
#' 
#' **The HBR**  
#' 
#' Since estimates of the EIR are computed by taking the product of the human biting rate (HBR or \eqn{B}) and the sporozoite rate, 
#' there is interest in knowing \eqn{fqM} and \eqn{B.} The quantity `fqM` is computed by [F_fqM.SI].  
#' The orbits of `fqM` and `HBR` are also computed during parsing, as 
#' \deqn{B = \beta \cdot fqM} 
#' 
#' @section Egg Laying:
#' 
#' \describe{
#'   \item{`G`}{the number of eggs laid, per patch, per day \deqn{G = \nu M \Gamma }}
#' }
#' 
#' \eqn{G} is the dynamical term passed to [EggLaying] to distribute eggs among the habitats
#' that are found in each patch.
#'
#' @seealso
#' 
#' + Basic Setup: [make_MY_obj_SI], [setup_MY_obj.SI]
#' + Dynamics: [dMYdt.SI]
#' + Parameters: [get_MY_pars.SI], [change_MY_pars.SI]
#' + Variables: [make_MY_inits_SI], [get_MY_inits], [change_MY_inits.SI], [setup_MY_inits.SI]
#' + The [SI Vignette](https://dd-harp.github.io/ramp.xds/articles/adult_si.html)
#' 
#' @name SI
#' @rdname SI
NULL


#' @title Get the `SI` (**MY**) skill set
#'
#' @inheritParams get_skillset_MY
#'
#' @return the **MY** module skill set, as a list
#' @seealso [SI]
#'
#' @keywords internal
#' @export
get_skillset_MY.SI = function(xds_obj, s){
  skills = list(
    dts = TRUE,
    xde = TRUE,
    delay = FALSE
  )
  return(skills)
}

#' Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @seealso [SI]
#' @keywords internal
#' @export
check_MY.SI = function(xds_obj, s){
  return(xds_obj)
}

#' @title Compute derivatives for `SI` (**MY**)
#' 
#' @description
#' 
#' The derivatives for the `SI` module are:
#' 
#' \deqn{
#' \begin{array}{rl}
#' dM/dt = & \Lambda - \Omega \cdot M \\
#' dP/dt = & f q(M-P) - \Omega \cdot P \\
#' dY/dt = & f q \kappa (M-Y) - \Omega \cdot Y
#' \end{array}
#' }
#' 
#' @inheritSection SI State Variables
#' @inheritSection SI Dynamical Terms
#' @inheritSection SI Parameters
#' @inheritSection SI Dynamics
#' 
#' @inheritParams dMYdt
#' @seealso [SI], [F_fqZ.SI], [F_eggs.SI]
#' @return a vector with the derivatives
#' @keywords internal
#' @export
dMYdt.SI <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      dM <- Lambda - (Omega %*% M)
      dP <- f*q*(M-P) - (Omega %*% P)
      dY <- f*q*kappa*(M-Y) - (Omega %*% Y)
      
      return(c(dM, dP, dY))
    })
  })
}


#' @title Update state variables for `SI` (**MY**)
#' @description Implements [Update_MYt] for the SI model.
#' @inheritSection SI Dynamics
#' @inheritParams Update_MYt
#' @seealso [SI], [dMYdt.SI]
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_MYt.SI <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{
      Omega = expm(-Omega)
      Mt <- Lambda + Omega %*% M
      Pt <- Omega%*%((1-exp(-f*q))*(M-P)) + (Omega %*% P)
      Yt <- Omega%*%((1-exp(-f*q*kappa))*(M-Y)) + (Omega %*% Y)
      return(list(M=unname(Mt), P = unname(Pt), Y=unname(Yt)))
    })
  })
}

#' @title Infective biting for `SI`
#' 
#' @description
#' Compute infective biting, the number of infectious bites by the mosquito population on humans in each patch, each day.
#'
#'
#' @inheritSection SI Infective Biting 
#' @inheritParams F_fqZ
#' @seealso [SI], [dMYdt.SI]
#' @return a [numeric] vector of length `nPatches`
#' @importFrom expm expm
#' @keywords internal
#' @export
F_fqZ.SI <- function(t, y, xds_obj, s) {
  Y = y[xds_obj$MY_obj[[s]]$ix$Y_ix]
  fqZ = with(xds_obj$MY_obj[[s]], f*q*(Upsilon %*%Y))
  return(fqZ)
}

#' @title Human biting or `SI`
#' 
#' @description 
#' Compute human biting, the number of bites by the mosquito population on humans in each patch, each day.
#' 
#' @inheritSection SI Infective Biting 
#' @inheritParams F_fqM
#' @seealso [SI]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.SI <- function(t, y, xds_obj, s){
  M = y[xds_obj$MY_obj[[s]]$ix$M_ix]
  fqM = with(xds_obj$MY_obj[[s]], f*q*M)
  return(fqM)
}

#' @title Compute component egg production rates for `SI`
#' @description The density of adult mosquitoes is \eqn{M}.
#' The **parameters** describing egg laying by adult mosquitoes are:
#' - \eqn{\nu} or `nu` is the egg batch rate
#' - \eqn{\Gamma} or `eggsPerBatch` is the number of eggs per batch
#'
#' The egg production rate per patch is \deqn{\nu \Gamma M}
#'
#' @inheritSection SI Egg Laying
#' @inheritParams F_eggs
#' @seealso [SI]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.SI <- function(t, y, xds_obj, s) {
  
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Mosquito bionomics for `SI` (**MY**)
#' @description Reset the effect sizes for static models.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritSection SI Parameters
#' @inheritParams MEffectSizes
#' @seealso [SI], [MEffectSizes.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.SI <- function(t, y, xds_obj, s) {

  xds_obj$MY_obj[[s]]$f_t      <- F_f(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t      <- F_q(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t      <- F_g(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t  <- F_sigma(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu       <- F_mu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu       <- F_nu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$eip      <- F_eip(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$K_matrix <- F_K_matrix(t, xds_obj, s)

  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  return(xds_obj)
}

#' @title Apply effect sizes for `SI` (**MY**)
#' @description Reset the effect sizes for static models.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritSection SI Parameters
#' @inheritParams MEffectSizes
#' @seealso [SI], [MBionomics.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.SI <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    xds_obj$MY_obj[[s]]$f <- es_f*f_t
    xds_obj$MY_obj[[s]]$q <- es_q*q_t
    xds_obj$MY_obj[[s]]$g <- es_g*g_t
    xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
    xds_obj <- update_Omega(xds_obj, s)
    return(xds_obj)
  })}



#' @title Set up `SI` (**MY**)
#' @description 
#' 
#' This sets up an `SI` model object for **MY** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_MY_obj_SI]
#' + the initial conditions are set by [setup_MY_inits]
#' + `options` is passed to both to override the defaults
#' 
#' @inheritSection SI State Variables
#' @inheritSection SI Parameters
#' 
#' @seealso [SI], [make_MY_obj_SI], [setup_MY_inits.SI]
#' 
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#' 
#' @inheritParams setup_MY_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_obj.SI = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]] <- make_MY_obj_SI(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$skill_set <- get_skillset_MY(xds_obj, s)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, TRUE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  return(xds_obj)
}

#' @title Make parameters for SI ODE adult mosquito model
#' @inheritSection SI Parameters
#' 
#' @param nPatches is the number of patches, an integer
#' @param options a named list: named values override defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @seealso [SI], [setup_MY_obj.SI]
#' @keywords internal
#' @export
make_MY_obj_SI = function(nPatches, options=list(), eip=0,
                          g=1/12, sigma=1/8, mu=0, f=0.3, q=0.95,
                          nu=1, eggsPerBatch=60){

  with(options,{
    MY_obj <- list()
    MY_obj$nPatches <- nPatches

    eip_par <- 'static'
    class(eip_par) <- 'static'

    MY_obj <- setup_eip_obj(checkIt(eip, nPatches), MY_obj)
    MY_obj <- setup_f_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_q_obj(checkIt(q, nPatches), MY_obj)
    MY_obj <- setup_g_obj(checkIt(g, nPatches), MY_obj)
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj)
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj)
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj)
    MY_obj$eggsPerBatch <- eggsPerBatch
    
    class(MY_obj) = "SI" 

    return(MY_obj)
  })}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the `SI` model.
#' @inheritParams setup_MY_ix
#' @inheritSection SI State Variables
#' @return a [list]
#' @seealso [SI]
#' @keywords internal
#' @importFrom utils tail
#' @export
setup_MY_ix.SI <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)
  
  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix, Y_ix=Y_ix)
  return(xds_obj)
})}


#' @title List variables for `SI` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritSection SI State Variables
#' @inheritParams get_MY_vars
#' @return a [list]
#' @seealso [SI]
#' @keywords internal
#' @export
get_MY_vars.SI <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         Y = y[Y_ix]
       )))
}


#' @title Parse outputs for `SI` (**MY**)
#' @description Implements [parse_MY_orbits] for the `SI` model
#' @inheritSection SI State Variables
#' @inheritSection SI Infective Biting 
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @seealso [SI]
#' @keywords internal
#' @export
parse_MY_orbits.SI <- function(outputs, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    M = outputs[,ix$M_ix]
    P = outputs[,ix$P_ix]
    Y = outputs[,ix$Y_ix]
    Z = t(Upsilon %*% t(Y))
    y = Y/M
    z = Z/M
    return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parity = P/M))
})}


#' @title Get parameters for `SI` (**MY**)
#' 
#' @description Get the stored baseline values of the bionomic parameters
#' for `SI` 
#' 
#' @inheritSection SI Parameters
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @seealso [SI], [change_MY_pars.SI]
#' @keywords internal
#' @export
get_MY_pars.SI <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, K_matrix=K_matrix
  ))
}

#' @title Change parameters for `SI` (**MY**)
#' @description For the `SI` module (**MY**) for the \eqn{s^{th}} species,
#' change *static* parameter values named in `options` leaving other values unchanged.
#' 
#' @note 
#' This is designed to work for static parameters or 
#' for parameters that are set up as ports, where the class of
#' the bionomic object is "static." 
#' 
#' Use advanced setup to modify dynamic bionomic ports. 
#' 
#' @inheritSection SI Parameters
#' 
#' @inheritParams change_MY_pars
#' @seealso [SI], [get_MY_pars.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_pars.SI <- function(xds_obj, options=list(), s=1) {
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


#' @title Setup initial values for `SI` (**MY**)
#' @description Set initial values for all the variables in the `SI` module
#' (**MY**) for the \eqn{s^{th}} species.
#' + [make_MY_inits_SI] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or
#' see the inline module documentation.
#' @inheritSection SI State Variables
#' 
#' @inheritParams setup_MY_inits
#' @seealso [SI], [make_MY_inits_SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_inits.SI = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_SI(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Make inits for `SI` adult mosquito model
#' @inheritSection SI State Variables
#' 
#' @param nPatches the number of patches
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @return a [list]
#' @seealso [SI], [setup_MY_inits.SI]
#' @keywords internal
#' @export
make_MY_inits_SI = function(nPatches, options = list(),
                            M=10, P=8, Y=1){
  stopifnot(P<M)
  stopifnot(Y<M)
  stopifnot(Y<P)
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    Y = checkIt(Y, nPatches)
    return(list(M=M,  P=P, Y=Y))
  })
}

#' @title Change initial values for `SI` (**MY**)
#' @description For the `SI` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#'
#' @inheritSection SI State Variables
#' 
#' @inheritParams change_MY_inits
#'
#' @seealso [SI], [get_MY_inits], [make_MY_inits_SI], [setup_MY_inits.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_inits.SI = function(xds_obj, options=list(), s=1){
  inits =  with(get_MY_inits(xds_obj, s), with(options,{
    list(M=M, Y=Y)}))
  xds_obj$MY_obj[[s]]$inits=inits
  return(xds_obj)
}

#' @title Get the feeding rates
#' @inheritSection SI Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector
#' @seealso [SI]
#' @keywords internal
#' @export
get_f.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @inheritSection SI Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector
#' @seealso [SI]
#' @keywords internal
#' @export
get_q.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @inheritSection SI Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector
#' @seealso [SI]
#' @keywords internal
#' @export
get_g.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rates
#' @inheritSection SI Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector
#' @seealso [SI]
#' @keywords internal
#' @export
get_sigma.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Get the initial mosquito population density 
#' @inheritSection SI State Variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial mosquito population density 
#' @seealso [SI], [get_MY_inits]
#' @keywords internal
#' @export
get_M.SI = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density 
#' @inheritSection SI Infective Biting 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial infective mosquito population density 
#' @seealso [SI], [get_MY_inits]
#' @keywords internal
#' @export
get_Z.SI = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}

#' @title Steady states: MY-SI
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritSection SI Dynamics
#' @inheritParams steady_state_MY
#' @return a [list]
#' @seealso [SI], [dMYdt.SI]
#' @keywords internal
#' @export
steady_state_MY.SI_ode = function(Lambda, kappa, xds_obj, s=1){
  with(xds_obj$MY_obj[[s]],{
    Omega_inv <- solve(Omega)
    M_eq <- as.vector(Omega_inv %*% Lambda)
    Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
    return(list(M=M_eq, Y=Y_eq))
})}
