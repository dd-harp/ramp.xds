# X
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
#' + mosquito ecology and infection dynamics are spatial -- a patch-based meta-population model -- so state variables and most parameters are vectors of length `nPatches`. The dispersal matrix has dimension `nPatches` \eqn{\times} `nPatches`
#' 
#' + biting by infectious mosquitoes is computed at time \eqn{t} as if the infected mosquitoes 
#' had survived through the EIP \eqn{(\tau)}.  
#' 
#' + parameters describe egg laying by the adult mosquito population
#' 
#' + several of the core bionomic parameters are set up as *ports* and designed to be computed as a *changing baseline* that can be *modified by vector control* 
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
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_mosy_emigration]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss fraction --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{the oviposition rate: the number of egg batches laid in each patch, per mosquito, per day --- use [setup_nu] or [setup_egg_batch_rates]}
#'   \item{`eggsPerBatch` or \eqn{\Gamma}}{the number of eggs per batch}
#' }
#' 
#' After basic setup, [change_MY_pars] can be used for static parameters, or
#' for parameters that are set up as static ports (the default).
#' Use advanced setup to modify dynamic ports. 
#'  
#' @section Survival and Dispersal:
#' 
#' Mosquito survival and dispersal are described by a single matrix, \eqn{\Omega.}
#'
#' The parameter \eqn{g} is interpreted as the per-capita mortality rate for mosquitoes
#' while they are in a patch. Mosquitoes emigrate from patches at the per-capita rates \eqn{\sigma},  
#' and some fraction of emigrating mosquitoes is lost (\eqn{\mu}). The combined rate of
#' mortality and loss during emigration is \deqn{g + \mu \sigma.}
#'  
#' The columns of the mosquito dispersal matrix \eqn{(K)} sum
#' to zero. The default matrix is all zeros, but for non-trivial dispersal matrices:  
#' \deqn{\mbox{diag}\left(K\right)=-1} 
#' and the non-diagonal elements are fractions of surviving mosquitoes that arrive at another patch after
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
#' @section ODE:
#' 
#' **Ecology** 
#' 
#' Adult mosquitoes emerge from aquatic habitats at rate \eqn{\Lambda} and disperse and die according to \eqn{\Omega}:
#' \deqn{
#' dM/dt = \Lambda - \Omega \cdot M 
#' }
#' The model also computes the density of parous adult mosquitoes: those that have
#' blood fed at least once: 
#' \deqn{
#' dP/dt = fq(M-P) - \Omega \cdot P \\
#' }
#' 
#' 
#' **Infection Dynamics**
#'  
#' Given the per-capita daily rate of human blood feeding \eqn{(fq)} and 
#' net infectiousness \eqn{(\kappa)}, the FoI for mosquitoes is \eqn{fq\kappa}. 
#' Changes in the density of infected mosquitoes
#' are given by:
#' \deqn{
#' dY/dt = fq\kappa(M-Y) - \Omega \cdot Y \\
#' }
#' 
#' 
#' @section DTS:
#'
#' **Ecology**
#'
#' Adult mosquitoes emerge from aquatic habitats at rate \eqn{\Lambda} and
#' disperse and die according to \eqn{\Omega}. The one-day update in
#' [Update_MYt.SI] is:
#' \deqn{M_t = \Lambda + e^{-\Omega}\cdot M_{t-1}}
#' The model also computes the density of parous adult mosquitoes: those that have
#' blood fed at least once:
#' \deqn{P_t = e^{-\Omega}\cdot\left[(1-e^{-fq})(M_{t-1}-P_{t-1}) + P_{t-1}\right]}
#'
#' **Infection Dynamics**
#'
#' Given the per-capita daily rate of human blood feeding \eqn{(fq)} and
#' net infectiousness \eqn{(\kappa)}, the FoI for mosquitoes is \eqn{fq\kappa}.
#' The density of infected mosquitoes is updated by:
#' \deqn{Y_t = e^{-\Omega}\cdot\left[(1-e^{-fq\kappa})(M_{t-1}-Y_{t-1}) + Y_{t-1}\right]}
#'
#' @note Parity is computed during parsing as \eqn{P/M} and its orbit is stored as `parity`.
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
#'   \item{`fqM`}{Human biting density: the number of bites on humans, per patch, per day}
#' } 
#' 
#' Note that \eqn{e^{-\Omega \tau}} is computed with the matrix exponential function [expm]
#' 
#' **The EIR**  
#'
#' \eqn{fqZ} is the dynamical term computed by [F_fqZ.SI]. It is called 
#' by [compute_EIR] (in [Transmission]) to compute and store `EIR`,
#' the local daily EIR \eqn{(E)}. Note that
#' `fqZ` is a vector of length `nPatches` but the
#' local EIR is a vector of length `nStrata`.
#' The **XY** interface uses 
#' information about humans and mosquitoes to build the biting distribution matrix, \eqn{\beta,} and if 
#' there are no visitors, then 
#' \deqn{E = \beta \cdot fqZ}
#' 
#' **The HBR**  
#' 
#' Since estimates of the EIR are computed by taking the product of the human biting rate (HBR or \eqn{B}) and the sporozoite rate, 
#' there is interest in knowing \eqn{fqM} and \eqn{B.} The quantity `fqM` is computed by [F_fqM.SI].  
#' The orbits of `fqM` and `HBR` are also computed during parsing, as 
#' \deqn{B = \beta \cdot fqM} 
#' 
#' @note The orbits of `fqZ` and `EIR` are computed and stored by [xds_solve] by the functions that parse the outputs.
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

#' @title Compute the derivatives for `SI` (**MY**)
#' 
#' @description Compute the derivatives for the `SI` module:
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
#' @inheritSection SI ODE
#' 
#' @inheritParams dMYdt
#' @seealso [SI], [F_fqZ.SI], [F_eggs.SI]
#' @note Derivatives are returned in blocks for `M`, `P`, and `Y`, each in patch order.
#' @return a [numeric] vector of derivatives in the module's state-variable order
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


#' @title Update states for `SI` (**MY**)
#' @description Update the state variables for the `SI` module:
#' \deqn{\begin{array}{rl}
#' M_t &= \Lambda+e^{-\Omega}\cdot M_{t-1} \\\\
#' P_t &= e^{-\Omega}\cdot\left[(1-e^{-fq})(M_{t-1}-P_{t-1})+P_{t-1}\right] \\\\
#' Y_t &= e^{-\Omega}\cdot\left[(1-e^{-fq\kappa})(M_{t-1}-Y_{t-1})+Y_{t-1}\right]
#' \end{array}}
#' @inheritSection SI State Variables
#' @inheritSection SI Dynamical Terms
#' @inheritSection SI Parameters
#' @inheritSection SI DTS
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
      return(c(Mt, Pt, Yt))
    })
  })
}


#' @title Get the state variables for `SI` (**MY**)
#' @description Extract the state variables for the `SI` module
#' from `y` and return them by name in a list for use during model solving.
#' @inheritSection SI State Variables
#' @inheritParams get_MY_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @seealso [SI]
#' @keywords internal
#' @export
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
get_MY_vars.SI <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         Y = y[Y_ix]
       )))
}


#' @title Mosquito bionomics for `SI` (**MY**)
#' @description Update mosquito bionomics and reset effect sizes for the `SI` module.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritSection SI Parameters
#' @inheritParams MBionomics
#' @seealso [SI], [MEffectSizes.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.SI <- function(t, y, xds_obj, s) {
  xds_obj$MY_obj[[s]]$f_t <- F_f(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t <- F_q(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t <- F_g(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t <- F_sigma(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu_t <- F_mu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu_t <- F_nu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$eip_t <- F_eip(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$K_matrix_t <- F_K_matrix(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$es_f <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_mu <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_nu <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_eip <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_K_matrix <- matrix(1, xds_obj$nPatches, xds_obj$nPatches)
  return(xds_obj)
}


#' @title Apply effect sizes for `SI` (**MY**)
#' @description Apply the accumulated effect sizes to the baseline bionomic
#' parameters for the `SI` module and update the mosquito demographic matrix.
#' @inheritSection SI Parameters
#' @inheritParams MEffectSizes
#' @seealso [SI], [MBionomics.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.SI <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], {
    xds_obj$MY_obj[[s]]$f <- f_t * es_f
    xds_obj$MY_obj[[s]]$q <- q_t * es_q
    xds_obj$MY_obj[[s]]$g <- g_t * es_g
    xds_obj$MY_obj[[s]]$sigma <- sigma_t * es_sigma
    xds_obj$MY_obj[[s]]$mu <- mu_t * es_mu
    xds_obj$MY_obj[[s]]$nu <- nu_t * es_nu
    xds_obj$MY_obj[[s]]$eip <- eip_t * es_eip
    xds_obj$MY_obj[[s]]$K_matrix <- K_matrix_t * es_K_matrix
    xds_obj <- change_Omega(xds_obj, s)
    return(xds_obj)
  })
}

#' @title Compute infective mosquito biting density for `SI` (**MY**)
#' @description Return the daily infective mosquito biting density on humans,
#' `fqZ`, for each patch in the `SI` module.
#' Compute `fqZ = f*q*(Upsilon %*% Y)` using the module's
#' survival matrix to obtain infective mosquito density.
#' 
#' @inheritSection SI Infective Biting 
#' @inheritParams F_fqZ
#' @seealso [SI], [dMYdt.SI], [F_fqM.SI]
#' @return a [numeric] vector of length `nPatches`
#' @importFrom expm expm
#' @keywords internal
#' @export
F_fqZ.SI <- function(t, y, xds_obj, s) {
  Y = y[xds_obj$MY_obj[[s]]$ix$Y_ix]
  fqZ = with(xds_obj$MY_obj[[s]], f*q*(Upsilon %*%Y))
  return(as.vector(fqZ))
}

#' @title Compute mosquito biting density for `SI` (**MY**)
#' @description Return the daily mosquito biting density on humans,
#' `fqM`, for each patch in the `SI` module.
#' Compute `fqM = f*q*M`.
#' 
#' @inheritSection SI Infective Biting 
#' @inheritParams F_fqM
#' @seealso [SI], [F_fqZ.SI]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.SI <- function(t, y, xds_obj, s){
  M = y[xds_obj$MY_obj[[s]]$ix$M_ix]
  fqM = with(xds_obj$MY_obj[[s]], f*q*M)
  return(fqM)
}

#' @title Compute egg production for `SI` (**MY**)
#' @description Return the number of eggs laid by the adult mosquito population, per patch
#'
#' @inheritSection SI Egg Laying
#' @inheritParams F_eggs
#' @seealso [SI], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.SI <- function(t, y, xds_obj, s) {
  
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}

#' @title Get the feeding rate for `SI` (**MY**)
#' @description Return the stored realized feeding rate per patch.
#' @inheritSection SI Parameters
#' @inheritParams get_f
#' @seealso [SI]
#' @details Read `f` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_f.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human blood fraction for `SI` (**MY**)
#' @description Return the stored realized human blood fraction per patch.
#' @inheritSection SI Parameters
#' @inheritParams get_q
#' @seealso [SI]
#' @details Read `q` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_q.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the adult mosquito mortality rate for `SI` (**MY**)
#' @description Return the stored realized adult mosquito mortality rate per patch.
#' @inheritSection SI Parameters
#' @inheritParams get_g
#' @seealso [SI]
#' @details Read `g` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_g.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rate for `SI` (**MY**)
#' @description Return the stored realized patch emigration rate per patch.
#' @inheritSection SI Parameters
#' @inheritParams get_sigma
#' @seealso [SI]
#' @details Read `sigma` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_sigma.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}


#' @title Make a module object for `SI` (**MY**)
#' @description Create an `SI` module object and initialize its parameters.
#' Inspect parameters with [get_MY_pars.SI] and change supported values
#' with [change_MY_pars.SI]. Values in `options` override parameter arguments.
#' @seealso [get_MY_pars.SI], [change_MY_pars.SI]
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
#' @param nu the oviposition rate: the number of egg batches laid in each patch,
#' per mosquito, per day
#' @param eggsPerBatch number of eggs per batch
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



#' @title Set up the module object for `SI` (**MY**)
#' @description 
#' 
#' Set up an `SI` module object for the **MY** component and the \eqn{s^{th}} vector species.
#'
#' + Create the module object with [make_MY_obj_SI], passing `nPatches` and `options`.
#' + Set the skill set using [get_skillset_MY].
#' + Set initial values using [setup_MY_inits], passing `options` to override defaults.
#' + Initialize the circadian function using [setup_F_circadian.setup].
#' + Initialize the mosquito demographic matrix port.
#' + Initialize the EIP survival and dispersal matrix port.
#' + Initialize the mosquito dispersal matrix using [setup_K_matrix.zero].
#' + Return the updated **`xds`** model object.
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
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, TRUE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  return(xds_obj)
}


#' @title Get parameters for `SI` (**MY**)
#' @seealso [make_MY_obj_SI], [change_MY_pars.SI]
#' 
#' @description Return the parameters for the `SI` module as a named list.
#' 
#' @inheritSection SI Parameters
#' 
#' @inheritParams get_MY_pars
#' @return a named [list] of parameter values
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
#' @seealso [make_MY_obj_SI], [get_MY_pars.SI]
#' @description Change parameter values named in `options` for the `SI`
#' module, leaving other values unchanged.
#' Refresh baseline bionomics at time zero using [MBionomics], without calling [MEffectSizes].
#' This method changes static parameter values.
#' To change `K_matrix`, use [setup_K_matrix]; this method does not change
#' the dispersal matrix.
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
    
    return(xds_obj)
  }))}


#' @title Make initial values for `SI` (**MY**)
#' @description Create a named list of initial values for the `SI` module.
#' Values in `options` override the defaults.
#' @details Initial values must be numeric scalars or have length `nPatches`.
#' Scalars are expanded to that length. Require `Y < P < M` in every patch.
#' @inheritSection SI State Variables
#' 
#' @param nPatches the number of patches
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @return a named [list] of initial values
#' @seealso [SI], [setup_MY_inits.SI]
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.SI]
make_MY_inits_SI = function(nPatches, options = list(),
                            M=10, P=8, Y=1){
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    Y = checkIt(Y, nPatches)
    stopifnot(P<M)
    stopifnot(Y<M)
    stopifnot(Y<P)
    return(list(M=M,  P=P, Y=Y))
  })
}


#' @title Set up initial values for `SI` (**MY**)
#' @description Set up initial values for the `SI` module using
#' [make_MY_inits_SI]. Values in `options` override the defaults.
#' @inheritSection SI State Variables
#' 
#' @inheritParams setup_MY_inits
#' @seealso [SI], [make_MY_inits_SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.SI]
#' @note Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.
#' @seealso [get_MY_inits]
setup_MY_inits.SI = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_SI(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Change initial values for `SI` (**MY**)
#' @description Change initial values named in `options` for the `SI`
#' module, leaving other values unchanged.
#'
#' @inheritSection SI State Variables
#' 
#' @inheritParams change_MY_inits
#'
#' @seealso [SI], [get_MY_inits], [make_MY_inits_SI], [setup_MY_inits.SI]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Stored initial values for this module are at
#' `xds_obj$MY_obj[[s]]$inits`.
change_MY_inits.SI = function(xds_obj, options=list(), s=1){
  inits =  with(get_MY_inits(xds_obj, s), with(options,{
    list(M=M, P=P, Y=Y)}))
  xds_obj$MY_obj[[s]]$inits=inits
  return(xds_obj)
}

#' @title Get the initial adult mosquito density for `SI` (**MY**)
#' @description Return the initial adult mosquito density per patch.
#' @inheritSection SI State Variables
#' @inheritParams get_M
#' @seealso [SI], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_M.SI = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density for `SI` (**MY**)
#' @description Return the initial infective mosquito density per patch.
#' @inheritSection SI State Variables
#' @inheritSection SI Infective Biting
#' @inheritParams get_Z
#' @seealso [SI], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_Z.SI = function(xds_obj, s=1){
  as.vector(xds_obj$MY_obj[[s]]$Upsilon %*% get_MY_inits(xds_obj, s)$Y)
}


#' @title Set up variable indices for `SI` (**MY**)
#' @description Assign consecutive variable indices for the `SI` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `M_ix`, `P_ix`, `Y_ix`.
#' Each block has length `nPatches`.
#' @inheritParams setup_MY_ix
#' @inheritSection SI State Variables
#' @return an **`xds`** model object
#' @seealso [SI]
#' @keywords internal
#' @importFrom utils tail
#' @export
#' @note Indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
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


#' @title Parse outputs for `SI` (**MY**)
#' @description Extract the model state outputs for the `SI` module
#' from `outputs` and return them by name in a list.
#' Compute `Z = t(Upsilon %*% t(Y))` using the module's survival matrix,
#' and return `y = Y/M`, `z = Z/M`, and `parity = P/M`.
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
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

#' @title Check `SI` (**MY**)
#'
#' @inheritParams check_MY
#'
#' @return an **`xds`** model object
#' @seealso [SI]
#' @keywords internal
#' @export
check_MY.SI = function(xds_obj, t=0, s){
  return(xds_obj)
}


#' @title Get the skill set for `SI` (**MY**)
#'
#' @description Return the capabilities and compatibilities of the `SI` module as a list.
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
    f_is_port = TRUE,
    q_is_port = TRUE,
    g_is_port = TRUE,
    sigma_is_port = TRUE,
    sigma_b_is_port = FALSE,
    sigma_q_is_port = FALSE,
    mu_is_port = TRUE,
    nu_is_port = TRUE,
    eip_is_port = TRUE,
    K_matrix_is_port = TRUE,
    dts = TRUE,
    xde = TRUE,
    delay = FALSE
  )
  return(skills)
}

#' @title Compute steady states for `SI` (**MY**)
#' @description Compute steady-state mosquito densities for the `SI` module
#' given adult mosquito emergence `Lambda` and net infectiousness `kappa`.
#' @note Delegate to [steady_state_MY_SI], which dispatches on `xds_obj$xds`.
#' @inheritParams steady_state_MY
#' @return a named [list] with elements `M`, `P`, and `Y`, each
#' containing one value per patch
#' @seealso [steady_state_MY_SI]
#' @keywords internal
#' @export
steady_state_MY.SI = function(Lambda, kappa, xds_obj, s=1){
  steady_state_MY_SI(Lambda, kappa, xds_obj, s)
}

#' @title Compute steady states for `SI` (**MY**)
#' @description Compute steady-state mosquito densities for the `SI` module
#' given adult mosquito emergence `Lambda` and net infectiousness `kappa`.
#' Only the differential-equation calculation is currently implemented.
#' @note Dispatch on `xds_obj$xds`; only the `xde` method is implemented.
#' @inheritParams steady_state_MY
#' @return a named [list] with elements `M`, `P`, and `Y`, each
#' containing one value per patch
#' @keywords internal
#' @export
steady_state_MY_SI = function(Lambda, kappa, xds_obj, s=1){
  UseMethod("steady_state_MY_SI", xds_obj$xds)
}

#' @title Compute steady states for `SI` (**MY**)
#' @description Compute steady-state mosquito densities for the `SI` module
#' given adult mosquito emergence `Lambda` and net infectiousness `kappa`
#' for the differential-equation system.
#' @inheritSection SI ODE
#' @inheritParams steady_state_MY_SI
#' @return a named [list] with elements `M`, `P`, and `Y`, each
#' containing one value per patch
#' @seealso [SI], [dMYdt.SI]
#' @keywords internal
#' @export
steady_state_MY_SI.xde = function(Lambda, kappa, xds_obj, s=1){
  with(xds_obj$MY_obj[[s]],{
    Omega_inv <- solve(Omega)
    M_eq <- as.vector(Omega_inv %*% Lambda)
    P_eq <- as.vector(solve(diag(f*q, nPatches) + Omega) %*% diag(f*q, nPatches) %*% M_eq)
    Y_eq <- as.vector(ginv(diag(f*q*kappa, nPatches) + Omega) %*% diag(f*q*kappa, nPatches) %*% M_eq)
    return(list(M=M_eq, P=P_eq, Y=Y_eq))
})}
