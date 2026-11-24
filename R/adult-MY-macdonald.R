# X
# specialized methods for the adult mosquito macdonald model
# TODO: Revisit K_matrix handling: macdonald should not implement it as a port.
# Address this through check_MY in a future change; defer implementation for now.
# TODO: Reconcile the Dynamics anchor's delayed product (f*q*kappa) with
# dMYdt.macdonald, which delays only kappa and uses current f and q.

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
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_mosy_emigration]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss fraction --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K` or `K_matrix`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{oviposition rate: the number of egg batches laid in each patch, per mosquito, per day --- use [setup_nu] or [setup_egg_batch_rates]}
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
#' lost. The combined rate of mortality and loss during emigration is \deqn{g+\mu\sigma.}
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
#' computes HBR from `fqM` in the same way.
#'
#' @note The resulting `EIR` and `HBR`
#' orbits are attached to the XH output during parsing.
#'
#' @section Egg Laying:
#' The oviposition rate \eqn{\nu} and the number of eggs per batch
#' \eqn{\Gamma} determine the egg production rate per patch:
#' \deqn{G=\nu\Gamma M.}
#' [F_eggs.macdonald] computes \eqn{G}, which is passed to [EggLaying] for
#' distribution among aquatic habitats.
#'
#' @section Note:
#' Scalar parameters have no ports for exogenous forcing or vector control.
#' Use the `GeM` module for those capabilities. The standard dispersal-matrix
#' port follows the shared bionomics and effect-size convention.
#'
#' @seealso [make_MY_obj_macdonald], [setup_MY_obj.macdonald], [dMYdt.macdonald]
#'
#' @references
#' + Macdonald G (1952) The analysis of the sporozoite rate. Tropical Diseases Bulletin 49:569-586.
#' + Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5 in *The Population Dynamics of Infectious Diseases: Theory and Applications,* Springer, Boston, MA.
#' @name macdonald
NULL

#' @title Compute the derivatives for `macdonald` (**MY**)
#' @description Compute the derivatives for the `macdonald` module:
#' \deqn{\begin{array}{rl}
#' dM/dt &= \Lambda-\Omega\cdot M \\
#' dP/dt &= f(M-P)-\Omega\cdot P \\
#' dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y \\
#' dZ/dt &= \Upsilon\cdot\left(fq\kappa_\tau(M_\tau-Y_\tau)\right)-\Omega\cdot Z \\
#' dA_\kappa/dt &= \kappa
#' \end{array}}
#' Here \eqn{\tau} is `eip` and \eqn{\Upsilon} is `Upsilon`.
#' For \eqn{t>\tau}, \eqn{M_\tau} and \eqn{Y_\tau} are the states at
#' \eqn{t-\tau}, and \eqn{\kappa_\tau} is the derivative of the accessory
#' variable \eqn{A_\kappa} at that time.
#' For \eqn{t\leq\tau}, the delayed states use their initial values and
#' \eqn{\kappa_\tau} uses the current \eqn{\kappa}.
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Dynamical Terms
#' @inheritSection macdonald Parameters
#' @inheritSection macdonald Dynamics
#' @inheritParams dMYdt
#' @seealso [macdonald], [F_fqZ.macdonald], [F_eggs.macdonald]
#' @note Derivatives are returned in patch-ordered blocks for `M`, `P`, `Y`,
#' and `Z`, followed by `kappa`, the derivative of the accessory state.
#' @return a [numeric] vector of derivatives in the module's state-variable order
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

#' @title Get the state variables for `macdonald` (**MY**)
#' @description Extract the state variables for the `macdonald` module
#' from `y` and return them by name in a list for use during model solving.
#' @inheritSection macdonald State Variables
#' @inheritParams get_MY_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @seealso [macdonald]
#' @keywords internal
#' @export
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
get_MY_vars.macdonald <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         Y = y[Y_ix],
         Z = y[Z_ix]
)))}



#' @title Mosquito bionomics for `macdonald` (**MY**)
#' @description Evaluate the dispersal-matrix port into `K_matrix_t` and
#' reset `es_K_matrix` to 1. Scalar parameters have no ports and remain unchanged.
#' @inheritSection macdonald Parameters
#' @inheritParams MBionomics
#' @seealso [macdonald], [MEffectSizes.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.macdonald <- function(t, y, xds_obj, s) {
  xds_obj$MY_obj[[s]]$K_matrix_t <- F_K_matrix(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$es_K_matrix <- matrix(1, xds_obj$nPatches, xds_obj$nPatches)
  return(xds_obj)
}


#' @title Apply effect sizes for `macdonald` (**MY**)
#' @description Set `K_matrix = K_matrix_t * es_K_matrix` and recompute
#' demographic and survival matrices. Scalar parameters remain unchanged.
#' @inheritSection macdonald Parameters
#' @inheritParams MEffectSizes
#' @seealso [macdonald], [MBionomics.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.macdonald <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], {
    xds_obj$MY_obj[[s]]$K_matrix <- K_matrix_t * es_K_matrix
    xds_obj <- change_Omega(xds_obj, s)
    return(xds_obj)
  })
}




#' @title Compute infective mosquito biting density for `macdonald` (**MY**)
#' @description Return the daily infective mosquito biting density on humans,
#' `fqZ`, for each patch in the `macdonald` module.
#' Compute `fqZ = f*q*Z`.
#' @inheritSection macdonald Infective Biting
#' @inheritParams F_fqZ
#' @seealso [macdonald], [dMYdt.macdonald], [F_fqM.macdonald]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.macdonald <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$Z_ix]
}

#' @title Compute mosquito biting density for `macdonald` (**MY**)
#' @description Return the daily mosquito biting density on humans,
#' `fqM`, for each patch in the `macdonald` module.
#' Compute `fqM = f*q*M`.
#' @inheritSection macdonald Infective Biting
#' @inheritParams F_fqM
#' @seealso [macdonald], [F_hbr], [F_fqZ.macdonald]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.macdonald <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$M_ix]
}

#' @title Compute egg production for `macdonald` (**MY**)
#' @description Return the number of eggs laid by the adult mosquito population, per patch
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

#' @title Get the feeding rate for `macdonald` (**MY**)
#' @description Return the stored realized feeding rate per patch.
#' @inheritSection macdonald Parameters
#' @inheritParams get_f
#' @seealso [macdonald]
#' @details Read `f` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_f.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human blood fraction for `macdonald` (**MY**)
#' @description Return the stored realized human blood fraction per patch.
#' @inheritSection macdonald Parameters
#' @inheritParams get_q
#' @seealso [macdonald]
#' @details Read `q` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_q.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the adult mosquito mortality rate for `macdonald` (**MY**)
#' @description Return the stored realized adult mosquito mortality rate per patch.
#' @inheritSection macdonald Parameters
#' @inheritParams get_g
#' @seealso [macdonald]
#' @details Read `g` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_g.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rate for `macdonald` (**MY**)
#' @description Return the stored realized patch emigration rate per patch.
#' @inheritSection macdonald Parameters
#' @inheritParams get_sigma
#' @seealso [macdonald]
#' @details Read `sigma` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_sigma.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}


#' @title Make a module object for `macdonald` (**MY**)
#' @description Create a `macdonald` module object and initialize its parameters.
#' Inspect parameters with [get_MY_pars.macdonald] and change supported values
#' with [change_MY_pars.macdonald]. Values in `options` override parameter arguments.
#' @seealso [get_MY_pars.macdonald], [change_MY_pars.macdonald]
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
#' @param nu the oviposition rate: the number of egg batches laid in each patch,
#' per mosquito, per day
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


#' @title Set up the module object for `macdonald` (**MY**)
#' @description
#'
#' Set up a `macdonald` module object for the **MY** component and the \eqn{s^{th}} vector species.
#'
#' + Set the module name to `macdonald`.
#' + Create the module object with [make_MY_obj_macdonald], passing `nPatches` and `options`.
#' + Set the skill set using [get_skillset_MY].
#' + Initialize the circadian function using [setup_F_circadian.setup].
#' + Initialize the mosquito demographic matrix port.
#' + Initialize the EIP survival and dispersal matrix port.
#' + Initialize the mosquito dispersal matrix using [setup_K_matrix.zero].
#' + Set initial values using [setup_MY_inits], passing `options` to override defaults.
#' + Return the updated **`xds`** model object.
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
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, TRUE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  return(xds_obj)
}



#' @title Get parameters for `macdonald` (**MY**)
#' @seealso [make_MY_obj_macdonald], [change_MY_pars.macdonald]
#' @description Return the parameters for the `macdonald` module as a named list.
#' @inheritSection macdonald Parameters
#' @inheritParams get_MY_pars
#' @return a named [list] of parameter values
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
#' @seealso [make_MY_obj_macdonald], [get_MY_pars.macdonald]
#' @description Change parameter values named in `options` for the `macdonald`
#' module, leaving other values unchanged.
#' Call [MBionomics] at time zero after updating parameters, without calling [MEffectSizes].
#' This method changes static parameter values.
#' Recompute the demographic matrix and survival through the EIP.
#' To change `K_matrix`, use [setup_K_matrix]; this method does not change
#' the dispersal matrix.
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
    xds_obj <- change_Omega(xds_obj, s)
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- MBionomics(0, y, xds_obj, s)
    return(xds_obj)
  }))}


#' @title Make initial values for `macdonald` (**MY**)
#' @description Create a named list of initial values for the `macdonald` module.
#' Values in `options` override the defaults.
#' @details Initial mosquito densities must be numeric scalars or have length
#' `nPatches`. Scalars are expanded to that length.
#' Initialize `dk_ix` to zero in each patch. This accessory value is constructed
#' internally rather than independently set through `options`.
#' @inheritSection macdonald State Variables
#' @param nPatches the number of patches
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a named [list] of initial values
#' @seealso [macdonald], [setup_MY_inits.macdonald]
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.macdonald]
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


#' @title Set up initial values for `macdonald` (**MY**)
#' @description Set up initial values for the `macdonald` module using
#' [make_MY_inits_macdonald]. Values in `options` override the defaults.
#' @inheritSection macdonald State Variables
#' @inheritParams setup_MY_inits
#' @seealso [macdonald], [make_MY_inits_macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.macdonald]
#' @note Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.
#' @seealso [get_MY_inits]
setup_MY_inits.macdonald = function(xds_obj, options=list(), s=1){with(xds_obj$MY_obj[[s]], {
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_macdonald(nPatches, options)
  return(xds_obj)
})}

#' @title Change initial values for `macdonald` (**MY**)
#' @description Change initial values named in `options` for the `macdonald`
#' module, leaving other values unchanged.
#'
#' @inheritSection macdonald State Variables
#' @inheritParams change_MY_inits
#'
#' @seealso [macdonald], [get_MY_inits], [make_MY_inits_macdonald], [setup_MY_inits.macdonald]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Stored initial values for this module are at
#' `xds_obj$MY_obj[[s]]$inits`.
change_MY_inits.macdonald = function(xds_obj, options=list(), s=1){
  inits = with(get_MY_inits(xds_obj, s),
            with(options,{
              list(M=M, P=P, Y=Y, Z=Z, dk_ix=dk_ix)
           }))
  xds_obj$MY_obj[[s]]$inits=inits
  return(xds_obj)
}

#' @title Get the initial adult mosquito density for `macdonald` (**MY**)
#' @description Return the initial adult mosquito density per patch.
#' @inheritSection macdonald State Variables
#' @inheritParams get_M
#' @seealso [macdonald], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_M.macdonald = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density for `macdonald` (**MY**)
#' @description Return the initial infective mosquito density per patch.
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Infective Biting
#' @inheritParams get_Z
#' @seealso [macdonald], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_Z.macdonald = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}

#' @title Set up variable indices for `macdonald` (**MY**)
#' @description Assign consecutive variable indices for the `macdonald` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `M_ix`, `P_ix`, `Y_ix`, `Z_ix`, `kappa_ix`.
#' Each block has length `nPatches`.
#' @inheritParams setup_MY_ix
#' @inheritSection macdonald State Variables
#' @return an **`xds`** model object
#' @seealso [macdonald]
#' @importFrom utils tail
#' @keywords internal
#' @export
#' @note Indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
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

#' @title Parse outputs for `macdonald` (**MY**)
#' @description Extract the model state outputs for the `macdonald` module
#' from `outputs` and return them by name in a list.
#' Also compute `y = Y/M`, `z = Z/M`, and `parous = P/M`.
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
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

#' @title Check `macdonald` (**MY**)
#'
#' @description This method currently performs no additional checks.
#' @inheritParams check_MY
#'
#' @return an **`xds`** model object
#' @seealso [macdonald]
#' @keywords internal
#' @export
check_MY.macdonald = function(xds_obj, s){
  return(xds_obj)
}


#' @title Get the skill set for `macdonald` (**MY**)
#'
#' @description Return the capabilities and compatibilities of the `macdonald` module as a list.
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
    f_is_port = FALSE,
    q_is_port = FALSE,
    g_is_port = FALSE,
    sigma_is_port = FALSE,
    sigma_b_is_port = FALSE,
    sigma_q_is_port = FALSE,
    mu_is_port = FALSE,
    nu_is_port = FALSE,
    eip_is_port = FALSE,
    K_matrix_is_port = TRUE,
    dts = FALSE,
    xde = TRUE,
    delay = TRUE
  )
  return(skills)
}

#' @title Compute steady states for `macdonald` (**MY**)
#' @description Compute steady-state mosquito densities for the `macdonald` module
#' given adult mosquito emergence `Lambda` and net infectiousness `kappa`
#' for the differential-equation system.
#' @inheritSection macdonald State Variables
#' @inheritSection macdonald Dynamics
#' @inheritParams steady_state_MY
#' @return a named [list] with elements `M`, `P`, `Y`, and `Z`, each
#' containing one value per patch
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
  Y_eq <- as.vector(ginv(diag(f*q*kappa, nPatches) + Omega) %*% diag(f*q*kappa, nPatches) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa, nPatches) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}
