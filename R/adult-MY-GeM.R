# X
# TODO: Reconcile dMYdt.GeM's scalar if (t <= eip) condition with the
# patch-length EIP values supplied by the port in multi-patch models.
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
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_mosy_emigration]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss fraction --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K` or `K_matrix`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{oviposition rate: the number of egg batches laid in each patch, per mosquito, per day --- use [setup_nu] or [setup_egg_batch_rates]}
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
#' The oviposition rate \eqn{\nu} and the number of eggs per batch,
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


#' @title Compute the derivatives for `GeM` (**MY**)
#'
#' @description Compute the derivatives for the `GeM` module:
#' \deqn{\begin{array}{rl}
#' dM/dt &= \Lambda-\Omega\cdot M \\
#' dP/dt &= f(M-P)-\Omega\cdot P \\
#' dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y \\
#' dZ/dt &= \Upsilon\cdot\left((fq\kappa)_\tau(M_\tau-Y_\tau)\right)-\Omega\cdot Z \\
#' d\Upsilon/dt &= \left((1-\dot{\ell})\Omega_\tau-\Omega\right)\cdot\Upsilon \\
#' dA_{fq\kappa}/dt &= fq\kappa \\
#' dA_g/dt &= g \\
#' dA_\sigma/dt &= \sigma
#' \end{array}}
#' 
#' Survival and dispersal through the EIP \eqn{(\Upsilon)} is computed as 
#' an accessory variable:
#' \deqn{
#' d\Upsilon/dt = \left((1-\dot{\ell})\Omega_\tau-\Omega\right)\cdot\Upsilon
#' }
#' 
#' The model also adds variables to internalize lags for \eqn{\sigma}, \eqn{g}, and 
#' \eqn{fq\kappa}. 
#' 
#' @inheritSection GeM State Variables
#' @inheritSection GeM Dynamical Terms
#' @inheritSection GeM Parameters
#' @inheritSection GeM EIP
#' @inheritSection GeM Accessory Variable
#' @inheritSection GeM Dynamics
#' @inheritParams dMYdt
#' @seealso [GeM], [F_fqZ.GeM], [F_eggs.GeM]
#' @note Derivatives are returned in patch-ordered blocks for `M`, `P`, `Y`,
#' and `Z`, followed by the column-wise flattened derivative of `U` and
#' patch-ordered accessory derivatives `f*q*kappa`, `g`, and `sigma`.
#' @return a [numeric] vector of derivatives in the module's state-variable order
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
        M_eip <- inits$M
        Y_eip <- inits$Y
        fqkappa_eip <- kappa*f*q
        g_eip <- g
        sigma_eip <- sigma
      } else {
        M_eip <- lagvalue(t=t-eip, nr = ix$M_ix)
        Y_eip <- lagvalue(t=t-eip, nr = ix$Y_ix)
        fqkappa_eip <- lagderiv(t=t-eip, nr = ix$fqkappa_ix)
        g_eip <- lagderiv(t=t-eip, nr = ix$g_ix)
        sigma_eip <- lagderiv(t=t-eip, nr = ix$sigma_ix)
      }

      Omega_eip <- F_Omega_xde(g_eip, sigma_eip, mu, K_matrix)

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      dYdt <- f*q*kappa*(M - Y) - (Omega %*% Y)
      dZdt <- U %*% (fqkappa_eip * (M_eip - Y_eip)) - (Omega %*% Z)
      dUdt <- ((1-deip_dt)*Omega_eip - Omega) %*% U

      return(c(dMdt, dPdt, dYdt, dZdt, as.vector(dUdt), f*q*kappa, g, sigma))
    })
  })
}

#' @title Get the state variables for `GeM` (**MY**)
#' @description Extract the state variables for the `GeM` module
#' from `y` and return them by name in a list for use during model solving.
#' Return `U` as an `nPatches` by `nPatches` matrix.
#' @inheritSection GeM State Variables
#' @inheritSection GeM Accessory Variable
#' @inheritParams get_MY_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @export
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
get_MY_vars.GeM <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix, return(list(
    M = y[M_ix],
    P = y[P_ix],
    Y = y[Y_ix],
    Z = y[Z_ix],
    U = matrix(y[U_ix], xds_obj$nPatches, xds_obj$nPatches)
)))}


#' @title Mosquito bionomics for `GeM` (**MY**)
#' @description Implements [MBionomics] for the `GeM` module.
#' @inheritSection GeM Parameters
#' @inheritSection GeM Survival and Dispersal
#' @inheritSection GeM EIP
#' @inheritParams MBionomics
#' @seealso [GeM], [MEffectSizes.GeM]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.GeM <- function(t, y, xds_obj, s) {
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


#' @title Apply effect sizes for `GeM` (**MY**)
#' @description Implements [MEffectSizes] for the `GeM` module.
#' @inheritSection GeM Parameters
#' @inheritSection GeM Survival and Dispersal
#' @inheritParams MEffectSizes
#' @seealso [GeM], [MBionomics.GeM]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.GeM <- function(t, y, xds_obj, s) {
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


#' @title Compute infective mosquito biting density for `GeM` (**MY**)
#' @description Return the daily infective mosquito biting density on humans,
#' `fqZ`, for each patch in the `GeM` module.
#' Compute `fqZ = f*q*Z`.
#' @inheritSection GeM Infective Biting
#' @inheritParams F_fqZ
#' @seealso [GeM], [dMYdt.GeM], [F_fqM.GeM]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.GeM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$Z_ix]
}

#' @title Compute mosquito biting density for `GeM` (**MY**)
#' @description Return the daily mosquito biting density on humans,
#' `fqM`, for each patch in the `GeM` module.
#' Compute `fqM = f*q*M`.
#' @inheritSection GeM Infective Biting
#' @inheritParams F_fqM
#' @seealso [GeM], [F_hbr], [F_fqZ.GeM]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.GeM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$M_ix]
}

#' @title Compute egg production for `GeM` (**MY**)
#' @description Return the number of eggs laid by the adult mosquito population, per patch
#' @inheritSection GeM Egg Laying
#' @inheritParams F_eggs
#' @seealso [GeM], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.GeM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggs_per_batch)
  })
}



#' @title Get the feeding rate for `GeM` (**MY**)
#' @description Return the stored realized feeding rate per patch.
#' @inheritSection GeM Parameters
#' @inheritParams get_f
#' @seealso [GeM]
#' @details Read `f` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_f.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human blood fraction for `GeM` (**MY**)
#' @description Return the stored realized human blood fraction per patch.
#' @inheritSection GeM Parameters
#' @inheritParams get_q
#' @seealso [GeM]
#' @details Read `q` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_q.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the adult mosquito mortality rate for `GeM` (**MY**)
#' @description Return the stored realized adult mosquito mortality rate per patch.
#' @inheritSection GeM Parameters
#' @inheritParams get_g
#' @seealso [GeM]
#' @details Read `g` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_g.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rate for `GeM` (**MY**)
#' @description Return the stored realized patch emigration rate per patch.
#' @inheritSection GeM Parameters
#' @inheritParams get_sigma
#' @seealso [GeM]
#' @details Read `sigma` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_sigma.GeM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}


#' @title Make a module object for `GeM` (**MY**)
#' @description Create a `GeM` module object and initialize its parameters.
#' Inspect parameters with [get_MY_pars.GeM] and change supported values
#' with [change_MY_pars.GeM]. Values in `options` override parameter arguments.
#' @seealso [get_MY_pars.GeM], [change_MY_pars.GeM]
#' @param nPatches is the number of patches, an integer
#' @param options a [list] of values that overrides the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu the oviposition rate: the number of egg batches laid in each patch,
#' per mosquito, per day
#' @param eggs_per_batch number of eggs per batch
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

    MY_obj <- setup_eip_obj(checkIt(eip, nPatches), MY_obj)
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



#' @title Set up the module object for `GeM` (**MY**)
#' @description
#'
#' Set up a `GeM` module object for the **MY** component and the \eqn{s^{th}} vector species.
#'
#' + Set the module name to `GeM`.
#' + Create the module object with [make_MY_obj_GeM], passing `nPatches` and `options`.
#' + Set the skill set using [get_skillset_MY].
#' + Initialize the circadian function using [setup_F_circadian.setup].
#' + Initialize the mosquito demographic matrix port.
#' + Initialize the EIP survival and dispersal matrix port.
#' + Initialize the mosquito dispersal matrix using [setup_K_matrix.zero].
#' + Set initial values using [setup_MY_inits], passing `options` to override defaults.
#' + Return the updated **`xds`** model object.
#'
#' @inheritSection GeM State Variables
#' @inheritSection GeM Parameters
#' @inheritSection GeM EIP
#' @inheritSection GeM Accessory Variable
#' @seealso [GeM], [make_MY_obj_GeM], [setup_MY_inits.GeM]
#'
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#' Its class is set to `GeM`.
#'
#' @inheritParams setup_MY_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_obj.GeM = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MYname = "GeM"
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

#' @title Get parameters for `GeM` (**MY**)
#' @seealso [make_MY_obj_GeM], [change_MY_pars.GeM]
#' @description Return the parameters for the `GeM` module as a named list.
#' @inheritSection GeM Parameters
#' @inheritParams get_MY_pars
#' @return a named [list] of parameter values
#' @seealso [GeM], [change_MY_pars.GeM]
#' @keywords internal
#' @export
get_MY_pars.GeM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggs_per_batch=eggs_per_batch, K_matrix=K_matrix
  ))
}

#' @title Change parameters for `GeM` (**MY**)
#' @seealso [make_MY_obj_GeM], [get_MY_pars.GeM]
#' @description Change parameter values named in `options` for the `GeM`
#' module, leaving other values unchanged.
#' Refresh baseline bionomics at time zero using [MBionomics], without calling [MEffectSizes].
#' To change `K_matrix`, use [setup_K_matrix]; this method does not change
#' the dispersal matrix.
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
    xds_obj$MY_obj[[s]]$eggs_per_batch = eggs_per_batch
    
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- MBionomics(0, y, xds_obj, s)
    return(xds_obj)
  }))}


#' @title Make initial values for `GeM` (**MY**)
#' @description Create a named list of initial values for the `GeM` module.
#' Values in `options` override the defaults.
#' @details Initial mosquito densities must be numeric scalars or have length
#' `nPatches`. Scalars are expanded to that length.
#' Initialize `U` from `Upsilon` and `d1`, `d2`, and `d3` to zero in each patch.
#' These accessory values are constructed internally rather than independently
#' set through `options`.
#' @param nPatches the number of patches
#' @param Upsilon an `nPatches` by `nPatches` matrix describing survival and
#' dispersal through the initial EIP
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @inheritSection GeM State Variables
#' @inheritSection GeM Accessory Variable
#' @seealso [GeM], [setup_MY_inits.GeM]
#' @return a named [list] of initial values
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.GeM]
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

#' @title Set up initial values for `GeM` (**MY**)
#' @description Set up initial values for the `GeM` module using
#' [make_MY_inits_GeM]. Values in `options` override the defaults.
#' The EIP survival and dispersal matrix is computed and passed to the constructor.
#' @inheritSection GeM State Variables
#' @inheritSection GeM Accessory Variable
#' @inheritParams setup_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.GeM]
#' @note Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.
#' @seealso [get_MY_inits]
setup_MY_inits.GeM = function(xds_obj, options=list(), s=1){with(xds_obj$MY_obj[[s]], {
  Omega = F_Omega_xde(g, sigma, mu, K_matrix)
  Upsilon = expm(-Omega*eip)
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_GeM(nPatches, Upsilon, options)
  return(xds_obj)
})}

#' @title Change initial values for `GeM` (**MY**)
#' @description Change initial values named in `options` for the `GeM`
#' module, leaving other values unchanged.
#' @inheritSection GeM State Variables
#' @inheritParams change_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [make_MY_inits_GeM], [setup_MY_inits.GeM]
#' @note Stored initial values for this module are at
#' `xds_obj$MY_obj[[s]]$inits`.
change_MY_inits.GeM <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]]$inits, with(options,{
    xds_obj$MY_obj[[s]]$inits$M = M
    xds_obj$MY_obj[[s]]$inits$Y = Y
    xds_obj$MY_obj[[s]]$inits$P = P
    xds_obj$MY_obj[[s]]$inits$Z = Z
    return(xds_obj)
  }))}

#' @title Get the initial adult mosquito density for `GeM` (**MY**)
#' @description Return the initial adult mosquito density per patch.
#' @inheritSection GeM State Variables
#' @inheritParams get_M
#' @seealso [GeM], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_M.GeM = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density for `GeM` (**MY**)
#' @description Return the initial infective mosquito density per patch.
#' @inheritSection GeM State Variables
#' @inheritParams get_Z
#' @seealso [GeM], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_Z.GeM = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}


#' @title Set up variable indices for `GeM` (**MY**)
#' @description Assign consecutive variable indices for the `GeM` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `M_ix`, `P_ix`, `Y_ix`, `Z_ix`, `U_ix`, `fqkappa_ix`, `g_ix`, `sigma_ix`.
#' Each block has length `nPatches`, except `U_ix`, which has length `nPatches^2`.
#' @inheritSection GeM State Variables
#' @inheritParams setup_MY_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @export
#' @note Indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
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

#' @title Parse outputs for `GeM` (**MY**)
#' @description Extract the model state outputs for the `GeM` module
#' from `outputs` and return them by name in a list.
#' Return `M`, `P`, `Y`, and `Z`, and compute `y = Y/M`, `z = Z/M`,
#' `parous = P/M`, `fqZ = f*q*Z`, and `fqM = f*q*M`.
#' The `U` and rate-history states are not included in the parsed outputs.
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
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
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parous=parous, fqZ=f*q*Z, fqM=f*q*M))
})}

#' @title Check `GeM` (**MY**)
#'
#' @inheritParams check_MY
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.GeM = function(xds_obj, s){

  return(xds_obj)
}


#' @title Get the skill set for `GeM` (**MY**)
#'
#' @description Return the capabilities and compatibilities of the `GeM` module as a list.
#'
#' @inheritParams get_skillset_MY
#'
#' @return the **MY** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_MY.GeM = function(xds_obj, s){
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
    dts = FALSE,
    xde = TRUE,
    delay = TRUE
  )
  return(skills)
}




#' @title Compute steady states for `GeM` (**MY**)
#' @description Compute steady-state mosquito densities for the `GeM` module
#' given adult mosquito emergence `Lambda` and net infectiousness `kappa`
#' for the differential-equation system.
#' @inheritSection GeM State Variables
#' @inheritSection GeM Parameters
#' @inheritSection GeM Dynamics
#' @inheritParams steady_state_MY
#' @seealso [GeM]
#' @return a named [list]
#' @importFrom MASS ginv
#' @keywords internal
#' @export
steady_state_MY.GeM = function(Lambda, kappa, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa, nPatches) + Omega) %*% diag(f*q*kappa, nPatches) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa, nPatches) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}
