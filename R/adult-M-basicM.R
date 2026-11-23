# Y
# specialized methods for a basic adult mosquito model

#' @title The `basicM` module for the **MY** component
#' @description
#' Implements a basic model of adult mosquito ecology without explicit
#' infection dynamics. It can be used when mosquito population dynamics are
#' needed without modeling mosquito infection.
#'
#' @section State Variables:
#' \describe{
#'   \item{`M`}{density of adult mosquitoes}
#'   \item{`P`}{density of parous adult mosquitoes: those that have blood-fed at least once}
#' }
#'
#' @section Dynamical Terms:
#' \describe{
#'   \item{`Lambda` or \eqn{\Lambda}}{adult mosquito emergence rate, computed by [Emergence]}
#' }
#'
#' @section Parameters:
#' \describe{
#'   \item{`f`}{blood feeding rate --- use [setup_f] or [setup_blood_feeding_rates]}
#'   \item{`q`}{human fraction --- use [setup_q] or [setup_human_fractions]}
#'   \item{`g`}{mosquito mortality rate --- use [setup_g] or [setup_mosquito_mortality]}
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_mosy_emigration]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss fraction --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K` or `K_matrix`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{oviposition rate: the number of egg batches laid in each patch, per mosquito, per day --- use [setup_nu] or [setup_egg_batch_rates]}
#'   \item{`eggsPerBatch` or \eqn{\Gamma}}{number of eggs per batch}
#' }
#'
#' After basic setup, [change_MY_pars.basicM] can be used to change static
#' parameter values. The `basicM` module evaluates bionomic parameter functions
#' through [MBionomics].
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
#'
#' @section Blood Feeding:
#' The blood feeding rate \eqn{f} and human fraction \eqn{q} are parameters of
#' the module. At baseline they are static. They should satisfy natural
#' constraints: \eqn{f=0} if no blood hosts are available, \eqn{q=0} if no
#' humans are available, and \eqn{q=1} if humans are the only available hosts.
#' The module does not model infection, but `fqM` is used to compute the
#' human biting rate.
#'
#' @section ODE:
#' Adult mosquitoes emerge from aquatic habitats at rate \eqn{\Lambda} and
#' disperse and die according to \eqn{\Omega}. The ODE model is
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda-\Omega\cdot M \\\\
#' dP/dt &= f(M-P)-\Omega\cdot P
#' \end{array}}
#' Here, \eqn{P} is the density of mosquitoes that have blood-fed at least
#' once.
#'
#' @section DTS:
#' Adult mosquitoes emerge from aquatic habitats at rate \eqn{\Lambda} and
#' disperse and die according to \eqn{\Omega}. The one-day update in
#' [Update_MYt.basicM] is
#' \deqn{
#' \begin{array}{rl}
#' M_t &= \Lambda+e^{-\Omega}\cdot M_{t-1} \\\\
#' P_t &= f(M_{t-1}-P_{t-1})+e^{-\Omega}\cdot P_{t-1}
#' \end{array}}
#' Here, \eqn{P} is the density of mosquitoes that have blood-fed at least
#' once.
#'
#' @note Parity, \eqn{P/M}, is computed during parsing and stored as `parous`.
#'
#' @section Infective Biting:
#' The `basicM` module does not represent mosquito infection, so its infective
#' biting density \eqn{fqZ} is zero. Its human biting density, \eqn{fqM}, is
#' the number of bites on humans in each patch per day.
#' [F_fqM.basicM] computes this patch-level quantity. [F_hbr] combines `fqM`
#' with the biting distribution matrix and resident-bite fraction to compute
#' the stratum-level human biting rate (`HBR`). The `EIR` is zero for this
#' module because there are no infective bites.
#'
#' @section Egg Laying:
#' The oviposition rate \eqn{\nu} and the number of eggs per batch
#' \eqn{\Gamma} determine the egg production rate per patch:
#' \deqn{G=\nu\Gamma M.}
#' [F_eggs.basicM] computes \eqn{G}, which is passed to [EggLaying] for
#' distribution among aquatic habitats.
#'
#' @seealso [make_M_obj_basicM], [setup_MY_obj.basicM], [dMYdt.basicM]
#'
#' @name basicM
#' @rdname basicM
NULL


#' @title Compute the derivatives for `basicM` (**MY**)
#' @description Compute the derivatives for the `basicM` module:
#' \deqn{\begin{array}{rl}
#' dM/dt &= \Lambda - \Omega \cdot M \\
#' dP/dt &= f(M-P) - \Omega \cdot P
#' \end{array}}
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamical Terms
#' @inheritSection basicM Parameters
#' @inheritSection basicM ODE
#' @inheritParams dMYdt
#' @seealso [basicM], [F_fqZ.basicM], [F_eggs.basicM]
#' @note Derivatives are returned in blocks for `M`, then `P`, each in patch order.
#' @return a [numeric] vector of derivatives in the module's state-variable order
#' @keywords internal
#' @export
dMYdt.basicM <- function(t, y, xds_obj, s){

  Lambda = xds_obj$terms$Lambda[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)

      return(c(dMdt, dPdt))
    })
  })
}

# specialized methods for the adult mosquito basicM model

#' @title Update states for `basicM` (**MY**)
#' @description Update the state variables for the `basicM` module:
#' \deqn{\begin{array}{rl}
#' M_t &= \Lambda+e^{-\Omega}\cdot M_{t-1} \\\\
#' P_t &= f(M_{t-1}-P_{t-1})+e^{-\Omega}\cdot P_{t-1}
#' \end{array}}
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamical Terms
#' @inheritSection basicM Parameters
#' @inheritSection basicM DTS
#' @inheritParams Update_MYt
#' @seealso [basicM], [dMYdt.basicM]
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_MYt.basicM <- function(t, y, xds_obj, s) {

  Lambda = xds_obj$terms$Lambda[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      Mt <- Lambda + expm::expm(-Omega) %*% M
      Pt <- f*(M-P) + expm::expm(-Omega) %*% P

      return(c(Mt, Pt))
    })
  })
}


#' @title Get the state variables for `basicM` (**MY**)
#' @description Extract the state variables for the `basicM` module
#' from `y` and return them by name in a list for use during model solving.
#' @inheritSection basicM State Variables
#' @inheritParams get_MY_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @export
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
get_MY_vars.basicM <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix]
       )))
}


#' @title Mosquito bionomics for `basicM` (**MY**)
#' @description Implements [MBionomics] for the `basicM` module.
#' @inheritSection basicM Parameters
#' @inheritSection basicM Survival and Dispersal
#' @inheritParams MBionomics
#' @seealso [basicM], [MEffectSizes.basicM]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.basicM <- function(t, y, xds_obj, s) {
  xds_obj$MY_obj[[s]]$f_t <- F_f(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t <- F_q(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t <- F_g(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t <- F_sigma(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu_t <- F_mu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu_t <- F_nu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$K_matrix_t <- F_K_matrix(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$es_f <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_mu <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_nu <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_K_matrix <- matrix(1, xds_obj$nPatches, xds_obj$nPatches)
  return(xds_obj)
}


#' @title Apply effect sizes for `basicM` (**MY**)
#' @description Implements [MEffectSizes] for the `basicM` module.
#' @inheritSection basicM Parameters
#' @inheritParams MEffectSizes
#' @seealso [basicM], [MBionomics.basicM]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.basicM <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], {
    xds_obj$MY_obj[[s]]$f <- f_t * es_f
    xds_obj$MY_obj[[s]]$q <- q_t * es_q
    xds_obj$MY_obj[[s]]$g <- g_t * es_g
    xds_obj$MY_obj[[s]]$sigma <- sigma_t * es_sigma
    xds_obj$MY_obj[[s]]$mu <- mu_t * es_mu
    xds_obj$MY_obj[[s]]$nu <- nu_t * es_nu
    xds_obj$MY_obj[[s]]$K_matrix <- K_matrix_t * es_K_matrix
    xds_obj <- change_Omega(xds_obj, s)
    return(xds_obj)
  })
}

#' @title Compute infective mosquito biting density for `basicM` (**MY**)
#' @description Return the daily infective mosquito biting density on humans,
#' `fqZ`, for each patch in the `basicM` module.
#' Return zero because this module does not track mosquito infection.
#' @inheritSection basicM Infective Biting
#' @inheritParams F_fqZ
#' @seealso [basicM], [F_fqM.basicM]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.basicM <- function(t, y, xds_obj, s) {
  0*y[xds_obj$MY_obj[[s]]$ix$M_ix]
}

#' @title Compute mosquito biting density for `basicM` (**MY**)
#' @description Return the daily mosquito biting density on humans,
#' `fqM`, for each patch in the `basicM` module.
#' Compute `fqM = f*q*M`.
#' @inheritSection basicM Infective Biting
#' @inheritParams F_fqM
#' @seealso [basicM], [F_hbr], [F_fqZ.basicM]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.basicM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  M = y[xds_obj$MY_obj[[s]]$ix$M_ix]
  return(f*q*M)
}

#' @title Compute egg production for `basicM` (**MY**)
#' @description Return the number of eggs laid by the adult mosquito population, per patch.
#' @inheritSection basicM Egg Laying
#' @inheritParams F_eggs
#' @seealso [basicM], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.basicM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]], {
    return(M*nu*eggsPerBatch)
  })
}


#' @title Get the feeding rate for `basicM` (**MY**)
#' @description Return the stored realized feeding rate per patch.
#' @inheritSection basicM Parameters
#' @inheritParams get_f
#' @seealso [basicM]
#' @details Read `f` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_f.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human blood fraction for `basicM` (**MY**)
#' @description Return the stored realized human blood fraction per patch.
#' @inheritSection basicM Parameters
#' @inheritParams get_q
#' @seealso [basicM]
#' @details Read `q` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_q.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the adult mosquito mortality rate for `basicM` (**MY**)
#' @description Return the stored realized adult mosquito mortality rate per patch.
#' @inheritSection basicM Parameters
#' @inheritParams get_g
#' @seealso [basicM]
#' @details Read `g` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_g.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rate for `basicM` (**MY**)
#' @description Return the stored realized patch emigration rate per patch.
#' @inheritSection basicM Parameters
#' @inheritParams get_sigma
#' @seealso [basicM]
#' @details Read `sigma` without evaluating ports or applying effect sizes.
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_sigma.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}


#' @title Make a module object for `basicM` (**MY**)
#' @description Create a `basicM` module object and initialize its parameters.
#' Inspect parameters with [get_MY_pars.basicM] and change supported values
#' with [change_MY_pars.basicM]. Values in `options` override parameter arguments.
#' @seealso [get_MY_pars.basicM], [change_MY_pars.basicM]
#' @param nPatches is the number of patches, an integer
#' @param options a named list: named values override defaults
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu the oviposition rate: the number of egg batches laid in each patch,
#' per mosquito, per day
#' @param eggsPerBatch number of eggs per batch
#' @return a [list]
#' @seealso [basicM], [setup_MY_obj.basicM]
#' @keywords internal
#' @export
make_M_obj_basicM = function(nPatches, options=list(),
                            g=1/12,  sigma=1/8,  mu=0,
                            f=0.3,  q=0.95,
                            nu=1,  eggsPerBatch=60){

  with(options,{
    MY_obj <- list()

    MY_obj$nPatches <- nPatches

    MY_obj <- setup_f_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_q_obj(checkIt(q, nPatches), MY_obj)
    MY_obj <- setup_g_obj(checkIt(g, nPatches), MY_obj)
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj)
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj)
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj)

    MY_obj$eggsPerBatch <- eggsPerBatch

    return(MY_obj)
})}



#' @title Set up the module object for `basicM` (**MY**)
#' @description
#'
#' Set up a `basicM` module object for the **MY** component and the \eqn{s^{th}} vector species.
#'
#' + Set the module name to `basicM`.
#' + Create the module object with [make_M_obj_basicM], passing `nPatches` and `options`.
#' + Set the skill set using [get_skillset_MY].
#' + Initialize the circadian function using [setup_F_circadian.setup].
#' + Initialize the mosquito demographic matrix port.
#' + Initialize the mosquito dispersal matrix using [setup_K_matrix.zero].
#' + Set initial values using [setup_MY_inits], passing `options` to override defaults.
#' + Return the updated **`xds`** model object.
#'
#' @inheritSection basicM State Variables
#' @inheritSection basicM Parameters
#' @seealso [basicM], [make_M_obj_basicM], [setup_MY_inits.basicM]
#'
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`.
#' Its classes are `basicM` and `basicM_` followed by `xds_obj$xds`.
#'
#' @inheritParams setup_MY_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_obj.basicM = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MYname = "basicM"
  MY_obj <- make_M_obj_basicM(xds_obj$nPatches, options)
  class(MY_obj) <- c("basicM", paste("basicM_", xds_obj$xds, sep=""))
  xds_obj$MY_obj[[s]]= MY_obj
  xds_obj$MY_obj[[s]]$skill_set <- get_skillset_MY(xds_obj, s)
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, FALSE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  return(xds_obj)
}

#' @title Get parameters for `basicM` (**MY**)
#' @seealso [make_M_obj_basicM], [change_MY_pars.basicM]
#' @description Return the parameters for the `basicM` module as a named list.
#' @inheritSection basicM Parameters
#' @inheritParams get_MY_pars
#' @return a named [list] of parameter values
#' @seealso [basicM], [change_MY_pars.basicM]
#' @keywords internal
#' @export
get_MY_pars.basicM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, K_matrix=K_matrix
  ))
}

#' @title Change parameters for `basicM` (**MY**)
#' @seealso [make_M_obj_basicM], [get_MY_pars.basicM]
#' @description Change parameter values named in `options` for the `basicM`
#' module, leaving other values unchanged.
#' Refresh baseline bionomics at time zero using [MBionomics], without calling [MEffectSizes].
#' To change `K_matrix`, use [setup_K_matrix]; this method does not change
#' the dispersal matrix.
#' @inheritSection basicM Parameters
#' @inheritParams change_MY_pars
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_pars.basicM <- function(xds_obj, options=list(), s=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_obj$f = f
    xds_obj$MY_obj[[s]]$q_obj$q = q
    xds_obj$MY_obj[[s]]$g_obj$g = g
    xds_obj$MY_obj[[s]]$sigma_obj$sigma = sigma
    xds_obj$MY_obj[[s]]$mu_obj$mu = mu
    xds_obj$MY_obj[[s]]$nu_obj$nu = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- MBionomics(0, y, xds_obj, s)
    
    return(xds_obj)
  }))}

#' @title Make initial values for `basicM` (**MY**)
#' @description Create a named list of initial values for the `basicM` module.
#' Values in `options` override the defaults.
#' @details Initial values must be numeric scalars or have length `nPatches`.
#' Scalars are expanded to that length.
#' @param nPatches the number of patches
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @return a named [list] of initial values
#' @keywords internal
#' @export
#' @seealso [setup_MY_inits.basicM], [change_MY_inits.basicM]
make_MY_inits_basicM = function(nPatches, options, M=5, P=1){
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    return(list(M=M, P=P))
  })}

#' @title Set up initial values for `basicM` (**MY**)
#' @description Set up initial values for the `basicM` module using
#' [make_MY_inits_basicM]. Values in `options` override the defaults.
#' @inheritSection basicM State Variables
#' @inheritParams setup_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [change_MY_inits.basicM]
#' @note Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.
#' @seealso [get_MY_inits]
setup_MY_inits.basicM = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_basicM(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Change initial values for `basicM` (**MY**)
#' @description Change initial values named in `options` for the `basicM`
#' module, leaving other values unchanged.
#' @inheritSection basicM State Variables
#' @inheritParams change_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [make_MY_inits_basicM], [setup_MY_inits.basicM]
#' @note Stored initial values for this module are at
#' `xds_obj$MY_obj[[s]]$inits`.
change_MY_inits.basicM <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]]$inits, with(options,{
    xds_obj$MY_obj[[s]]$inits$M = M
    xds_obj$MY_obj[[s]]$inits$P = P
    return(xds_obj)
  }))}

#' @title Get the initial adult mosquito density for `basicM` (**MY**)
#' @description Return the initial adult mosquito density per patch.
#' @inheritSection basicM State Variables
#' @inheritParams get_M
#' @seealso [basicM], [get_MY_inits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_M.basicM = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Set up variable indices for `basicM` (**MY**)
#' @description Assign consecutive variable indices for the `basicM` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `M_ix`, `P_ix`.
#' Each block has length `nPatches`.
#' @inheritParams setup_MY_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @export
#' @note Indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
setup_MY_ix.basicM <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(P_ix, 1)

  xds_obj$max_ix = max_ix

  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix)

  return(xds_obj)
})}

#' @title Parse outputs for `basicM` (**MY**)
#' @description Extract the model state outputs for the `basicM` module
#' from `outputs` and return them by name in a list.
#' Also compute `parous = P/M`.
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' @inheritSection basicM State Variables
#' @inheritSection basicM ODE
#' @inheritSection basicM DTS
#' @inheritParams parse_MY_orbits
#' @seealso [basicM], [get_MY_vars.basicM]
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits.basicM <- function(outputs, xds_obj, s) {
  with(xds_obj$MY_obj[[s]]$ix,{
    M = outputs[,M_ix]
    P = outputs[,P_ix]
    parous = P/M
    return(list(M=M, P=P, parous=parous))
  })}


#' @title Check `basicM` (**MY**)
#'
#' @inheritParams check_MY
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.basicM = function(xds_obj, s){
  return(xds_obj)
}

#' @title Get the skill set for `basicM` (**MY**)
#'
#' @description Return the capabilities and compatibilities of the `basicM` module as a list.
#'
#' @inheritParams get_skillset_MY
#'
#' @return the **MY** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_MY.basicM = function(xds_obj, s){
  skills = list(
    f_is_port = TRUE,
    q_is_port = TRUE,
    g_is_port = TRUE,
    sigma_is_port = TRUE,
    sigma_b_is_port = FALSE,
    sigma_q_is_port = FALSE,
    mu_is_port = TRUE,
    nu_is_port = TRUE,
    eip_is_port = FALSE,
    K_matrix_is_port = TRUE,
    dts = FALSE,
    xde = TRUE,
    delay = FALSE
  )
  return(skills)
}


#' @title Compute steady states for `basicM` (**MY**)
#' @description Compute steady-state mosquito densities for the `basicM` module
#' given adult mosquito emergence `Lambda`.
#' @note Delegate to [steady_state_M_basicM], which dispatches on `xds_obj$xds`.
#' @inheritParams steady_state_M
#' @return a named [numeric] vector containing `M` followed by `P`, each
#' with one value per patch
#' @seealso [steady_state_M_basicM]
#' @keywords internal
#' @export
steady_state_M.basicM = function(Lambda, xds_obj, s=1){
  steady_state_M_basicM(Lambda, xds_obj, s)
}

#' @title Compute steady states for `basicM` (**MY**)
#' @description Compute steady-state mosquito densities for the `basicM` module
#' given adult mosquito emergence `Lambda`.
#' Both differential equations and discrete-time systems are supported.
#' @note Dispatch on `xds_obj$xds` to select the `xde` or `dts` method.
#' @inheritParams steady_state_M
#' @return a named [numeric] vector containing `M` followed by `P`, each
#' with one value per patch
#' @keywords internal
#' @export
steady_state_M_basicM = function(Lambda, xds_obj, s=1){
  UseMethod("steady_state_M_basicM", xds_obj$xds)
}

#' @title Compute steady states for `basicM` (**MY**)
#' @description Compute steady-state mosquito densities for the `basicM` module
#' given adult mosquito emergence `Lambda` for the differential-equation system.
#' @inheritSection basicM State Variables
#' @inheritSection basicM ODE
#' @inheritParams steady_state_M_basicM
#' @seealso [basicM]
#' @return a named [numeric] vector containing `M` followed by `P`, each
#' with one value per patch
#' @keywords internal
#' @export
steady_state_M_basicM.xde = function(Lambda, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  Omega_inv <- solve(Omega)
  M_eq  <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  return(c(M=M_eq, P=P_eq))
})}

#' @title Compute steady states for `basicM` (**MY**)
#' @description Compute steady-state mosquito densities for the `basicM` module
#' given adult mosquito emergence `Lambda` for the discrete-time system.
#' Minimize the sum of squared fixed-point residuals using [stats::nlm].
#' @inheritSection basicM State Variables
#' @inheritSection basicM DTS
#' @inheritParams steady_state_M_basicM
#' @seealso [basicM]
#' @return a named [numeric] vector containing `M` followed by `P`, each
#' with one value per patch
#' @keywords internal
#' @export
steady_state_M_basicM.dts = function(Lambda, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  
  dMP <- function(MP, Lambda, MY_obj){with(MY_obj,{
    M = MP[1:nPatches]
    P = MP[1:nPatches + nPatches]
    dM = Lambda + expm::expm(-Omega) %*% M - M
    dP = f*(M-P) + expm::expm(-Omega) %*% P - P
    sum(dM^2) + sum(dP^2)
  })}
  
  MP_eq=nlm(dMP, rep(Lambda, 2), MY_obj=xds_obj$MY_obj[[s]], Lambda=Lambda,
            gradtol=1e-8, steptol=1e-8, ndigit=15)$estimate

  M_eq = MP_eq[1:xds_obj$nPatches] 
  P_eq = MP_eq[1:xds_obj$nPatches + xds_obj$nPatches] 
  return(c(M=M_eq, P=P_eq))
})}
