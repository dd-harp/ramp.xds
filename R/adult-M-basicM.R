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
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate --- use [setup_sigma] or [setup_emigration_rates]}
#'   \item{`mu` or \eqn{\mu}}{emigration loss fraction --- use [setup_mu] or [setup_emigration_loss]}
#'   \item{`K` or `K_matrix`}{mosquito dispersal matrix --- use [setup_K_matrix]}
#'   \item{`nu` or \eqn{\nu}}{egg batch rate --- use [setup_nu] or [setup_egg_batch_rates]}
#'   \item{`eggsPerBatch` or \eqn{\Gamma}}{number of eggs per batch}
#' }
#'
#' After basic setup, [change_MY_pars.basicM] can be used to change static
#' parameter values. The `basicM` module does not implement dynamic bionomic
#' ports.
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
#'
#' @section Blood Feeding:
#' The blood feeding rate \eqn{f} and human fraction \eqn{q} are parameters of
#' the module. At baseline they are static. They should satisfy natural
#' constraints: \eqn{f=0} if no blood hosts are available, \eqn{q=0} if no
#' humans are available, and \eqn{q=1} if humans are the only available hosts.
#' The module does not model infection, but `fqM` is used to compute the
#' human biting rate.
#'
#' @section Dynamics:
#' Adult mosquitoes emerge from aquatic habitats at rate \eqn{\Lambda} and
#' disperse and die according to \eqn{\Omega}. The ODE model is
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda-\Omega\cdot M \\\\
#' dP/dt &= f(M-P)-\Omega\cdot P
#' \end{array}}
#' Here, \eqn{P} is the density of mosquitoes that have blood-fed at least
#' once. Parity, \eqn{P/M}, is computed during parsing and stored as `parous`.
#' For discrete-time models, [Update_MYt.basicM] uses the one-day update
#' equations.
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
#' The egg batch rate \eqn{\nu} and the number of eggs per batch
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

#' @title The skill set
#'
#' @inheritParams setup_skillset_MY
#'
#' @return the **`xds`** object
#'
#' @keywords internal
#' @export
setup_skillset_MY.basicM = function(xds_obj, s){
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
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_MY.basicM = function(xds_obj, s){
  return(xds_obj)
}


#' @title Compute derivatives for `basicM` (**MY**)
#' @description Implements [dMYdt] for the basicM xde ODE model.
#' @details \deqn{\begin{array}{rl}dM/dt &= \Lambda(t) - \Omega \cdot M \\ dP/dt &= f(M-P) - \Omega\cdot P\end{array}}
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamical Terms
#' @inheritSection basicM Parameters
#' @inheritSection basicM Dynamics
#' @inheritParams dMYdt
#' @seealso [basicM], [F_fqZ.basicM], [F_eggs.basicM]
#' @return a [numeric] vector
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


#' @title Mosquito bionomics for `basicM` (**MY**)
#' @description Implements [MBionomics] for models with no exogenous forcing
#' @inheritSection basicM Parameters
#' @inheritSection basicM Survival and Dispersal
#' @inheritParams MBionomics
#' @seealso [basicM], [MEffectSizes.basicM]
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBionomics.basicM <- function(t, y, xds_obj, s){with(xds_obj$MY_obj[[s]],{
  # Baseline parameters
  xds_obj$MY_obj[[s]]$f_t      <- F_f(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t      <- F_q(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t      <- F_g(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t  <- F_sigma(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu       <- F_mu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu       <- F_nu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$K_matrix <- F_K_matrix(t, xds_obj, s)

  # Reset Effect Sizes
  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)

  return(xds_obj)
})}

#' @title Apply effect sizes for `basicM` (**MY**)
#' @description Implements [MEffectSizes] for models with no exogenous forcing
#' @inheritSection basicM Parameters
#' @inheritParams MEffectSizes
#' @seealso [basicM], [MBionomics.basicM]
#' @return an **`xds`** object
#' @keywords internal
#' @export
MEffectSizes.basicM <- function(t, y, xds_obj, s) {with(xds_obj$MY_obj[[s]],{
  xds_obj$MY_obj[[s]]$f     <- es_f*f_t
  xds_obj$MY_obj[[s]]$q     <- es_q*q_t
  xds_obj$MY_obj[[s]]$g     <- es_g*g_t
  xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
  xds_obj$MY_obj[[s]]$mu    <- es_mu*mu_t

  xds_obj <- update_Omega(xds_obj, s)

  return(xds_obj)
})}


#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamics
#' @inheritParams steady_state_M
#' @seealso [basicM]
#' @return a [list]
#' @keywords internal
#' @export
steady_state_M.basicM_ode = function(Lambda, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  Omega_inv <- solve(Omega)
  M_eq  <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  return(c(M=M_eq, P=P_eq))
})}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamics
#' @inheritParams steady_state_M
#' @seealso [basicM]
#' @return a [list]
#' @keywords internal
#' @export
steady_state_M.basicM_dts = function(Lambda, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  Omega_inv <- solve(Omega)
  M_eq  <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  return(c(M=M_eq, P=P_eq))
})}

# specialized methods for the adult mosquito basicM model

#' @title Update state variables for `basicM` (**MY**)
#' @description Implements [Update_MYt] for the basicM model.
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamics
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
      Pt <- (1-exp(-f))*(M-P) + expm::expm(-Omega) %*% P

      return(c(Mt, Pt))
    })
  })
}



#' @title Set up `basicM` (**MY**)
#' @description
#'
#' This sets up a `basicM` model object for the **MY** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_M_obj_basicM]
#' + the initial conditions are set by [setup_MY_inits]
#' + `options` is passed to both to override the defaults
#'
#' @inheritSection basicM State Variables
#' @inheritSection basicM Parameters
#' @seealso [basicM], [make_M_obj_basicM], [setup_MY_inits.basicM]
#'
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#'
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @keywords internal
#' @export
setup_MY_obj.basicM = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MYname = "basicM"
  MY_obj <- make_M_obj_basicM(xds_obj$nPatches, options)
  class(MY_obj) <- c("basicM", paste("basicM_", xds_obj$xds, sep=""))
  xds_obj$MY_obj[[s]]= MY_obj
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, FALSE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  return(xds_obj)
}

#' @title Make parameters for basicM adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param options a named list: named values override defaults
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
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

#' @title Get parameters for `basicM` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritSection basicM Parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @seealso [basicM], [change_MY_pars.basicM]
#' @keywords internal
#' @export
get_MY_pars.basicM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, K_matrix=K_matrix
  ))
}

#' @title Infective mosquito biting rate for `basicM`
#' @description Implements [F_fqZ] for the basicM xde model.
#' @inheritSection basicM Infective Biting
#' @inheritParams F_fqZ
#' @seealso [basicM], [F_fqM.basicM]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.basicM <- function(t, y, xds_obj, s) {
  0*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title Mosquito biting rate for `basicM`
#' @description Returns `fqM`, the daily mosquito biting density on humans
#' for each patch in the basicM model.
#' @inheritSection basicM Infective Biting
#' @inheritParams F_fqM
#' @seealso [basicM], [F_hbr]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.basicM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  M = y[xds_obj$ix$MY[[s]]$M_ix]
  return(f*q*M)
}

#' @title Egg production rate for `basicM`
#' @description Implements [F_eggs] for the basic ecology model
#' @inheritSection basicM Egg Laying
#' @inheritParams F_eggs
#' @seealso [basicM], [EggLaying]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.basicM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]], {
    return(M*nu*eggsPerBatch)
  })
}

#' @title Setup initial values for `basicM` (**MY**)
#' @description Set initial values for all the variables in the `basicM` module
#' (**MY**) for the \eqn{s^{th}} species.
#' + [make_MY_inits_basicM] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or
#' see the inline module documentation.
#' @inheritSection basicM State Variables
#' @inheritParams setup_MY_inits
#' @return a [list] vector
#' @keywords internal
#' @export
setup_MY_inits.basicM = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_basicM(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Change initial values for `basicM` (**MY**)
#' @description For the `basicM` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#' @inheritSection basicM State Variables
#' @inheritParams change_MY_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_inits.basicM <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MYinits[[s]]$M = M
    xds_obj$MYinits[[s]]$P = P
    return(xds_obj)
  }))}

#' @title Make inits for basicM adult mosquito model
#' @param nPatches the number of patches
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @return a [list]
#' @keywords internal
#' @export
make_MY_inits_basicM = function(nPatches, options, M=5, P=1){
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    return(list(M=M, P=P))
  })}


#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritSection basicM State Variables
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.basicM <- function(y, xds_obj, s){
  with(xds_obj$ix$MY[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix]
       )))
}

#' @title Change parameters for `basicM` (**MY**)
#' @description For the `basicM` module (**MY**) for the \eqn{s^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#' @inheritSection basicM Parameters
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_pars.basicM <- function(xds_obj, options=list(), s=1) {
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

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the basic M model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @importFrom utils tail
#' @keywords internal
#' @export
setup_MY_ix.basicM <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(P_ix, 1)

  xds_obj$max_ix = max_ix

  xds_obj$ix$MY[[s]] = list(M_ix=M_ix, P_ix=P_ix)

  return(xds_obj)
})}

#' @title Parse outputs for `basicM` (**MY**)
#' @description Implements [parse_MY_orbits] for the basicM model.
#' @inheritSection basicM State Variables
#' @inheritSection basicM Dynamics
#' @inheritParams parse_MY_orbits
#' @seealso [basicM], [get_MY_vars.basicM]
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits.basicM <- function(outputs, xds_obj, s) {
  with(xds_obj$ix$MY[[s]],{
    M = outputs[,M_ix]
    P = outputs[,P_ix]
    parous = P/M
    return(list(M=M, P=P, parous=parous))
  })}


#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_f.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_q.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_g.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_sigma.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}
