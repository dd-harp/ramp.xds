# X
#' @title The `basicL` module for the L component
#' @description
#' A basic model of aquatic mosquito
#' ecology with density-dependent mortality and delayed maturation, based
#' on Smith DL, *et al.* (2013).
#'
#' @section State Variables:
#' \describe{
#'   \item{`L`}{density of mosquito larvae in each habitat}
#' }
#'
#' @section Parameters:
#' \describe{
#'   \item{`psi` or \eqn{\psi}}{maturation rate}
#'   \item{`xi` or \eqn{\xi}}{delayed maturation parameter in response to mean crowding}
#'   \item{`phi` or \eqn{\phi}}{density-independent mortality rate}
#'   \item{`theta` or \eqn{\theta}}{slope of density-dependent mortality}
#' }
#' 
#' @section Dynamical Term:
#'   
#' \describe{
#'   \item{`eta` or \eqn{\eta}}{the number of eggs laid in each habitat, per day}
#' }
#'
#' @section ODE:
#' 
#' The derivatives in [dLdt.basicL] are:
#' \deqn{dL/dt = \eta - (\psi e^{-\xi L} + \phi + \theta L) L}
#'
#' @section DTS:
#'
#' The one-day update in [Update_Lt.basicL] is:
#' \deqn{L_t = \eta + e^{-(\psi e^{-\xi L_{t-1}} + \phi + \theta L_{t-1})} L_{t-1}}
#'
#' @section Emergence:
#' 
#' The emergence rate of adult female mosquitoes from each habitat is computed by [F_emerge.basicL]:
#' \deqn{\alpha = \psi e^{-\xi L} L}
#' 
#' @section Regulation:
#'
#' In this model, population is regulated in two ways.
#' First, per-capita mortality increases with mean crowding;
#' per-capita mortality is \eqn{\phi + \theta L.}
#' Second, maturation is delayed in response to mean crowding \eqn{\psi\;e^{-\xi L}}.
#' Depending on the values of \eqn{\xi,}
#' productivity in some habitats might not be a monotonically increasing function of egg laying.
#'
#' The model by Smith DL, *et al.* (2013) did not include delayed maturation;
#' that model is recovered by setting \eqn{\xi=0.}
#'
#' @references{\insertRef{SmithDL2013LarvalDynamics}{ramp.xds} }
#' @seealso [make_L_obj_basicL], [setup_L_obj.basicL]
#' @name basicL
#' @rdname basicL
NULL

#' @title Compute the derivatives for `basicL` (**L**)
#'
#' @description Compute the derivatives for the `basicL` module:
#' \deqn{dL/dt = \eta-\left(\psi e^{-\xi L}+\phi+\theta L\right)L.}
#'
#' @inheritParams dLdt
#' @inheritSection basicL State Variables
#' @inheritSection basicL Dynamical Term
#' @inheritSection basicL Parameters
#' @inheritSection basicL ODE
#' @inheritSection basicL Emergence
#' @inheritSection basicL Regulation
#' @note Derivatives for `L` are returned in habitat order.
#' The egg input is read from `xds_obj$terms$eta[[s]]`.
#' @return a [numeric] vector of derivatives in the module's state-variable
#' order, with length `nHabitats`
#' @references{\insertRef{SmithDL2013LarvalDynamics}{ramp.xds} }
#' @seealso [basicL], [make_L_obj_basicL], [Update_Lt.basicL], [EggLaying]
#' @keywords internal
#' @export
dLdt.basicL <- function(t, y, xds_obj, s) {
  eta <- as.vector(xds_obj$terms$eta[[s]])
  with(get_L_vars(y, xds_obj, s),{
    with(xds_obj$L_obj[[s]], {
      dL = eta - (psi*exp(-xi*L) + phi + (theta*L))*L
      return(dL)
    })
  })
}

#' @title Update states for `basicL` (**L**)
#' @description Update the state variables for the `basicL` module:
#' \deqn{L_t = \eta + e^{-(\psi e^{-\xi L_{t-1}}+\phi+\theta L_{t-1})}L_{t-1}}
#' @inheritSection basicL State Variables
#' @inheritSection basicL Dynamical Term
#' @inheritSection basicL Parameters
#' @inheritSection basicL DTS
#' @inheritSection basicL Emergence
#' @inheritSection basicL Regulation
#' @inheritParams Update_Lt
#' @note Updated values for `L` are returned in habitat order.
#' The egg input is read from `xds_obj$terms$eta[[s]]`.
#' @return a [numeric] vector of updated states in the module's state-variable
#' order, with length `nHabitats`
#' @references{\insertRef{SmithDL2013LarvalDynamics}{ramp.xds} }
#' @seealso [basicL], [make_L_obj_basicL], [dLdt.basicL], [EggLaying]
#' @keywords internal
#' @export
Update_Lt.basicL <- function(t, y, xds_obj, s) {
  eta <- as.vector(xds_obj$terms$eta[[s]])
  with(get_L_vars(y, xds_obj, s),{
    with(xds_obj$L_obj[[s]], {
      Lt = eta + exp(-(psi*exp(-xi*L) + phi + (theta*L)))*L
      return(Lt)
    })
  })
}

#' @title Get the state variables for `basicL` (**L**)
#' @description Extract the state variables for the `basicL` module
#' from `y` and return them by name in a list for use during model solving.
#' @inheritParams get_L_vars
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @export
#' @note Variable indices are stored at `xds_obj$L_obj[[s]]$ix`.
get_L_vars.basicL <- function(y, xds_obj, s){
  with(xds_obj$L_obj[[s]]$ix,
       return(list(
         L = y[L_ix]
       )))
}

#' @title Mosquito bionomics for `basicL` (**L**)
#'
#' @description Compute the baseline bionomic parameter values for the `basicL`
#' module and reset their effect sizes to 1.
#' @inheritSection basicL Parameters
#' @note Evaluate [F_psi], [F_phi], [F_xi], and [F_theta] at `t`.
#' Store their values as `psi_t`, `phi_t`, `xi_t`, and `theta_t` at
#' `xds_obj$L_obj[[s]]`, and reset the corresponding `es_` fields to 1
#' per habitat. Realized parameter values are set by [LEffectSizes.basicL].
#' @inheritParams LBionomics
#' @seealso [basicL], [LEffectSizes.basicL]
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
LBionomics.basicL <- function(t, y, xds_obj, s) {

  with(xds_obj$L_obj[[s]],{
    xds_obj$L_obj[[s]]$psi_t      <- F_psi(t, xds_obj, s)
    xds_obj$L_obj[[s]]$phi_t      <- F_phi(t, xds_obj, s)
    xds_obj$L_obj[[s]]$xi_t       <- F_xi(t, xds_obj, s)
    xds_obj$L_obj[[s]]$theta_t    <- F_theta(t, xds_obj, s)

    xds_obj$L_obj[[s]]$es_psi   <- rep(1, nHabitats)
    xds_obj$L_obj[[s]]$es_phi   <- rep(1, nHabitats)
    xds_obj$L_obj[[s]]$es_xi    <- rep(1, nHabitats)
    xds_obj$L_obj[[s]]$es_theta <- rep(1, nHabitats)
    return(xds_obj)
})}

#' @title Apply effect sizes for `basicL` (**L**)
#' @description Apply effect sizes to the baseline bionomic parameter values for the `basicL` module.
#' @inheritSection basicL Parameters
#' @note At `xds_obj$L_obj[[s]]`, set `psi = psi_t * es_psi`,
#' `phi = phi_t * es_phi`, `xi = xi_t * es_xi`, and
#' `theta = theta_t * es_theta` per habitat.
#' This function does not evaluate parameter ports or reset effect sizes.
#' @inheritParams LEffectSizes
#' @seealso [basicL], [LBionomics.basicL]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
LEffectSizes.basicL <- function(t, y, xds_obj, s) {
  with(xds_obj$L_obj[[s]],{
    xds_obj$L_obj[[s]]$psi   <- psi_t*es_psi
    xds_obj$L_obj[[s]]$phi   <- phi_t*es_phi
    xds_obj$L_obj[[s]]$xi    <- xi_t*es_xi
    xds_obj$L_obj[[s]]$theta <- theta_t*es_theta
    return(xds_obj)
})}

#' @title Compute adult mosquito emergence for `basicL` (**L**)
#' @description Return adult female mosquito emergence per habitat.
#' @inheritSection basicL Emergence
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @seealso [basicL], [dLdt.basicL], [Emergence]
#' @keywords internal
#' @export
F_emerge.basicL <- function(t, y, xds_obj, s) {
  L <- y[xds_obj$L_obj[[s]]$ix$L_ix]
  with(xds_obj$L_obj[[s]],{
    return(psi*exp(-xi*L)*L)
  })
}

#' @title Get the maturation rate for `basicL` (**L**)
#' @description Return the stored realized maturation rate per habitat.
#' @inheritSection basicL Parameters
#' @inheritParams get_psi
#' @seealso [basicL], [change_psi]
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
get_psi.basicL = function(xds_obj, s=1){
  xds_obj$L_obj[[s]]$psi
}

#' @title Get the density-independent mortality rate for `basicL` (**L**)
#' @description Return the stored realized density-independent mortality rate per habitat.
#' @inheritSection basicL Parameters
#' @inheritParams get_phi
#' @seealso [basicL], [change_phi]
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
get_phi.basicL = function(xds_obj, s=1){
  xds_obj$L_obj[[s]]$phi
}

#' @title Get the delayed maturation parameter for `basicL` (**L**)
#' @description Return the stored realized delayed maturation parameter per habitat.
#' @inheritSection basicL Parameters
#' @inheritParams get_xi
#' @seealso [basicL], [change_xi]
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
get_xi.basicL = function(xds_obj, s=1){
  xds_obj$L_obj[[s]]$xi
}

#' @title Get the slope of density-dependent mortality for `basicL` (**L**)
#' @description Return the stored realized slope of density-dependent mortality per habitat.
#' @inheritSection basicL Parameters
#' @inheritParams get_theta
#' @seealso [basicL], [change_theta]
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
get_theta.basicL = function(xds_obj, s=1){
  xds_obj$L_obj[[s]]$theta
}

#' @title Make a module object for `basicL` (**L**)
#' @description Create a `basicL` module object and initialize its parameters.
#' Inspect parameters with [get_L_pars.basicL] and change supported values
#' with [change_L_pars.basicL]. Values in `options` override parameter arguments.
#' @seealso [get_L_pars.basicL], [change_L_pars.basicL]
#'
#' @inheritSection basicL Parameters
#'
#' @param nHabitats the number of habitats in the model
#' @param options a named [list]
#'
#' @param psi maturation rates for each aquatic habitat
#' @param xi delayed maturation in response to mean crowding
#' @param phi density-independent mortality rates for each aquatic habitat
#' @param theta slope of density-dependent mortality for each aquatic habitat
#'
#' @seealso Called by: [setup_L_obj.basicL]. Related: [dLdt.basicL] & [Update_Lt.basicL]
#' @return a [list]: an **L** module object
#' @keywords internal
#' @export
make_L_obj_basicL = function(nHabitats, options=list(), psi=1/8, xi=0, phi=1/8, theta=1/100){
  with(options,{
    L_obj = list()
    class(L_obj) <- "basicL"
    L_obj$nHabitats <- nHabitats

    L_obj <- setup_psi_obj(checkIt(psi, nHabitats), L_obj)
    L_obj <- setup_phi_obj(checkIt(phi, nHabitats), L_obj)
    L_obj <- setup_xi_obj(checkIt(xi, nHabitats), L_obj)
    L_obj <- setup_theta_obj(checkIt(theta, nHabitats), L_obj)

    return(L_obj)
  })
}


#' @title Set up the module object for `basicL` (**L**)
#' @description
#'
#' Set up a `basicL` module object for the **L** component and the \eqn{s^{th}} vector species.
#'
#' + Create the module object with [make_L_obj_basicL], passing `nHabitats` and `options`.
#' + Set the skill set using [get_skillset_L].
#' + Set initial values using [setup_L_inits], passing `options` to override defaults.
#' + Initialize habitat search weights using [setup_habitat_search_weights.setup].
#' + Return the updated **`xds`** model object.
#'
#' @seealso [basicL], [make_L_obj_basicL]
#'
#' @note The module object is stored at `xds_obj$L_obj[[s]]`
#' Its classes are `basicL` and `basicL_` followed by `xds_obj$xds`.
#'
#' @inheritParams setup_L_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_L_obj.basicL = function(Lname, membership, xds_obj, options=list(), s=1){
  L_obj <- make_L_obj_basicL(xds_obj$nHabitats, options)
  class(L_obj) <- c("basicL", paste("basicL_", xds_obj$xds, sep=""))
  xds_obj$L_obj[[s]] = L_obj
  xds_obj$L_obj[[s]]$skill_set <- get_skillset_L(xds_obj, s)
  xds_obj <- setup_L_inits(xds_obj, options=options, s=s)
  xds_obj <- setup_habitat_search_weights("setup", xds_obj, list(membership=membership), s=s)
  return(xds_obj)
}

#' @title Get parameters for `basicL` (**L**)
#' @seealso [make_L_obj_basicL], [change_L_pars.basicL]
#' @description Return the parameters for the `basicL` module as a named list.
#' @inheritSection basicL Parameters
#' @inheritParams get_L_pars
#' @return a named [list] of parameter values
#' @seealso [dLdt.basicL] or [change_L_pars.basicL]
#' @keywords internal
#' @export
get_L_pars.basicL <- function(xds_obj, s=1) {
  with(xds_obj$L_obj[[s]], list(
    psi=psi, xi=xi, phi=phi, theta=theta
  ))
}

#' @title Change parameters for `basicL` (**L**)
#' @seealso [make_L_obj_basicL], [get_L_pars.basicL]
#' @description Change parameter values named in `options` for the `basicL`
#' module, leaving other values unchanged.
#' Refresh baseline bionomics at time zero using [LBionomics], without calling [LEffectSizes].
#'
#' @inheritSection basicL Parameters
#' @inheritParams change_L_pars
#' @seealso [dLdt.basicL] or [make_L_obj_basicL]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_L_pars.basicL <- function(xds_obj, options=list(), s=1) {
  nHabitats <- xds_obj$nHabitats
  for (parameter in c("psi", "xi", "phi", "theta")) {
    if (parameter %in% names(options)) {
      port <- paste0(parameter, "_obj")
      xds_obj$L_obj[[s]][[port]][[parameter]] <-
        checkIt(options[[parameter]], nHabitats)
    }
  }

  y <- get_inits(xds_obj, flatten=TRUE)
  LBionomics(0, y, xds_obj, s)
}

#' @title Make initial values for `basicL` (**L**)
#' @description Create a named list of initial values for the `basicL` module.
#' Values in `options` override the defaults.
#' Initial values of the variable \eqn{L} can be set
#' by passing `L` in `options`.
#' @details Initial values must be numeric scalars or have length `nHabitats`.
#' Scalars are expanded to that length.
#' @param nHabitats the number of habitats in the model
#' @param options a [list] that overrides default values
#' @param L initial conditions
#' @return a named [list] of initial values
#' @keywords internal
#' @export
#' @seealso [setup_L_inits.basicL], [change_L_inits.basicL]
make_L_inits_basicL = function(nHabitats, options=list(), L=1){with(options,{
  L = checkIt(L, nHabitats)
  return(list(L=L))
})}



#' @title Set up initial values for `basicL` (**L**)
#' @description Set up initial values for the `basicL` module using
#' [make_L_inits_basicL]. Values in `options` override the defaults.
#'
#' The initial values of \eqn{L} can be passed as `options$L`.
#' @inheritParams setup_L_inits
#' @seealso [make_L_inits_basicL]
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [change_L_inits.basicL]
#' @note Initial values are stored at `xds_obj$L_obj[[s]]$inits`.
#' @seealso [get_L_inits]
setup_L_inits.basicL = function(xds_obj, options=list(), s=1){
  xds_obj$L_obj[[s]]$inits = make_L_inits_basicL(xds_obj$nHabitats, options)
  return(xds_obj)
}

#' @title Change initial values for `basicL` (**L**)
#' @description Change initial values named in `options` for the `basicL`
#' module, leaving other values unchanged.
#'
#' Initial values of the variable \eqn{L} are reset if they are
#' passed as a named component of `options`.
#' @inheritParams change_L_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @seealso [make_L_inits_basicL], [setup_L_inits.basicL]
#' @note Stored initial values for this module are at
#' `xds_obj$L_obj[[s]]$inits`.
change_L_inits.basicL <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$L_obj[[s]]$inits, with(options,{
    xds_obj$L_obj[[s]]$inits$L = L
    return(xds_obj)
}))}

#' @title Set up variable indices for `basicL` (**L**)
#' @description Assign consecutive variable indices for the `basicL` module,
#' starting after `xds_obj$max_ix`, and update the maximum index.
#' Allocate blocks in the order `L_ix`.
#' Each block has length `nHabitats`.
#' @inheritParams setup_L_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @export
#' @note Indices are stored at `xds_obj$L_obj[[s]]$ix`.
#' The maximum index is stored at `xds_obj$max_ix`.
setup_L_ix.basicL <- function(xds_obj, s) {with(xds_obj,{

  L_ix <- seq(from = max_ix+1, length.out = nHabitats)
  max_ix <- tail(L_ix, 1)

  xds_obj$max_ix = max_ix

  xds_obj$L_obj[[s]]$ix = list(L_ix=L_ix)

  return(xds_obj)
})}

#' @title Parse outputs for `basicL` (**L**)
#' @description Extract the model state outputs for the `basicL` module
#' from `outputs` and return them by name in a list.
#' Return the aquatic mosquito density `L`.
#' @note Variable indices are stored at `xds_obj$L_obj[[s]]$ix`.
#' @inheritParams parse_L_orbits
#' @return a [list]
#' @keywords internal
#' @export
parse_L_orbits.basicL <- function(outputs, xds_obj, s) {
  L = outputs[,xds_obj$L_obj[[s]]$ix$L_ix]
  return(list(L=L))
}

#' @title Check `basicL` (**L**)
#' @description Run no consistency checks
#'
#' @inheritParams check_L
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_L.basicL = function(xds_obj, s){
  return(xds_obj)
}

# the aquatic mosquito `basicL` competition model

#' @title Get the skill set for `basicL` (**L**)
#'
#' @description Return the capabilities and compatibilities of the `basicL` module as a list.
#'
#' @inheritParams get_skillset_L
#'
#' @return the **L** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_L.basicL = function(xds_obj, s){
  skills = list(
    psi_is_port = TRUE,
    phi_is_port = TRUE,
    xi_is_port = TRUE,
    theta_is_port = TRUE,
    dts = TRUE,
    xde = TRUE,
    delay = FALSE
  )
  return(skills)
}

#' @title Compute steady states for `basicL` (**L**)
#' @description Compute steady-state aquatic mosquito densities for the `basicL`
#' module given the daily egg input `eta` in each habitat.
#' @note Delegate to [steady_state_L_basicL], which dispatches on `xds_obj$xds`.
#' @inheritParams steady_state_L
#' @return a named [list] containing the values of \eqn{L} at the steady state
#' @seealso [steady_state_L_basicL]
#' @keywords internal
#' @export
steady_state_L.basicL = function(eta, xds_obj, s=1){
  steady_state_L_basicL(eta, xds_obj, s)
}

#' @title Compute steady states for `basicL` (**L**)
#' @description Compute steady-state aquatic mosquito densities for the `basicL`
#' module given the daily egg input `eta` in each habitat.
#' Both differential equations and discrete-time systems are supported.
#' @note Dispatch on `xds_obj$xds` to select the `xde` or `dts` method.
#' @inheritParams steady_state_L
#' @return a named [list] containing the values of \eqn{L} at the steady state
#' @keywords internal
#' @export
steady_state_L_basicL = function(eta, xds_obj, s=1){
  UseMethod("steady_state_L_basicL", xds_obj$xds)
}

#' @title Compute the steady state of `dLdt.basicL` (**L** component)
#' @description Given an egg deposition rate `eta`,
#' return a steady state value for the equations in [dLdt.basicL]
#' @inheritSection basicL ODE
#' @note This function does not use deSolve
#' @inheritParams steady_state_L_basicL
#' @return a named [list] containing the values of \eqn{L} at the steady state
#' @importFrom stats nlm
#' @keywords internal
#' @export
steady_state_L_basicL.xde = function(eta, xds_obj, s=1){
    dL <- function(L, eta, L_obj){with(L_obj,{
      sum((eta - (psi*exp(-xi*L) + phi + (theta*L))*L)^2)
    })}
    L=nlm(dL, eta, L_obj=xds_obj$L_obj[[s]], eta=eta)$estimate
    list(L=L)
}

#' @title Compute steady states for `basicL` (**L**)
#' @description Given the number of eggs laid in each habitat, per day, `eta`,
#' return steady-state values for the discrete-time system in [Update_Lt.basicL].
#' Minimize the sum of squared fixed-point residuals using [stats::nlm].
#' @inheritSection basicL DTS
#' @note This function does not use deSolve
#' @inheritParams steady_state_L_basicL
#' @return a named [list] containing the values of \eqn{L} at the steady state
#' @importFrom stats nlm
#' @keywords internal
#' @export
steady_state_L_basicL.dts = function(eta, xds_obj, s=1){
  dL <- function(L, eta, L_obj){with(L_obj,{
    sum((eta + exp(-(psi*exp(-xi*L) + phi + (theta*L)))*L - L)^2)
  })}
  L=nlm(dL, eta, L_obj=xds_obj$L_obj[[s]], eta=eta)$estimate
  list(L=L)
}
