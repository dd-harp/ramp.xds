# the aquatic mosquito `basicL` competition model

#' @title The **L** Module Skill Set 
#' 
#' @description The **L** skill set is a list of 
#' an module's capabilities 
#'
#' @param Lname the name of the **L** module 
#' 
#' @return *L* module skill set, as a list 
#' 
#' @export
skill_set_L.basicL = function(Lname = "basicL"){
  list(trivial=FALSE)
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_L.basicL = function(xds_obj, s){
  return(xds_obj)
}

#' @title Compute Derivatives for **L** module `basicL` 
#' 
#' @description
#' This implements differential equation model for aquatic mosquito ecology.
#' The equations have been modified slightly from a version published by
#' Smith DL, *et al.* (2013). 
#'
#' **Variables:**
#'
#' - \eqn{L}: the density of mosquito larvae in each habitat
#'
#' **Input Term:**
#' 
#' - \eqn{\eta} or `eta`: egg deposition rate (from [F_eggs])
#'
#' **Parameters:**
#'
#' - \eqn{\psi} or `psi`: maturation rate
#' - \eqn{\xi} or `xi`: delayed maturation parameter in response to mean crowding
#' - \eqn{\phi} or `phi`: density-independent death rate
#' - \eqn{\theta} or `theta`: the slope of the mortality rate in response to mean crowding
#'
#' **Dynamical System:**
#'
#' \deqn{dL/dt = \eta - (\psi\;e^{-\xi L} + \phi + \theta L)L}
#' 
#' **Output Term:**
#' 
#' - The function [F_emerge] computes the net emergence rate (\eqn{\alpha}):
#'
#' \deqn{\alpha = \psi e^{-\xi L} L }
#' 
#' **Regulation:**
#' 
#' In this model, population is regulated in two ways. 
#' First, per-capita mortality increases with mean crowding; 
#' per-capita mortality is \eqn{\phi + \theta L.} 
#' Second, maturation is delayed in response to mean crowding \eqn{\psi\;e^{-\xi L.}} 
#' Depending on the values of \eqn{\xi,} 
#' productivity in some habitats might not be a monotonically increasing function of egg laying. 
#' 
#' The model by Smith DL, *et al.* (2013) did not include delayed maturation; 
#' that model is recovered by setting \eqn{\xi=0.}
#'
#' @inheritParams dLdt
#' @return a [numeric] vector
#' @references{\insertRef{SmithDL2013LarvalDynamics}{ramp.xds} }
#' @seealso [make_L_obj_basicL]
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


#' @title Update State Variables for `basicL` (**L** Component)
#' @description Implements [Update_Lt] for the `basicL` competition model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.basicL <- function(t, y, xds_obj, s) {
  eta <- xds_obj$eggs_laid[[s]]
  with(get_L_vars(y, xds_obj, s),{
    L <- y[L_ix]
    with(xds_obj$L_obj[[s]], {
      Lt = eta + (1-exp(-(psi+xi*L)))*exp(-(phi+theta*L))*L
      return(Lt)
    })
  })
}

#' @title Compute Emergent Adults for `basicL` (**L** Component)
#' @description The number of adults emerging from the habitats,
#' per day, is:
#' \deqn{\psi e^{-\xi L} L.}
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @seealso [dLdt.basicL]
#' @export
F_emerge.basicL <- function(t, y, xds_obj, s) {
  L <- y[xds_obj$L_obj[[s]]$ix$L_ix]
  with(xds_obj$L_obj[[s]],{
    return(psi*exp(-xi*L)*L)
  })
}

#' @title Baseline Bionomics for `basicL` (**L** Component)
#' 
#' @description Set **L** component parameters
#' to baseline values for `basicL`
#' @inheritParams LBaseline
#' @keywords internal 
#' 
#' @return a **`ramp.xds`** object
#' @export
LBaseline.basicL <- function(t, y, xds_obj, s) {

  with(xds_obj$L_obj[[s]],{
    xds_obj$L_obj[[s]]$psi_t      <- F_maturation(t, xds_obj, s) 
    xds_obj$L_obj[[s]]$phi_t      <- F_larval_mort(t, xds_obj, s) 
    xds_obj$L_obj[[s]]$xi_t       <- F_dlay_maturation(t, xds_obj, s) 
    xds_obj$L_obj[[s]]$theta_t    <- F_larval_dd_mort(t, xds_obj, s) 
    
    xds_obj$L_obj[[s]]$es_psi   <- rep(1, nHabitats)
    xds_obj$L_obj[[s]]$es_phi   <- rep(1, nHabitats)
    xds_obj$L_obj[[s]]$es_xi    <- rep(1, nHabitats)
    xds_obj$L_obj[[s]]$es_theta <- rep(1, nHabitats)
    return(xds_obj)
})}

#' @title Bionomics for `basicL` (**L** Component)
#' @description Implements [LBionomics] for the `basicL`
#' @inheritParams LBionomics
#' @keywords internal 
#' @return a **`ramp.xds`** object
#' @export
LBionomics.basicL <- function(t, y, xds_obj, s) {
  with(xds_obj$L_obj[[s]],{
    xds_obj$L_obj[[s]]$psi   <- psi_t*es_psi
    xds_obj$L_obj[[s]]$phi   <- phi_t*es_phi
    xds_obj$L_obj[[s]]$xi    <- xi_t*es_xi
    xds_obj$L_obj[[s]]$theta <- theta_t*es_theta
    return(xds_obj)
})}


#' @title Setup `L_obj` for `basicL` (**L** Component)
#' @description The function sets up `L_obj` for the \eqn{s^{th}} species
#' by calling [make_L_obj_basicL]
#' @inheritParams setup_L_obj
#' @return an **`xds`** object
#' @seealso [make_L_obj_basicL]
#' @export
setup_L_obj.basicL = function(Lname, xds_obj, s, options=list()){
  L_obj <- make_L_obj_basicL(xds_obj$nHabitats, options)
  class(L_obj) <- c("basicL", paste("basicL_", xds_obj$xds, sep=""))
  xds_obj$L_obj[[s]] = L_obj 
  xds_obj <- LBaseline(0, 0, xds_obj, 1)
  return(xds_obj)
}

#' @title Make `L_obj` for `basicL` (**L** Component)
#' @description The following parameters will be set to the values in
#' `options.` If they are not found, default values will be used.
#'
#' - \eqn{\psi} or `psi`: maturation rate
#' - \eqn{\xi} or `xi`: delayed maturation response due to mean crowding
#' - \eqn{\phi} or `phi`: density-independent death rate
#' - \eqn{\theta} or `theta`: density dependence in mortality: the slope of the response to mean crowding
#'
#' @param nHabitats the number of habitats in the model
#' @param options a named [list]
#' 
#' @param psi maturation rates for each aquatic habitat
#' @param xi delayed maturation in response to mean crowding
#' @param phi density-independent mortality rates for each aquatic habitat
#' @param theta density-dependent mortality terms for each aquatic habitat
#' 
#' @seealso Called by: [setup_L_obj.basicL]. Related: [dLdt.basicL] & [Update_Lt.basicL]
#' @return **`L_obj`** an **L** component object
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

#' @title Get **L** Component Parameters for `basicL`
#' @description Get the **L** component parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @keywords internal
#' @return a [list]
#' @seealso [dLdt.basicL] or [change_L_pars.basicL]
#' @export
get_L_pars.basicL <- function(xds_obj, s=1) {
  with(xds_obj$L_obj[[s]], list(
    psi=psi, xi=xi, phi=phi, theta=theta
  ))
}

#' @title Set **L** Component parameters for `basicL`
#' @description Set the values of **L** component parameters
#' - `psi` or \eqn{\psi}
#' - `xi`  or \eqn{\xi}
#' - `phi` or \eqn{\phi}
#' - `theta` or \eqn{\theta}
#' @inheritParams change_L_pars
#' @seealso [dLdt.basicL] or [make_L_obj_basicL]
#' @return an **`xds`** object
#' @export
change_L_pars.basicL <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$L_obj[[s]], with(options,{
    xds_obj$L_obj[[s]]$psi_t = checkIt(psi, nHabitats)
    xds_obj$L_obj[[s]]$xi = checkIt(xi, nHabitats)
    xds_obj$L_obj[[s]]$phi_t = checkIt(phi, nHabitats)
    xds_obj$L_obj[[s]]$theta = checkIt(theta, nHabitats)
    return(xds_obj)
  }))}



#' @title Setup Initial Values for `basicL` (**L** Component)
#' @description This sets initial values of the variable \eqn{L} by
#' calling [make_L_inits_basicL]. Default values are used unless other values
#' are passed in `options` by name (*i.e.* `options$L`)
#' @inheritParams setup_L_inits
#' @seealso [make_L_inits_basicL]
#' @return a [list]
#' @export
setup_L_inits.basicL = function(xds_obj, s, options=list()){
  xds_obj$L_obj[[s]]$inits = make_L_inits_basicL(xds_obj$nHabitats, options)
  return(xds_obj)
}

#' @title Make Initial Values for `basicL` (**L** Component)
#' @description Initial values of the variable \eqn{L} can be set
#' @param nHabitats the number of habitats in the model
#' @param options a [list] that overwrites default values
#' @param L initial conditions
#' @return a [list] with Linits added
#' @export
make_L_inits_basicL = function(nHabitats, options=list(), L=1){with(options,{
  L = checkIt(L, nHabitats)
  return(list(L=L))
})}

#' @title List **L** Component Variables for `basicL`
#' @description Extract the **L** component variables from the
#' vector of state variables (`y`) and return them as a named list
#' @inheritParams get_L_vars
#' @return a named [list]
#' @export
get_L_vars.basicL <- function(y, xds_obj, s){
  with(xds_obj$L_obj[[s]]$ix,
       return(list(
         L = y[L_ix]
       )))
}

#' @title Set the Initial Values for `basicL` (**L** Component)
#' @description Initial values of the variable \eqn{L} are reset if they are
#' passed as a named component of `options`
#' @inheritParams change_L_inits
#' @return an **`xds`** object
#' @export
change_L_inits.basicL <- function(xds_obj, s=1, options=list()) {
  with(xds_obj$L_obj[[s]]$inits, with(options,{
    xds_obj$L_obj[[s]]$inits$L = L
    return(xds_obj)
}))}

#' @title Setup Variables Indices for `basicL` (**L** Component)
#' @description Index the **L** component variables
#' for the `basicL` module
#' @inheritParams setup_L_ix
#' @keywords internal 
#' @return an **`xds`** object
#' @export
setup_L_ix.basicL <- function(xds_obj, s) {with(xds_obj,{

  L_ix <- seq(from = max_ix+1, length.out = nHabitats)
  max_ix <- max(L_ix)

  xds_obj$max_ix = max_ix

  xds_obj$L_obj[[s]]$ix = list(L_ix=L_ix)

  return(xds_obj)
})}

#' @title parse **L** Component Variables for `basicL`
#' @description The function returns the column representing
#' the variable \eqn{L} from a matrix where each row is a state variable.
#' The variable is returned as a named list.
#' @inheritParams parse_L_orbits
#' @return a named [list]
#' @export
parse_L_orbits.basicL <- function(outputs, xds_obj, s) {
  L = outputs[,xds_obj$L_obj[[s]]$ix$L_ix]
  return(list(L=L))
}

#' @title Compute the Steady State of `dLdt.basicL` (**L** Component)
#' @description Given an egg deposition rate `eta,`
#' return a steady state value for the equations in [dLdt.basicL]
#' @note This function does not use deSolve
#' @inheritParams steady_state_L
#' @return the values of \eqn{L} at the steady state
#' @importFrom stats nlm
#' @export
steady_state_L.basicL_ode = function(eta, xds_obj, s=1){
  with(xds_obj$L_obj[[s]],{
    dL <- function(L, eta, L_obj){with(L_obj,{
      sum((eta - (psi*exp(-xi*L) + phi + (theta*L))*L)^2)
    })}
    L=nlm(dL, eta, L_obj=L_obj, eta=eta)$estimate
    list(L=L)
})}
