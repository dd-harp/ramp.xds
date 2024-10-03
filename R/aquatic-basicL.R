# the aquatic mosquito `basicL` competition model

#' @title **L** Component Derivatives for `basicL`
#' @description
#' This implements differential equation model for aquatic mosquito ecology.
#' The equations have been modified slightly from a version published by
#' Smith DL, *et al.* (2013); this version includes delayed maturation i
#' in response to mean crowding.
#'
#' **Variables:**
#'
#' - \eqn{L}: the density of mosquito larvae in each habitat
#'
#' **Parameters and Terms:**
#'
#' - \eqn{\eta} or `eta`: egg deposition rate (from [F_eggs])
#' - \eqn{\psi} or `psi`: maturation rate
#' - \eqn{\xi} or `xi`: delayed maturation parameter in response to mean crowding
#' - \eqn{\phi} or `phi`: density-independent death rate
#' - \eqn{\theta} or `theta`: the slope of the mortality rate in response to mean crowding
#'
#' **Dynamics:**
#'
#' \deqn{dL/dt = \eta - (\psi\;e^{-\xi L} + \phi + \theta L)L}
#'
#' Per-capita mortality is thus \eqn{\phi + \theta L}, and the emergence rate
#' of adult mosquitoes is \eqn{\psi e^{-\xi L} L }
#'
#' @inheritParams dLdt
#' @return a [numeric] vector
#' @references{\insertRef{SmithDL2013LarvalDynamics}{ramp.xds} }
#' @seealso [create_Lpar_basicL]
#' @export
dLdt.basicL <- function(t, y, pars, s) {
  eta <- as.vector(pars$eggs_laid[[s]])
  with(pars$ix$L[[s]],{
    L <- y[L_ix]
    with(pars$Lpar[[s]], {
      dL = eta - (psi*exp(-xi*L) + phi + (theta*L))*L
      return(dL)
    })
  })
}

#' @title Set up `Lpar` for the `basicL` model
#' @description The function sets up `Lpar` for the \eqn{s^{th}} species
#' by calling [create_Lpar_basicL]
#' @inheritParams make_Lpar
#' @return an **`xds`** object
#' @seealso [create_Lpar_basicL]
#' @export
make_Lpar.basicL = function(Lname, pars, s, Lopts=list()){
  pars$Lpar[[s]] = create_Lpar_basicL(pars$nHabitats, Lopts)
  pars <- LBionomics(0, 0, pars, s)
  return(pars)
}

#' @title Create `Lpar` for `basicL`
#' @description The following parameters will be set to the values in
#' `Lopts.` If they are not found, default values will be used.
#'
#' - \eqn{\psi} or `psi`: maturation rate
#' - \eqn{\xi} or `xi`: delayed maturation response due to mean crowding
#' - \eqn{\phi} or `phi`: density-independent death rate
#' - \eqn{\theta} or `theta`: density dependence in mortality: the slope of the response to mean crowding
#'
#' @param nHabitats the number of habitats in the model
#' @param Lopts a named [list]
#' @param psi maturation rates for each aquatic habitat
#' @param xi delayed maturation in response to mean crowding
#' @param phi density-independent mortality rates for each aquatic habitat
#' @param theta density-dependent mortality terms for each aquatic habitat
#' @seealso Called by: [make_Lpar.basicL]. Related: [dLdt.basicL] & [Update_Lt.basicL]
#' @return **`Lpar`** an **L** component object
#' @export
create_Lpar_basicL = function(nHabitats, Lopts=list(), psi=1/8, xi=0, phi=1/8, theta=1/100){
  with(Lopts,{
    Lpar = list()
    class(Lpar) <- "basicL"
    Lpar$nHabitats <- nHabitats
    Lpar$psi_t = checkIt(psi, nHabitats)
    Lpar$xi = checkIt(xi, nHabitats)
    Lpar$phi_t = checkIt(phi, nHabitats)
    Lpar$theta = checkIt(theta, nHabitats)
    Lpar$es_psi = rep(1, nHabitats)
    Lpar$es_phi = rep(1, nHabitats)
    return(Lpar)
  })
}


#' @title Get **L** component parameters
#' @description Get the **L** component parameters
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @seealso [dLdt.basicL]
#' @export
get_Lpars.basicL <- function(pars, s=1) {
  with(pars$Lpar[[s]], list(
    psi=psi, xi=xi, phi=phi, theta=theta
  ))
}

#' @title Set **L** Component parameters for `basicL`
#' @description Set the values of **L** component parameters
#' - `psi` or \eqn{\psi}
#' - `xi`  or \eqn{\xi}
#' - `phi` or \eqn{\phi}
#' - `theta` or \eqn{\theta}
#' @inheritParams set_Lpars
#' @seealso [dLdt.basicL] or [create_Lpar_basicL]
#' @return an **`xds`** object
#' @export
set_Lpars.basicL <- function(pars, s=1, Lopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$Lpar[[s]], with(Lopts,{
    pars$Lpar[[s]]$psi_t = checkIt(psi, nHabitats)
    pars$Lpar[[s]]$xi = checkIt(xi, nHabitats)
    pars$Lpar[[s]]$phi_t = checkIt(phi, nHabitats)
    pars$Lpar[[s]]$theta = checkIt(theta, nHabitats)
    return(pars)
  }))}


#' @title Compute the Steady State for [dLdt.basicL]
#' @description Given an egg deposition rate `eta,`
#' return a steady state value for the equations in [dLdt.basicL]
#' @note This function does not use deSolve
#' @inheritParams xde_steady_state_L
#' @return the values of \eqn{L} at the steady state
#' @importFrom stats nlm
#' @export
xde_steady_state_L.basicL = function(eta, Lpar){
  dL <- function(L, eta, Lpar){with(Lpar,{
    sum((eta - (psi*exp(-xi*L) + phi + (theta*L))*L)^2)
  })}
  L=nlm(dL, eta, Lpar=Lpar, eta=eta)$estimate
  list(L=L)
}

#' @title Update **L** Component Variables for `basicL`
#' @description Implements [Update_Lt] for the basicL competition model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.basicL <- function(t, y, pars, s) {
  eta <- pars$eggs_laid[[s]]
  with(list_Lvars(y, pars, s),{
    L <- y[L_ix]
    with(pars$Lpar[[s]], {
      Lt = eta + (1-exp(-psi*exp(-xi*L)))*exp(-(phi+theta*L))*L
      return(Lt)
    })
  })
}

#' @title Compute emergent adults
#' @description The function computes the number of
#' emergent adults from aquatic habitats for `basicL`
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge.basicL <- function(t, y, pars, s) {
  L <- y[pars$ix$L[[s]]$L_ix]
  with(pars$Lpar[[s]],{
    return(psi*exp(-xi*L)*L)
  })
}




#' @title Reset **L** Component Parameters to Baseline
#' @description Set **L** component parameters
#' to baseline values for `basicL`
#' @inheritParams LBaseline
#' @return a **`ramp.xds`** object
#' @export
LBaseline.basicL <- function(t, y, pars, s) {
  with(pars$Lpar[[s]],{
    pars$Lpar[[s]]$es_psi <- rep(1, nHabitats)
    pars$Lpar[[s]]$es_phi <- rep(1, nHabitats)
    return(pars)
})}

#' @title Modify **L** Component Parameters
#' @description Implements [LBionomics] for the `basicL`
#' @inheritParams LBionomics
#' @return a **`ramp.xds`** object
#' @export
LBionomics.basicL <- function(t, y, pars, s) {
  with(pars$Lpar[[s]],{
    pars$Lpar[[s]]$psi <- psi_t*es_psi
    pars$Lpar[[s]]$phi <- phi_t*es_phi
    return(pars)
  })}



#' @title List **L** Component Variables
#' @description Extract the **L** component variables from the
#' vector of state variables (`y`) and return them as a named list
#' @inheritParams list_Lvars
#' @return a named [list]
#' @export
list_Lvars.basicL <- function(y, pars, s){
  with(pars$ix$L[[s]],
       return(list(
         L = y[L_ix]
       )))
}


#' @title Setup the Initial Values for **L** Component Variables for `basicL`
#' @description This sets initial values of the variable \eqn{L} by
#' calling [create_Linits_basicL]. Default values are used unless other values
#' are passed in `Lopts` by name (*i.e.* `Lopts$L`)
#' @inheritParams make_Linits
#' @seealso [create_Linits_basicL]
#' @return a [list]
#' @export
make_Linits.basicL = function(pars, s, Lopts=list()){
  pars$Linits[[s]] = create_Linits_basicL(pars$nHabitats, Lopts)
  return(pars)
}

#' @title Set the Initial Values for **L** Component Variables for `basicL`
#' @description Initial values of the variable \eqn{L} are reset if they are
#' passed as a named component of `Lopts`
#' @inheritParams set_Linits
#' @return an **`xds`** object
#' @export
set_Linits.basicL <- function(pars, s=1, Lopts=list()) {
  with(pars$Linits[[s]], with(Lopts,{
    pars$Linits[[s]]$L = L
    return(pars)
}))}

#' @title Update Initial Values for `basicL` from a state vector `y`
#' @description Extract the values of the variable \eqn{L} from
#' a state vector `y` and use them to set the initial value for \eqn{L}
#' @inheritParams update_Linits
#' @return an **`xds`** object
#' @export
update_Linits.basicL <- function(pars, y, s) {
  L = y[pars$ix$L[[s]]$L_ix]
  pars$Linits[[s]] = L
  return(pars)
}

#' @title Create Initial Values for **L** Component Variables for `basicL`
#' @description Initial values of the variable \eqn{L} can be set
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param L initial conditions
#' @return a [list] with Linits added
#' @export
create_Linits_basicL = function(nHabitats, Lopts=list(), L=1){with(Lopts,{
  L = checkIt(L, nHabitats)
  return(list(L=L))
})}

#' @title Parse **L** Component Variables for `basicL`
#' @description The function returns the column representing
#' the variable \eqn{L} from a matrix where each row is a state variable.
#' The variale is returned as a named list.
#' @inheritParams parse_Lorbits
#' @return a named [list]
#' @export
parse_Lorbits.basicL <- function(outputs, pars, s) {
  L = outputs[,pars$ix$L[[s]]$L_ix]
  return(list(L=L))
}

#' @title Make Indices for **L** Component Variables for `basicL`
#' @description Set the values of the indices for the **L** component variables
#' for the `basicL` module
#' @inheritParams make_indices_L
#' @return an **`xds`** object
#' @importFrom utils tail
#' @export
make_indices_L.basicL <- function(pars, s) {with(pars,{

  L_ix <- seq(from = max_ix+1, length.out = nHabitats)
  max_ix <- tail(L_ix, 1)

  pars$max_ix = max_ix

  pars$ix$L[[s]] = list(L_ix=L_ix)

  return(pars)
})}


