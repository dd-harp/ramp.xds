# specialized methods for the aquatic mosquito basicL_dts competition model

#' @title Reset aquatic parameters to baseline
#' @description Implements [LBionomics] for the RM model
#' @inheritParams LBionomics
#' @return a named [list]
#' @export
LBionomics.basicL_dts <- function(t, y, pars, s) {with(pars$Lpar[[s]],{
  pars$Lpar[[s]]$psi <- psi0
  pars$Lpar[[s]]$phi <- phi0
  pars$Lpar[[s]]$xi   <- xi0
  pars$Lpar[[s]]$theta <- theta0

  return(pars)
})}


#' @title Number of newly emerging adults from each larval habitat
#' @description Implements [F_alpha] for the basicL_dts competition model.
#' @inheritParams F_alpha
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_alpha.basicL_dts <- function(t, y, pars, s) {
  L <- y[pars$ix$L[[s]]$L_ix]
  with(pars$Lpar[[s]],{
    exp(-(psi*exp(-xi*L)))*exp(-(phi+theta*L))*L
  })
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [DT_Lt] for the basicL_dts competition model.
#' @inheritParams DT_Lt
#' @return a [numeric] vector
#' @export
DT_Lt.basicL_dts <- function(t, y, pars, s) {
  eta <- pars$eggs_laid[[s]]
  with(list_Lvars(y, pars, s),{
    L <- y[L_ix]
    with(pars$Lpar[[s]], {
      Lt = eta + (1-exp(-psi*exp(-xi*L)))*exp(-(phi+theta*L))*L
      return(dL)
    })
  })
}

#' @title Setup Lpar for the basicL_dts model
#' @description Implements [dts_setup_Lpar] for the basicL_dts model
#' @inheritParams dts_setup_Lpar
#' @return a [list] vector
#' @export
dts_setup_Lpar.basicL_dts = function(Lname, pars, s, Lopts=list()){
  pars$Lpar[[s]] = make_Lpar_basicL_dts(pars$nHabitats, Lopts)
  return(pars)
}

#' @title Setup the basicL_dts model
#' @description Implements [setup_Linits] for the basicL_dts model
#' @inheritParams setup_Linits
#' @return a [list]
#' @export
setup_Linits.basicL_dts = function(pars, s, Lopts=list()){
  pars$Linits[[s]] = make_Linits_basicL_dts(pars$nHabitats, Lopts)
  return(pars)
}

#' @title Make parameters for basicL_dts competition aquatic mosquito model
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param psi maturation rates for each aquatic habitat
#' @param xi delayed maturation due to mean crowding
#' @param phi density-independent mortality rates for each aquatic habitat
#' @param theta density-dependent mortality terms for each aquatic habitat
#' @return a [list] with Lpar added
#' @export
make_Lpar_basicL_dts = function(nHabitats, Lopts=list(), psi=1/8, xi=0, phi=1/8, theta=1/100){with(Lopts,{
  Lpar = list()
  class(Lpar) <- "basicL_dts"

  Lpar$psi = checkIt(psi, nHabitats)
  Lpar$xi = checkIt(xi, nHabitats)
  Lpar$psi = checkIt(psi, nHabitats)
  Lpar$theta = checkIt(theta, nHabitats)

  Lpar$psi0 = checkIt(psi, nHabitats)
  Lpar$xi0 = checkIt(xi, nHabitats)
  Lpar$psi0 = checkIt(psi, nHabitats)
  Lpar$theta0 = checkIt(theta, nHabitats)

  return(Lpar)
})}

#' @title Make inits for basicL_dts competition aquatic mosquito model
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param L0 initial conditions
#' @return a [list] with Linits added
#' @export
make_Linits_basicL_dts = function(nHabitats, Lopts=list(), L0=1){with(Lopts,{
  L0 = checkIt(L0, nHabitats)
  return(list(L=L0))
})}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [make_indices_L] for basicL_dts competition model.
#' @inheritParams make_indices_L
#' @return none
#' @importFrom utils tail
#' @export
make_indices_L.basicL_dts <- function(pars, s) {with(pars,{

  L_ix <- seq(from = max_ix+1, length.out=nHabitats)
  max_ix <- tail(L_ix, 1)

  pars$max_ix = max_ix
  pars$ix$L[[s]] = list(L_ix=L_ix)
  return(pars)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams list_Lvars
#' @return a [list]
#' @export
list_Lvars.basicL_dts <- function(y, pars, s){
  with(pars$ix$L[[s]],
       return(list(
         L = y[L_ix]
       )))
}

#' @title Parse the variable names for the basicL_dts model
#' @description Implements [parse_outputs_L] for basicL_dts competition model.
#' @inheritParams parse_outputs_L
#' @return [list]
#' @export
parse_outputs_L.basicL_dts <- function(outputs, pars, s) {
  time = outputs[,1]
  L = outputs[,pars$ix$L[[s]]$L_ix+1]
  return(list(time=time, L=L))
}


#' @title Make parameters for basicL_dts competition aquatic mosquito model
#' @param pars a [list]
#' @param psi maturation rates for each aquatic habitat
#' @param xi delayed maturation due to mean crowding
#' @param phi density-independent mortality rates for each aquatic habitat
#' @param theta density-dependent mortality terms for each aquatic habitat
#' @return a [list] with Lpar added
#' @export
make_parameters_L_basicL_dts <- function(pars, psi, xi, phi, theta) {
  stopifnot(is.numeric(psi), is.numeric(phi), is.numeric(theta))
  Lpar <- list()
  class(Lpar) <- 'basicL_dts'
  Lpar$psi <- psi
  Lpar$xi <- xi
  Lpar$phi <- phi
  Lpar$theta <- theta

  Lpar$psi0 <- psi
  Lpar$xi0 <- xi
  Lpar$phi0 <- phi
  Lpar$theta0 <- theta

  pars$Lpar[[1]] <- Lpar
  return(pars)
}

#' @title Make inits for basicL_dts competition aquatic mosquito model
#' @param pars a [list]
#' @param L0 initial conditions
#' @return a [list] with Linits added
#' @export
make_inits_L_basicL_dts <- function(pars, L0){
  stopifnot(is.numeric(L0))
  pars$Linits[[1]] <- list(L=L0)
  return(pars)
}

#' @title Update inits for the basicL_dts aquatic mosquito competition model
#' @inheritParams update_inits_L
#' @return none
#' @export
update_inits_L.basicL_dts <- function(pars, y0, s) {
  L = y0[pars$ix$L[[s]]$L_ix]
  pars = make_Linits_basicL_dts(pars$nHabitats, L0=L)
  return(pars)
}

#' @title Return initial values as a vector
#' @description Implements [get_inits_L] for the GeRM model.
#' @inheritParams get_inits_L
#' @return none
#' @export
get_inits_L.basicL_dts <- function(pars, s){
  pars$Linits[[s]]$L
}

