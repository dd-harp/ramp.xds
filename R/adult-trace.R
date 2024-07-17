# specialized methods for the adult mosquito trace model

#' @title Compute bloodfeeding and mortality rates
#' @description Implements [MBionomics] for the trace model.
#' @inheritParams MBionomics
#' @return a named [list]
#' @export
MBionomics.trace <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f <- f0
    pars$MYZpar[[s]]$q <- q0
    return(pars)
})}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqZ] for the trace model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqZ.trace <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], return(f*q*scale*MYZf(t)))
}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqM] for the trace model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqM.trace <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], return(f*q*scale*MYZf(t)))
}


#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the trace model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.trace <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], return(scale*MYZf(t)))
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [dMYZdt] for the trace (forced emergence) model.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.trace <- function(t, y, pars, s){
  numeric(0)
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [DT_MYZt] for the trace (forced emergence) model.
#' @inheritParams DT_MYZt
#' @return a [numeric] vector
#' @export
DT_MYZt.trace <- function(t, y, pars, s){
  numeric(0)
}


#' @title Setup the trace
#' @description Implements [xde_setup_MYZpar] for the trace model
#' @inheritParams xde_setup_MYZpar
#' @return a [list] vector
#' @export
xde_setup_MYZpar.trace = function(MYZname, pars, s, EIPopts=list(), MYZopts=NULL, calK=NULL){
  pars$MYZpar[[s]] = make_MYZpar_trace(pars$nPatches, MYZopts)
  return(pars)
}

#' @title Steady States: MYZ-trace
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
xde_steady_state_MYZ.trace = function(Lambda, kappa, MYZpar){with(MYZpar,{
  return(numeric(0))
})}

#' @title Setup the trace
#' @description Implements [dts_setup_MYZpar] for the trace model
#' @inheritParams dts_setup_MYZpar
#' @return a [list] vector
#' @export
dts_setup_MYZpar.trace = function(MYZname, pars, s, EIPopts=list(), MYZopts=NULL, calK=NULL){
  pars$MYZpar[[s]] = make_MYZpar_trace(pars$nPatches, MYZopts)
  return(pars)
}


#' @title Make parameters for trace aquatic mosquito model
#' @param nPatches an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param f the blood feeding rate
#' @param q the human fraction
#' @param MYZm a vector of mean mosquito densities
#' @param MYZf a [function] of the form MYZf(t, pars) that computes temporal fluctuations
#' @return none
#' @export
make_MYZpar_trace = function(nPatches, MYZopts,
                              f = 0.3, q = 0.95,
                              MYZm = 1, MYZf = NULL){

  with(MYZopts,{
    MYZpar <- list()
    class(MYZpar) <- "trace"

    MYZpar$f <- checkIt(f, nPatches)
    MYZpar$q <- checkIt(q, nPatches)
    MYZpar$f0 <- MYZpar$f
    MYZpar$q0 <- MYZpar$q

    MYZpar$scale <- checkIt(MYZm, nPatches)
    if(is.null(MYZf)) MYZf = function(t){return(1)}
    MYZpar$MYZf = MYZf
    MYZpar$MYZm = MYZm

    return(MYZpar)
  })}

#' @title Setup the trace model
#' @description Implements [setup_MYZinits] for the trace model
#' @inheritParams setup_MYZinits
#' @return a [list] vector
#' @export
setup_MYZinits.trace = function(pars, s, MYZopts=NULL){
  return(pars)
}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for trace (forced emergence) model.
#' @inheritParams make_indices_MYZ
#' @return none
#' @export
make_indices_MYZ.trace <- function(pars, s) {
  return(pars)
}

#' @title Parse the output of deSolve and return variables for the trace model
#' @description Implements [parse_outputs_MYZ] for trace
#' @inheritParams parse_outputs_MYZ
#' @return [list]
#' @export
parse_outputs_MYZ.trace <- function(outputs, pars, s) {
  return(list())
}

#' @title Make parameters for trace aquatic mosquito model
#' @param pars a [list]
#' @param f the blood feeding rate
#' @param q the human fraction
#' @param MYZm a vector of mean mosquito densities
#' @param MYZf a [function] of the form MYZf(t, pars) that computes temporal fluctuations
#' @return none
#' @export
make_parameters_MYZ_trace <- function(pars, f=0.3, q=.95, MYZm=1, MYZf=NULL) {
  stopifnot(is.numeric(MYZm))
  MYZpar <- list()
  class(MYZpar) <- 'trace'
  xde <- "trace"
  class(xde) <- "trace"
  MYZpar$xde <- xde
  MYZpar$f0 <- f
  MYZpar$f <- f
  MYZpar$q0 <- q
  MYZpar$q <- q
  MYZpar$scale <- checkIt(MYZm, pars$nPatches)
  if(is.null(MYZf)) MYZf = function(t){return(1)}
  MYZpar$MYZf = MYZf
  pars$MYZpar[[1]] <- MYZpar
  return(pars)
}

#' @title Make parameters for trace aquatic mosquito model
#' @param pars a [list]
#' @param MYZ0 is set to NULL for the trace model
#' @return none
#' @export
make_inits_MYZ_trace<- function(pars, MYZ0=NULL) {
  pars$MYZinits[[1]] = numeric(0)
  return(pars)
}

#' @title Update inits for trace
#' @inheritParams update_inits_MYZ
#' @return none
#' @export
update_inits_MYZ.trace <- function(pars, y0, s) {
  return(pars)
}

#' @title Return initial values as a vector
#' @description Implements [get_inits_MYZ] for the GeRM model.
#' @inheritParams get_inits_MYZ
#' @return none
#' @export
get_inits_MYZ.trace <- function(pars, s){
  return(c())
}

