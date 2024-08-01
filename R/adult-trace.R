# specialized methods for the adult mosquito trace model

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqZ] for the trace model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqZ.trace <- function(t, y, pars, s) {
    with(pars$MYZpar[[s]], return(with(baseline,f*q)*MYZf(t, scale=1)))
}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqM] for the trace model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqM.trace <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], return(with(baseline,f*q)*MYZf(t, scale=1)))
}


#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the trace model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.trace <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], return(MYZf(t, scale=1)))
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
#' @description Implements [Update_MYZt] for the trace (forced emergence) model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.trace <- function(t, y, pars, s){
  numeric(0)
}


#' @title Setup the trace
#' @description Implements [make_MYZpar] for the trace model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.trace = function(MYZname, pars, s, MYZopts=NULL){
  MYZpar <- create_MYZpar_trace(pars$nPatches, MYZopts)
  class(MYZpar) <- 'trace'
  pars$MYZpar[[s]] <- MYZpar
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

#' @title Make parameters for trace aquatic mosquito model
#' @param nPatches an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param f the blood feeding rate
#' @param q the human fraction
#' @param MYZm a vector of mean mosquito densities
#' @param MYZf a [function] of the form MYZf(t, pars) that computes temporal fluctuations
#' @return none
#' @export
create_MYZpar_trace = function(nPatches, MYZopts,
                               f = 1, q = 1,
                               MYZm = 1, MYZf = NULL){

  with(MYZopts,{
    MYZpar <- list()
    MYZpar$nPatches <- nPatches

    base = list()
    base$f      <- checkIt(f, nPatches)
    base$q      <- checkIt(q, nPatches)
    class(base) <- 'static'
    MYZpar$baseline = base
    MYZpar$now = base

    MYZpar$MYZm <- checkIt(MYZm, nPatches)
    if(is.null(MYZf)) MYZf = function(t, scale=1){return(scale*(MYZm + 0*t))}
    MYZpar$MYZf = MYZf
    return(MYZpar)
  })}

#' @title Setup the trace model
#' @description Implements [make_MYZinits] for the trace model
#' @inheritParams make_MYZinits
#' @return a [list] vector
#' @export
make_MYZinits.trace = function(pars, s, MYZopts=NULL){
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

#' @title Update inits for trace
#' @inheritParams update_MYZinits
#' @return none
#' @export
update_MYZinits.trace <- function(pars, y0, s) {
  return(pars)
}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the GeRM model.
#' @inheritParams get_MYZinits
#' @return none
#' @export
get_MYZinits.trace <- function(pars, s){
  return(c())
}

