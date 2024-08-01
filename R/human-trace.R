# specialized methods for a human trace model

#' @title Derivatives for human population
#' @description Implements [dXdt] for the trace model.
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.trace <- function(t, y, pars, i) {
  numeric(0)
}

#' @title Derivatives for human population
#' @description Implements [Update_Xt] for the trace model.
#' @inheritParams Update_Xt
#' @return a [numeric] vector
#' @export
Update_Xt.trace <- function(t, y, pars, i) {
  numeric(0)
}

#' @title Make parameters for trace human model
#' @param nPatches the number of patches
#' @param Xopts a [list]
#' @param kappa net infectiousness
#' @param HPop initial human population density
#' @return a [list]
#' @export
create_Xpar_trace <- function(nPatches, Xopts, kappa=.1, HPop=1){with(Xopts,{
  Xpar <- list()
  class(Xpar) <- c('trace')
  Xpar$H = checkIt(HPop, nPatches)
  Xpar$kappa= checkIt(kappa, nPatches)
  Xpar$Kf = function(t){return(0*t + kappa)}
  return(Xpar)
})}

#' @title Size of effective infectious human population
#' @description Implements [F_X] for the trace model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.trace <- function(y, pars, i) {
  H = F_H(y, pars, i)
  X = with(pars$Xpar[[i]],  H*kappa)
  return(X)
}

#' @title Size of the human population
#' @description Implements [F_H] for the trace model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.trace <- function(y, pars, i) {
  pars$Xpar[[i]]$H
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_pr] for the trace model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr.trace <- function(vars, Xpar) {
  return(numeric(0))
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_pr] for the trace model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr_by_lm.trace <- function(vars, Xpar) {
  return(numeric(0))
}

#' @title Compute the steady states for the trace model as a function of the daily EIR
#' @description Compute the steady state of the trace model as a function of the daily eir.
#' @inheritParams xde_steady_state_X
#' @return the steady states as a named vector
#' @export
xde_steady_state_X.trace = function(foi, H, Xpar){with(Xpar,{
  return(numeric(0))
})}


#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_pr] for the trace model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr_by_rdt.trace <- function(vars, Xpar) {
  return(numeric(0))
}


#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_pr] for the trace model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr_by_pcr.trace <- function(vars, Xpar) {
  return(numeric(0))
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the trace model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.trace <- function(y, pars, i) {
  return(1)
}


#' @title xde_setup Xpar.trace
#' @description Implements [make_Xpar] for the trace model
#' @inheritParams make_Xpar
#' @return a [list] vectord
#' @export
make_Xpar.trace = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = create_Xpar_trace(pars$nPatches, Xopts)
  return(pars)
}


#' @title Setup Xinits.trace
#' @description Implements [make_Xinits] for the trace model
#' @inheritParams make_Xinits
#' @return a [list] vector
#' @export
make_Xinits.trace = function(pars, H, i, Xopts=list()){
  pars$Xpar[[i]]$H = H
  return(pars)
}

#' @title Add indices for human population to parameter list
#' @description Implements [make_X_indices] for the trace model.
#' @inheritParams make_X_indices
#' @return none
#' @importFrom utils tail
#' @export
make_X_indices.trace <- function(pars, i) {
  return(pars)
}

#' @title Parse the output of deSolve and return variables for the trace model
#' @description Implements [parse_outputs_X] for the trace model
#' @inheritParams parse_outputs_X
#' @return none
#' @export
parse_outputs_X.trace <- function(outputs, pars,i) {
  return(list())
}



#' @title Update inits for the trace human model from a vector of states
#' @inheritParams update_Xinits
#' @return none
#' @export
update_Xinits.trace <- function(pars, y0, i) {
  return(pars)
}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`.
#' @inheritParams get_Xinits
#' @return none
#' @export
get_Xinits.trace <- function(pars, i){
  numeric(0)
}
