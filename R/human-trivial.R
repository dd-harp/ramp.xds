# specialized methods for a human trivial model

#' @title Size of effective infectious human population
#' @description Implements [F_X] for the trivial model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X.trivial <- function(t, y, pars, i) {
  H = F_H(t, y, pars, i)
  X = with(pars$Xpar[[i]],  H*kappa*F_season(t, phase, season_opts)*F_trend(t, trend_opts))
  return(X)
}

#' @title Size of the human population
#' @description Implements [F_H] for the trivial model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.trivial <- function(t, y, pars, i) {
  pars$Xpar[[i]]$H
}

#' @title Make parameters for trivial human model
#' @param nPatches the number of patches
#' @param Xopts a [list]
#' @param kappa net infectiousness
#' @param HPop initial human population density
#' @param F_season a F_seasonality function
#' @param phase a parameter to set the phase of F_season
#' @param season_opts a [list] of options to pass to F_season
#' @param F_trend a F_trend function
#' @param trend_opts a [list] of options to pass to F_trend
#' @return a [list]
#' @export
create_Xpar_trivial <- function(nPatches, Xopts, kappa=.1, HPop=1,
                                F_season=F_no_season, phase=0, season_opts=list(),
                                F_trend=F_no_trend, trend_opts=list()){with(Xopts,{
                                  Xpar <- list()
                                  class(Xpar) <- c('trivial')
                                  Xpar$H = checkIt(HPop, nPatches)
                                  Xpar$kappa= checkIt(kappa, nPatches)
                                  Xpar$F_season <- F_season
                                  Xpar$phase <- phase
                                  Xpar$season_opts <- season_opts
                                  Xpar$F_trend <- F_trend
                                  Xpar$trend_opts <- trend_opts
                                  return(Xpar)
                                })}

#' @title \eqn{\cal X} Component Derivatives for the `trivial` model
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.trivial <- function(t, y, pars, i) {
  numeric(0)
}

#' @title Update States for the `trivial` \eqn{\cal X} Model
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' @inheritParams Update_Xt
#' @return a [numeric] vector
#' @export
Update_Xt.trivial <- function(t, y, pars, i) {
  numeric(0)
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_pr] for the trivial model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr.trivial <- function(vars, Xpar) {
  return(numeric(0))
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_pr] for the trivial model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr_by_lm.trivial <- function(vars, Xpar) {
  return(numeric(0))
}

#' @title Compute the steady states for the trivial model as a function of the daily EIR
#' @description Compute the steady state of the trivial model as a function of the daily eir.
#' @inheritParams xde_steady_state_X
#' @return the steady states as a named vector
#' @export
xde_steady_state_X.trivial = function(foi, H, Xpar){with(Xpar,{
  return(numeric(0))
})}


#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_pr] for the trivial model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr_by_rdt.trivial <- function(vars, Xpar) {
  return(numeric(0))
}


#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_pr] for the trivial model.
#' @inheritParams F_pr
#' @return a [numeric] vector numeric(0)
#' @export
F_pr_by_pcr.trivial <- function(vars, Xpar) {
  return(numeric(0))
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the trivial model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.trivial <- function(y, pars, i) {
  return(1)
}


#' @title xde_setup Xpar.trivial
#' @description Implements [make_Xpar] for the trivial model
#' @inheritParams make_Xpar
#' @return a [list] vectord
#' @export
make_Xpar.trivial = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = create_Xpar_trivial(pars$nPatches, Xopts)
  return(pars)
}


#' @title Setup Xinits.trivial
#' @description Implements [make_Xinits] for the trivial model
#' @inheritParams make_Xinits
#' @return a [list] vector
#' @export
make_Xinits.trivial = function(pars, H, i, Xopts=list()){
  pars$Xpar[[i]]$H = H
  return(pars)
}

#' @title Add indices for human population to parameter list
#' @description Implements [make_X_indices] for the trivial model.
#' @inheritParams make_X_indices
#' @return none
#' @importFrom utils tail
#' @export
make_X_indices.trivial <- function(pars, i) {
  return(pars)
}

#' @title Parse the output of deSolve and return variables for the trivial model
#' @description Implements [parse_Xorbits] for the trivial model
#' @inheritParams parse_Xorbits
#' @return none
#' @export
parse_Xorbits.trivial <- function(outputs, pars,i) {
  return(list())
}



#' @title Update inits for the trivial human model from a vector of states
#' @inheritParams update_Xinits
#' @return none
#' @export
update_Xinits.trivial <- function(pars, y0, i) {
  return(pars)
}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`.
#' @inheritParams get_Xinits
#' @return none
#' @export
get_Xinits.trivial <- function(pars, i){
  numeric(0)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[s]]`.
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
get_Xpars.trivial <- function(pars, i=1) {
  with(pars$Xpar[[i]], list(
    kappa=kappa,
    F_season=F_season,
    phase=phase,
    season_opts=season_opts,
    F_trend=F_trend,
    trend_opts=trend_opts
  ))
}



#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[s]]`.
#' @inheritParams set_Xpars
#' @return an **`xds`** object
#' @export
set_Xpars.trivial <- function(pars, i=1, Xopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$Xpar[[i]], with(Xopts,{
    pars$Xpar[[i]]$kappa = kappa
    pars$Xpar[[i]]$F_season = F_season
    pars$Xpar[[i]]$phase = phase
    pars$Xpar[[i]]$season_opts = season_opts
    pars$Xpar[[i]]$F_trend = F_trend
    pars$Xpar[[i]]$trend_opts = trend_opts
    return(pars)
  }))}

