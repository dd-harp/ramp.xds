# specialized methods for a human trivial model

#' @title The **XH** Module Skill Set 
#' 
#' @description The **XH** skill set is a list of 
#' an module's capabilities. The relevant documentation 
#' for setup is in [make_XH_obj_trivial]
#' 
#'
#' @inheritParams skill_set_XH  
#' 
#' @return the trivial *XH* module skill set, a list 
#' 
#' @export
skill_set_XH.trivial = function(Xname){
  list(
    H_dynamics = TRUE, 
    mda        = TRUE, 
    msat       = TRUE, 
    malaria    = TRUE, 
    pr_obs     = TRUE, 
    pf_lm      = FALSE, 
    pf_rdt     = FALSE, 
    pf_pcr     = FALSE
  ) 
}

#' Check / update before solving 
#'
#' @inheritParams check_XH
#'
#' @returns an **`xds`** model object 
#' @export
check_XH.trivial = function(xds_obj, i){
  return(xds_obj)
}

#' @title Size of effective infectious human population
#' 
#' @description The trace function for \eqn{\kappa} follows
#' the **`ramp.xds`** standard for time series forcing. The
#' trace function for \eqn{H} can be modified to follow a
#' trend. Here, \eqn{I = \kappa H}. 
#' 
#' @inheritParams F_I
#' 
#' @return a [numeric] vector of length `nStrata`
#' @export
F_I.trivial <- function(t, y, xds_obj, i) {
  H = F_H(t, y, xds_obj, i)
  I = with(xds_obj$XH_obj[[i]],  H*kappa*F_season(t)*F_trend(t)*F_shock(t))
  return(I)
}

#' @title Size of the human population
#' @description Implements [F_H] for the trivial model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.trivial <- function(t, y, xds_obj, i) {
  H = with(xds_obj$XH_obj[[i]],  H*H_trend(t))
  return(H) 
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_infectivity] for the trivial model.
#' @inheritParams F_infectivity
#' @return a [numeric] vector of length `nStrata`
#' @export
F_infectivity.trivial <- function(y, xds_obj, i) {
  return(1)
}

#' @title Make parameters for trivial human model
#' 
#' @description The trivial module configures forcing with a seasonal pattern
#' 
#' + \eqn{H = F_H(t)} - human / host population density 
#' + \eqn{I = F_I(t)} - human / host infectious density 
#' 
#' Since \eqn{\kappa} is the ratio \eqn{I/H} 
#'  
#' @param nPatches the number of patches
#' @param options a [list]
#' @param kappa net infectiousness
#' @param HPop initial human population density
#' @param F_season a function describing a seasonal pattern 
#' @param season_par parameters to configure a `F_season` using [make_function]
#' @param F_trend a function describing a temporal trend 
#' @param trend_par parameters to configure `F_trend` using [make_function]
#' @param F_shock a function describing a temporal shock 
#' @param shock_par parameters to configure `F_shock` using [make_function]
#' @param H_trend a function describing a temporal trend in human population density
#' @param H_trend_par parameters to configure `H_trend` using [make_function]
#' @return a [list]
#' @export
make_XH_obj_trivial <- function(nPatches, options, kappa=.1, HPop=1,
                              F_season=F_flat, season_par = list(), 
                              F_trend=F_flat, trend_par = list(), 
                              F_shock=F_flat, shock_par = list(), 
                              H_trend=F_flat, H_trend_par = list()){
  with(options,{
    XH_obj <- list()
    class(XH_obj) <- c('trivial')
    XH_obj$H = checkIt(HPop, nPatches)
    XH_obj$kappa= checkIt(kappa, nPatches)
    
    XH_obj$F_season = F_season
    XH_obj$season_par <- season_par
    if(length(season_par)>0)
      XH_obj$F_season <- make_function(season_par) 
    
    XH_obj$F_trend = F_trend
    XH_obj$trend_par <- trend_par
    if(length(trend_par)>0)
      XH_obj$F_trend <- make_function(trend_par) 

    XH_obj$F_shock = F_shock
    XH_obj$shock_par <- shock_par
    if(length(shock_par)>0)
      XH_obj$F_shock <- make_function(shock_par) 
    
    XH_obj$H_trend = H_trend
    XH_obj$H_trend_par <- H_trend_par
    if(length(H_trend_par)>0)
      XH_obj$H_trend <- make_function(H_trend_par) 

    return(XH_obj)
  })}

#' @title Handle Derivatives for the `trivial` **XH**-module
#' 
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' 
#' @inheritParams dXHdt
#' @return a [numeric] vector
#' @export
dXHdt.trivial <- function(t, y, xds_obj, i) {
  numeric(0)
}

#' @title Handle State Updating for the `trivial` **X**-Module
#' 
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' 
#' @inheritParams Update_XHt
#' @return a [numeric] vector
#' @export
Update_XHt.trivial <- function(t, y, xds_obj, i) {
  numeric(0)
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @export
F_prevalence.trivial <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Compute the NI 
#' @description Implements [F_ni] for the trivial model.
#' @inheritParams F_ni
#' @return a [numeric] vector numeric(0)
#' @export
F_ni.trivial <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @export
F_pfpr_by_lm.trivial <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @export
F_pfpr_by_rdt.trivial <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @export
F_pfpr_by_pcr.trivial <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title xde_setup XH_obj.trivial
#' @description Implements [setup_XH_obj] for the trivial model
#' @inheritParams setup_XH_obj
#' @return a [list] vectord
#' @export
setup_XH_obj.trivial = function(Xname, xds_obj, i, options=list()){
  xds_obj$XH_obj[[i]] = make_XH_obj_trivial(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Setup Xinits.trivial
#' @description Implements [setup_XH_inits] for the trivial model
#' @inheritParams setup_XH_inits
#' @return a [list] vector
#' @export
setup_XH_inits.trivial = function(xds_obj, H, i=1, options=list()){
  return(xds_obj)
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the trivial model.
#' @inheritParams setup_XH_ix
#' @return none
#' @importFrom utils tail
#' @export
setup_XH_ix.trivial <- function(xds_obj, i) {
  return(xds_obj)
}

#' @title parse the output of deSolve and return variables for the trivial model
#' @description Implements [parse_XH_orbits] for the trivial model
#' @inheritParams parse_XH_orbits
#' @return none
#' @export
parse_XH_orbits.trivial <- function(outputs, xds_obj,i) {
  return(list())
}


#' @title Get Variables by Name 
#' 
#' @description Return an empty list 
#' 
#' @inheritParams get_XH_vars 
#' @return a [list]
#' @export
get_XH_vars.trivial<- function(y, xds_obj, i) {
    return(list())
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$XH_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [list]
#' @export
get_XH_pars.trivial <- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]], list(
    kappa=kappa,
    F_season=F_season,
    F_trend=F_trend,
    F_shock=F_shock,
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$XH_obj[[s]]`.
#' @inheritParams change_XH_pars
#' @return an **`xds`** object
#' @export
change_XH_pars.trivial <- function(xds_obj, i=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$XH_obj[[i]]$kappa = kappa
    xds_obj$XH_obj[[i]]$F_season = F_season
    xds_obj$XH_obj[[i]]$F_trend = F_trend
    xds_obj$XH_obj[[i]]$F_shock = F_shock
    return(xds_obj)
}))}

