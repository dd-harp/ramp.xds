# X
# TODO: Complete the deferred trivialXH documentation/NULL-anchor and setup review.

#' @title `trivialXH` --- **XH** module
#' 
#' @description
#' The trivial module sets up two forcing functions:
#' 1. a forcing function to output net infectiousness, \eqn{\kappa}
#' 2. a forcing function to output human population density, \eqn{H}
#' 
#' Basic setup uses a function if it is passed. If no function is passed, 
#' the functions are built from function objects (see [ramp.func::F_obj]) using [make_function],
#' which returns a function of the form \eqn{F(t,V).} 
#' 
#' @note The variables passed to the functions, \eqn{V}, are returned by
#' [get_variables], which dispatches on the class of the stored function
#' object (by default, an empty list).
#' 
#' @section Net Infectiousness: 
#' 
#' The forcing functions are set up as composed time series functions with
#' four separate composable elements: 
#' \deqn{F_\kappa(t) = \kappa \times S(t, V_s) \times T(t, V_t) \times K(t, V_k)} 
#' where
#' + \eqn{\kappa} or `kappa` is the mean net infectiousness 
#' + \eqn{S(t,V_s)} or `F_season` is a seasonal pattern function
#' + \eqn{T(t,V_t)} or `F_trend` is a trend pattern function
#' + \eqn{K(t,V_k)} or `F_shock` is a perturbation function
#' 
#' @section Human Population Density: 
#' 
#' The forcing functions are set up as composed time series functions with
#' four separate composable elements: 
#' \deqn{F_H(t) = H \times S(t, V_s) \times T(t, V_t) \times K(t, V_k)} 
#' where
#' + \eqn{H} or `H` is the mean human population density
#' + \eqn{S(t,V_s)} or `H_season` is a seasonal pattern function
#' + \eqn{T(t,V_t)} or `H_trend` is a trend pattern function
#' + \eqn{K(t,V_k)} or `H_shock` is a perturbation function
#' 
#' @section Seasonal Pattern: 
#' 
#' Note: \eqn{0 \leq F_\kappa(t) \leq 1} 
#' 
#' @section Parameters:
#' Test positivity `q` defaults to `0.8`.
#' 
#' **Net Infectiousness**
#' \describe{
#'   \item{`kappa`}{mean net infectiousness}
#'   \item{`F_season`}{a seasonal pattern function, \eqn{{S(t,V_s)}}}
#'   \item{`F_trend`}{a trend function, \eqn{T(t,V_t)}}
#'   \item{`F_shock`}{a shock function, \eqn{K(t,V_k)}}
#' }
#' 
#' **Human Population Density**
#' \describe{
#'   \item{`H`}{human population density}
#'   \item{`H_season`}{a seasonal pattern function, \eqn{{S(t,V_s)}}}
#'   \item{`H_trend`}{a trend function, \eqn{T(t,V_t)}}
#'   \item{`H_shock`}{a shock function, \eqn{K(t,V_k)}}
#' }
#' By default, `season_par`, `trend_par`, and `shock_par` configure
#' constant-one forcing functions for net infectiousness. The corresponding
#' `H_season_par`, `H_trend_par`, and `H_shock_par` configure constant-one
#' forcing functions for human population density.
#'
#' @section Prevalence:
#' [F_pr_obs.trivialXH] returns an empty numeric vector, as does
#' [F_prevalence.trivialXH], because this module has no infection states.
#'
#' @section Get and Change:
#' 
#' + `get_XH_pars` --- the `trivialXH` method returns all the parameters
#' + `change_XH_pars` --- change parameters by name 
#' 
#' @note
#' \describe{
#'   \item{`season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#'   \item{`H_season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`H_trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`H_shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#' }
#'
#' @note
#' 
#' 1. The module has no state variables.
#'   
#' 2. The size of an object saved by `saveRDS` balloons if it saves a function,
#' so the `saveXDS` function strips the functions and `readXDS` remakes the function
#' from the stored function object using [make_function].
#' 
#' 3. `F_season`, `F_trend`, `F_shock`, `H_season`, `H_trend`, and `H_shock`
#' can be set up manually by passing any
#' user-defined function. If so, the user should use `saveRDS` and `readRDS`
#' rather than `saveXDS` and `readXDS`.
#' 
#' 4. Setup uses a `residence` vector. The module constructs `H` and `kappa`
#' with length `nStrata[i]`.
#' 
#'
#' @name trivialXH
#' @rdname trivialXH
NULL

#' @title Compute the derivatives for `trivialXH` (**XH**)
#' @description Return an empty numeric vector because the `trivialXH` module has no state variables.
#' @inheritParams dXHdt
#' @return a [numeric] vector
#' @keywords internal
#' @export
dXHdt.trivialXH <- function(t, y, xds_obj, i) {
  numeric(0)
}

#' @title Update states for `trivialXH` (**XH**)
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' @inheritParams Update_XHt
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_XHt.trivialXH <- function(t, y, xds_obj, i) {
  numeric(0)
}


#' @title Get the state variables for `trivialXH` (**XH**)
#'
#' @description Return an empty list
#'
#' @inheritParams get_XH_vars
#' @return a [list]
#' @keywords internal
#' @export
get_XH_vars.trivialXH<- function(y, xds_obj, i) {
    return(list())
}

#' @title Size of effective infectious human population
#' @description Implements [F_I] for the trivial model
#' @inheritParams F_I
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_I.trivialXH <- function(t, y, xds_obj, i) {
  H = F_H(t, y, xds_obj, i)
  with(xds_obj$XH_obj[[i]],{
    V_s = get_variables(season_par, t, y, xds_obj, i)
    V_t = get_variables(trend_par, t, y, xds_obj, i)
    V_k = get_variables(shock_par, t, y, xds_obj, i)
    X = H*kappa*F_season(t, V_s)*F_trend(t, V_t)*F_shock(t, V_k)
    return(X)
})}

#' @title Size of the human population
#' @description Implements [F_H] for the trivial model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_H.trivialXH <- function(t, y, xds_obj, i) {
  H = with(xds_obj$XH_obj[[i]],  H)
  with(xds_obj$XH_obj[[i]],{
    V_s = get_variables(H_season_par, t, y, xds_obj, i)
    V_t = get_variables(H_trend_par, t, y, xds_obj, i)
    V_k = get_variables(H_shock_par, t, y, xds_obj, i)
    X = H*H_season(t, V_s)*H_trend(t, V_t)*H_shock(t, V_k)
  return(X) 
})}

#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_infectivity] for the trivial model.
#' @inheritParams F_infectivity
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_infectivity.trivialXH <- function(y, xds_obj, i) {
  return(1)
}

#' @title Make the module object for `trivialXH` (**XH**)
#' 
#' @description 
#' This builds a forcing function as a *composed time series function* using either functions or parameters:
#' + Use a function if passed, and set the function object to an empty list
#' + If no function is passed, the parameters should be a function object (see [ramp.func::F_obj])
#' + Use `make_function` to create a function \eqn{F(t,V)}
#' 
#' @seealso [ramp.func::F_obj]
#' @param nPatches the number of population strata (legacy argument name)
#' @param options a [list]
#' @param kappa net infectiousness (mean value)
#' @param H initial human population density
#' @param F_season the seasonal pattern function
#' @param F_trend the trend function
#' @param F_shock the shock function
#' @param season_par a list of options for F_season
#' @param trend_par a list of options for F_trend
#' @param shock_par a list of options for F_shock
#' @param H_season the seasonal pattern function for \eqn{H}
#' @param H_trend the trend function for \eqn{H}
#' @param H_shock the shock function for \eqn{H}
#' @param H_season_par a list of options for H_season
#' @param H_trend_par a list of options for H_trend
#' @param H_shock_par a list of options for H_shock
#' 
#' @return a [list]
#' @keywords internal
#' @param q test positivity; a numeric scalar or input of length `nPatches`
#' @export
make_XH_obj_trivial <- function(nPatches, options,
                                kappa=0, 
                                H=1,
                                F_season = list(), 
                                F_trend = list(), 
                                F_shock = list(),
                                season_par = list(name = "F_one"),
                                trend_par = list(name = "F_one"),
                                shock_par = list(name = "F_one"), 
                                H_season = list(), 
                                H_trend = list(), 
                                H_shock = list(),
                                H_season_par = list(name = "F_one"),
                                H_trend_par = list(name = "F_one"),
                                H_shock_par = list(name = "F_one"), q=0.8){
  with(options,{
    XH_obj <- list()
    class(XH_obj) <- c('trivialXH')
    
    XH_obj$H = checkIt(H, nPatches)
    XH_obj$kappa= checkIt(kappa, nPatches)
    
    XH_obj <- make_ts_obj(XH_obj, list(
      F_season=F_season, F_trend=F_trend, F_shock=F_shock,
      season_par=season_par, trend_par=trend_par, shock_par=shock_par,
      H_season=H_season, H_trend=H_trend, H_shock=H_shock,
      H_season_par=H_season_par, H_trend_par=H_trend_par,
      H_shock_par=H_shock_par
    ))
    
    XH_obj$q = checkIt(q, nPatches)

    return(XH_obj)
})}

#' @title Set up the module object for `trivialXH` (**XH**)
#' @description
#'
#' This sets up a `trivialXH` model object for the **XH** module for the \eqn{i^{th}} host species
#' + the object is created by [make_XH_obj_trivial]
#' + `options` is passed to override the defaults
#'
#' There are no dynamical variables requiring initial values;
#' [setup_XH_inits] leaves the object unmodified.
#'
#' @seealso [trivialXH], [make_XH_obj_trivial]
#'
#' @note The module object is stored at `xds_obj$XH_obj[[i]]`
#' The compatibility name `"trivial"` creates the same `trivialXH` object.
#'
#' @inheritParams setup_XH_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_XH_obj.trivialXH = function(XHname, residence, xds_obj, options=list(), i=1){
  xds_obj$XHname = "trivialXH"
  xds_obj$XH_obj[[i]] = make_XH_obj_trivial(xds_obj$nStrata[i], options)
  xds_obj <- setup_XH_inits(xds_obj, options=options, i=i)
  xds_obj <- setup_timespent("setup", xds_obj, list(residence=residence), i)
  xds_obj <- setup_blood_search_weights("setup", xds_obj, i=i)
  xds_obj <- setup_time_away("setup", xds_obj, i=i)
  xds_obj <- setup_travel_eir("no_travel", xds_obj, i=i)
  return(xds_obj)
}

#' @inheritParams setup_XH_obj
#' @rdname setup_XH_obj.trivialXH
#' @export
setup_XH_obj.trivial = function(XHname, residence, xds_obj, options=list(), i=1){
  setup_XH_obj.trivialXH(XHname, residence, xds_obj, options=options, i=i)
}


#' @title Get parameters for `trivialXH` (**XH**)
#' @description Return stored parameter values for the `trivialXH` module.
#' @inheritParams get_XH_pars
#' @return a [list]
#' @keywords internal
#' @export
get_XH_pars.trivialXH <- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]], list(q=q,
    kappa=kappa,
    season_par=season_par,
    trend_par=trend_par,
    shock_par=shock_par,
    H=H,
    H_season_par=H_season_par,
    H_trend_par=H_trend_par,
    H_shock_par=H_shock_par
  ))
}

#' @title Change parameters for `trivialXH` (**XH**)
#' @description Change parameter values named in `options`,
#' leaving other parameter values unchanged. Rebuild only forcing functions
#' whose parameters are named in `options`; an empty parameter list resets
#' the corresponding function to constant one.
#' @inheritParams change_XH_pars
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_XH_pars.trivialXH <- function(xds_obj, options=list(), i=1) {
  for(par in intersect(c("kappa", "H", "q"), names(options)))
    xds_obj$XH_obj[[i]][[par]] = options[[par]]

  forcing_functions = c(season_par="F_season", trend_par="F_trend",
                        shock_par="F_shock", H_season_par="H_season",
                        H_trend_par="H_trend", H_shock_par="H_shock")
  for(par in intersect(names(forcing_functions), names(options))){
    xds_obj$XH_obj[[i]][[par]] = options[[par]]
    xds_obj$XH_obj[[i]][[forcing_functions[[par]]]] = make_function(options[[par]])
  }
  return(xds_obj)
}


#' @title Set up initial values for `trivialXH` (**XH**)
#' @description The `trivialXH` module (**XH**) for the \eqn{i^{th}} species
#' has no dynamical variables requiring initial values.
#' This method returns the unmodified **`xds`** object; `options` is unused.
#' @inheritParams setup_XH_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_XH_inits.trivialXH = function(xds_obj, options=list(), i=1){
  return(xds_obj)
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the trivial model.
#' @inheritParams setup_XH_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @export
setup_XH_ix.trivialXH <- function(xds_obj, i) {
  
  return(xds_obj)
}

#' @title Parse outputs for `trivialXH` (**XH**)
#' @description Implements [parse_XH_orbits] for the trivial model
#' @inheritParams parse_XH_orbits
#' @return a [list]
#' @keywords internal
#' @export
parse_XH_orbits.trivialXH <- function(outputs, xds_obj,i) {
  return(list())
}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_prevalence.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Compute observed prevalence
#' @description Return observed prevalence for the `trivialXH` module as
#' `q` times the true prevalence returned by [F_prevalence].
#' True prevalence is empty for this module, so observed prevalence is empty too.
#' @inheritParams F_pr_obs
#' @return an empty [numeric] vector
#' @seealso [trivialXH]
#' @keywords internal
#' @export
F_pr_obs.trivialXH <- function(vars, XH_obj) {
  XH_obj$q * F_prevalence(vars, XH_obj)
}

#' @title Compute the NI
#' @description Implements [F_ni] for the trivial model.
#' @inheritParams F_ni
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_ni.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Compute observed prevalence by light microscopy
#' @description Implements [F_pfpr_by_lm] for the trivial model.
#' @inheritParams F_pfpr_by_lm
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_pfpr_by_lm.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title Compute observed prevalence by RDT
#' @description Implements [F_pfpr_by_rdt] for the trivial model.
#' @inheritParams F_pfpr_by_rdt
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_pfpr_by_rdt.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title Compute observed prevalence by PCR
#' @description Implements [F_pfpr_by_pcr] for the trivial model.
#' @inheritParams F_pfpr_by_pcr
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_pfpr_by_pcr.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Check `trivialXH` (**XH**)
#'
#' @inheritParams check_XH
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_XH.trivialXH = function(xds_obj, t=0, i){
  return(xds_obj)
}

# specialized methods for a human trivial model

#' @title Get the skill set for `trivialXH` (**XH**)
#'
#' @description Return the capabilities and compatibilities of the `trivialXH` module as a list.
#'
#'
#' @inheritParams get_skillset_XH
#'
#' @note The compatibility name `"trivial"` returns the same skill set as
#' `"trivialXH"`.
#'
#' @return the **XH** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_XH.trivialXH = function(xds_obj, i){
  skills = list(
    dts         = FALSE,
    xde         = TRUE,
    delay       = FALSE,
    demography  = FALSE,
    malaria     = FALSE,
    pr_obs      = TRUE,
    mda         = FALSE,
    msat        = FALSE
  )
  return(skills)
}

#' @inheritParams get_skillset_XH
#' @rdname get_skillset_XH.trivialXH
#' @export
get_skillset_XH.trivial = function(xds_obj, i){
  get_skillset_XH.trivialXH(xds_obj, i)
}
