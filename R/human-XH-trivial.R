
#' @title `trivialXH` --- **XH** module
#' 
#' @description
#' The trivial module sets up two trace functions: 
#' 1. a trace function to output net infectiousness, \eqn{\kappa}
#' 2. a trace function to output human population density, \eqn{H}
#' 
#' Basic setup uses a function if it is passed. If no function is passed, 
#' the functions are built from function objects (see [ramp.func::F_obj]) using [make_function],
#' which returns a function of the form \eqn{F(t,V).} 
#' 
#' The variables passed to the functions, \eqn{V}, are returned by  
#' [get_variables], which dispatches on the class of the stored function
#' object (by default, an empty list).
#' 
#' @section Net Infectiousness: 
#' 
#' The trace functions are set up as composed time series functions with 
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
#' The trace functions are set up as composed time series functions with 
#' four separate composable elements: 
#' \deqn{F_H(t) = H \times S(t, V_s) \times T(t, V_t) \times K(t, V_k)} 
#' where
#' + \eqn{\kappa} or `kappa` is the mean net infectiousness 
#' + \eqn{S(t,V_s)} or `H_season` is a seasonal pattern function
#' + \eqn{T(t,V_t)} or `H_trend` is a trend pattern function
#' + \eqn{K(t,V_k)} or `H_shock` is a perturbation function
#' 
#' @section Seasonal Pattern: 
#' 
#' Note: \eqn{0 \leq F_\kappa(t) \leq 1} 
#' 
#' @section Parameters:
#' 
#' **Net Infectivity**
#' \describe{
#'   \item{`kappa`}{mean net infectiousness}
#'   \item{`F_season`}{a seasonal pattern function, \eqn{{S(t,V_s)}}}
#'   \item{`F_trend`}{a trend function, \eqn{T(t,V_t)}}
#'   \item{`F_shock`}{a shock function, \eqn{K(t,V_k)}}
#'   \item{`season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#' }
#' 
#' **Human Population Density**
#' \describe{
#'   \item{`H`}{human population density}
#'   \item{`H_season`}{a seasonal pattern function, \eqn{{S(t,V_s)}}}
#'   \item{`H_trend`}{a trend function, \eqn{T(t,V_t)}}
#'   \item{`H_shock`}{a shock function, \eqn{K(t,V_k)}}
#'   \item{`H_season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`H_trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`H_shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#' }
#' Setup also adds the objects `season_par` and `trend_par` and `shock_par` for
#' use by `ramp.func`
#' 
#' @section Get and Change:
#' 
#' + `get_XH_pars` --- the `trivialXH` method returns all the parameters
#' + `change_XH_pars` --- change parameters by name 
#' 
#' @section Notes:
#' 
#' 1. The module has no state variables.
#'   
#' 2. The size of an object saved by `saveRDS` balloons if it saves a function,
#' so `saveXDS` function strips the functions and `readXDS` remakes the function
#' from the stored function object using [make_function].
#' 
#' 3. `F_season`, `F_trend`, and `F_shock` can be set up manually by passing any
#' user defined function. If so, the user should use `saveRDS` and `readRDS` 
#' rather than `saveXDS` and `readXDS` 
#' 
#' 4. Setup expects that `membership= c(1:nPatches),` but any membership vector 
#' works.
#' 
#'
#' @name trivialXH
#' @rdname trivialXH
NULL

# specialized methods for a human trivial model

#' @title The **XH** module skill set
#'
#' @description The **XH** skill set is a list of
#' a module's capabilities
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
   dts = FALSE,
   xde = TRUE,
   delay = FALSE,
   not_implemented = TRUE
  )
  return(skills)
}

#' @rdname get_skillset_XH.trivialXH
#' @export
get_skillset_XH.trivial = function(xds_obj, i){
  get_skillset_XH.trivialXH(xds_obj, i)
}

#' Check / update before solving
#'
#' @inheritParams check_XH
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_XH.trivialXH = function(xds_obj, i){
  return(xds_obj)
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

#' @title Make parameters for trivial human model
#' 
#' @description 
#' This builds a trace function as a *composed time series function* using either functions or parameters:
#' + Use a function if passed, and set the function object to an empty list
#' + If no function is passed, the parameters should be a function object (see [ramp.func::F_obj])
#' + Use `make_function` to create a function \eqn{F(t,V)}
#' 
#' @seealso [ramp.func::F_obj]
#' @param nPatches the number of patches
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
                                H_shock_par = list(name = "F_one")){
  with(options,{
    XH_obj <- list()
    class(XH_obj) <- c('trivialXH')
    
    XH_obj$H = checkIt(H, nPatches)
    XH_obj$kappa= checkIt(kappa, nPatches)
    
    if(length(F_season)>1){
      XH_obj$season_par = list()
      XH_obj$F_season = F_season  
    } else {
      XH_obj$season_par = season_par
      XH_obj$F_season = make_function(season_par)  
    }
    
    if(length(F_trend)>1){
      XH_obj$trend_par = list()
      XH_obj$F_trend = F_trend  
    } else {
      XH_obj$trend_par = trend_par
      XH_obj$F_trend = make_function(trend_par)  
    }

    if(length(F_shock)>1){
      XH_obj$shock_par = list() 
      XH_obj$F_shock = F_shock  
    } else {
      XH_obj$shock_par = shock_par
      XH_obj$F_shock = make_function(shock_par)  
    }

    if(length(H_season)>1){
      XH_obj$H_season_par = list()
      XH_obj$H_season = H_season  
    } else {
      XH_obj$H_season_par = H_season_par
      XH_obj$H_season = make_function(H_season_par)  
    }
    
    if(length(H_trend)>1){
      XH_obj$H_trend_par = list()
      XH_obj$H_trend = H_trend  
    } else {
      XH_obj$H_trend_par = H_trend_par
      XH_obj$H_trend = make_function(H_trend_par)  
    }
    
    if(length(H_shock)>1){
      XH_obj$H_shock_par = list() 
      XH_obj$H_shock = H_shock  
    } else {
      XH_obj$H_shock_par = H_shock_par
      XH_obj$H_shock = make_function(H_shock_par)  
    }
    
    return(XH_obj)
})}

#' @title Compute derivatives for `trivialXH` (**XH**)
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' @inheritParams dXHdt
#' @return a [numeric] vector
#' @keywords internal
#' @export
dXHdt.trivialXH <- function(t, y, xds_obj, i) {
  numeric(0)
}

#' @title Handle state updating for the `trivialXH` **X**-module
#' @description The trivial model has no state variables so it returns
#' a numeric vector of length 0
#' @inheritParams Update_XHt
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_XHt.trivialXH <- function(t, y, xds_obj, i) {
  numeric(0)
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

#' @title Compute the NI
#' @description Implements [F_ni] for the trivial model.
#' @inheritParams F_ni
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_ni.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_pfpr_by_lm.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_pfpr_by_rdt.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}


#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_prevalence] for the trivial model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector numeric(0)
#' @keywords internal
#' @export
F_pfpr_by_pcr.trivialXH <- function(vars, XH_obj) {
  return(numeric(0))
}

#' @title Set up `trivialXH` (**XH**)
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
  xds_obj$XH_obj[[i]] = make_XH_obj_trivial(xds_obj$nPatches, options)
  xds_obj <- setup_XH_inits(xds_obj, options=options, i=i)
  xds_obj$XH_obj[[i]]$skill_set <- get_skillset_XH(xds_obj, i)
  xds_obj <- setup_timespent("setup", xds_obj, list(residence=residence), i)
  xds_obj <- setup_blood_search_weights("setup", xds_obj, i=i)
  xds_obj <- setup_time_away("setup", xds_obj, i=i)
  xds_obj <- setup_travel_eir("no_travel", xds_obj, i=i)
  return(xds_obj)
}

#' @rdname setup_XH_obj.trivialXH
#' @export
setup_XH_obj.trivial = function(XHname, residence, xds_obj, options=list(), i=1){
  setup_XH_obj.trivialXH(XHname, residence, xds_obj, options=options, i=i)
}


#' @title Setup initial values for `trivialXH` (**XH**)
#' @description The `trivialXH` module (**XH**) for the \eqn{i^{th}} species
#' has no dynamical variables requiring initial values.
#' This method returns the unmodified **`xds`** object; `options` is unused.
#' @inheritParams setup_XH_inits
#' @return a [list] vector
#' @keywords internal
#' @export
setup_XH_inits.trivialXH = function(xds_obj, options=list(), i=1){
  return(xds_obj)
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the trivial model.
#' @inheritParams setup_XH_ix
#' @return a [list]
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


#' @title List variables for `trivialXH` (**XH**)
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


#' @title Change parameters for `trivialXH` (**XH**)
#' @description For the `trivialXH` module (**XH**) for the \eqn{i^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#' @param xds_obj an **`xds`** model object
#' @return a [list]
#' @keywords internal
#' @export
get_XH_pars.trivialXH <- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]], list(
    kappa=kappa,
    F_season=F_season,
    F_trend=F_trend,
  ))
}

#' @title Get parameters for `trivialXH` (**XH**)
#' @description This method dispatches on the type of `xds_obj$XH_obj[[s]]`.
#' @inheritParams change_XH_pars
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_XH_pars.trivialXH <- function(xds_obj, options=list(), i=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$XH_obj[[i]]$kappa = kappa
    xds_obj$XH_obj[[i]]$F_season = F_season
    xds_obj$XH_obj[[i]]$F_trend = F_trend
    return(xds_obj)
}))}
