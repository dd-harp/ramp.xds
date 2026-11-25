# Y
# TODO: Complete the deferred trivialL documentation/NULL-anchor and setup review.
# specialized methods for the aquatic mosquito trivial model

#' @title The trivial module for the L component
#' @description
#' The trivial module outputs the emergence 
#' rate of adult mosquitoes by calling a
#' *forcing function*:
#' \deqn{F_\alpha(t) = \Lambda \times F_S(t, V_s) \times F_T(t, V_t) \times F_K(t, V_k)} 
#' where
#' 
#' + \eqn{\Lambda} is the mean daily emergence rate of adult mosquitoes from each aquatic habitat
#' + \eqn{F_S(t,V_s)} or `F_season` is a seasonal pattern function
#' + \eqn{F_T(t,V_t)} or `F_trend` is a trend pattern function
#' + \eqn{F_K(t,V_k)} or `F_shock` is a perturbation function
#' 
#' @note The variables \eqn{V_s}, \eqn{V_t}, and \eqn{V_k} are
#' called by [get_variables], which dispatches on the class of 
#' `season_par` or `trend_par` or `shock_par`.
#' 
#' 
#' @section Parameters:
#' \describe{
#'   \item{`Lambda`}{the mean daily emergence rate of adult mosquitoes from each aquatic habitat}
#'   \item{`F_season`}{a seasonal pattern function, \eqn{{F_S(t,V_s)}}}
#'   \item{`F_trend`}{a trend function, \eqn{F_T(t,V_t)}}
#'   \item{`F_shock`}{a shock function, \eqn{F_K(t,V_k)}}
#' }
#'
#' @note
#' \describe{
#'   \item{`season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#' }
#'  
#' By default, `season_par`, `trend_par`, and `shock_par` configure
#' constant-one forcing functions.
#' 
#' @seealso
#' 
#' + [get_L_pars], [get_mean_forcing], [get_season], [get_trend], [get_shock]
#' + [show_season], [show_trend], [show_shock]
#' + [change_L_pars], [change_mean_forcing], [change_season], [change_trend], [change_shock]
#' 
#' @note
#' 
#' 1. The module has no state variables.
#'   
#' 2. The size of an object saved by `saveRDS` balloons if it saves a function,
#' so the `saveXDS` function strips the functions and `readXDS` remakes the functions
#' from the stored parameters.
#' `F_season`, `F_trend`, and `F_shock` can be set up manually by passing any
#' user-defined function. If so, the user should use `saveRDS` and `readRDS`
#' rather than `saveXDS` and `readXDS` 
#' 
#' 3. Setup expects that `membership= c(1:nPatches)`, but any membership vector
#' works.  
#' 
#'
#' @name trivialL
NULL

#' @title Compute the derivatives for `trivialL` (**L**)
#' @description Return an empty numeric vector because the `trivialL` module has no state variables.
#' @inheritParams dLdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dLdt.trivialL <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Update states for `trivialL` (**L**)
#' @description Returns a numeric vector of length 0
#' @inheritParams Update_Lt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_Lt.trivialL <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Get the state variables for `trivialL` (**L**)
#' @description Returns an empty list; the `trivialL` module has no state variables
#' @inheritParams get_L_vars
#' @return an empty [list]
#' @keywords internal
#' @export
get_L_vars.trivialL <- function(y, xds_obj, s){
  return(list())
}

#' @title Mosquito bionomics for `trivialL` (**L**)
#' @description Return the **`xds`** object unmodified 
#' @inheritParams LBionomics
#' @return an **`xds`** model object
#' @keywords internal
#' @export
LBionomics.trivialL <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `trivialL` (**L**)
#' @description Return the **`xds`** object unmodified 
#' @inheritParams LEffectSizes
#' @return an **`xds`** model object
#' @keywords internal
#' @export
LEffectSizes.trivialL <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Compute emergent adults for `trivialL` (**L**)
#' @description The number of emerging adults is a function \deqn{\Lambda F_S(t) F_T(t) F_K(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging from each aquatic habitat per day
#' + \eqn{F_S(t)} or `F_season` is a seasonal signal (ideally, with an average annual mean of 1)
#' + \eqn{F_T(t)} or `F_trend` is a function returning a trend (ideally, with an average value of 1)
#' + \eqn{F_K(t)} or `F_shock` is a function describing a perturbation (by default, set to 1)
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
F_emerge.trivialL <- function(t, y, xds_obj, s) {
  with(xds_obj$L_obj[[s]],{
    V_s = get_variables(season_par, t, y, xds_obj, s)
    V_t = get_variables(trend_par, t, y, xds_obj, s)
    V_k = get_variables(shock_par, t, y, xds_obj, s)
    return(Lambda*F_season(t, V_s)*F_trend(t, V_t)*F_shock(t, V_k))
})}


#' @title Make the module object for `trivialL` (**L**)
#' @description The number of emerging adults is a function \deqn{\Lambda F_S(t) F_T(t) F_K(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging from each aquatic habitat per day
#' + \eqn{F_S(t)} or `F_season` is a seasonal pattern function (ideally, with an average annual mean of 1)
#' + \eqn{F_T(t)} or `F_trend` is a trend pattern function (ideally, with an average value of 1)
#' + \eqn{F_K(t)} or `F_shock` is a perturbation function
#'
#' Use a supplied pattern function if passed, setting its corresponding
#' parameter object to an empty list. Otherwise, construct the function from
#' the corresponding parameter object using [make_function].
#' @param nHabitats the number of habitats in the model
#' @param options a [list] that overrides default values
#' @param Lambda vector of mean emergence rates from each aquatic habitat
#' @param F_season the seasonal pattern function
#' @param F_trend the trend function
#' @param F_shock the shock function
#' @param season_par a list of options for F_season
#' @param trend_par a list of options for F_trend
#' @param shock_par a list of options for F_shock
#' @return a [list]: an L module object 
#' @keywords internal
#' @export
make_L_obj_trivial = function(nHabitats, options=list(),
                             Lambda=100,
                             F_season = list(),
                             F_trend = list(),
                             F_shock = list(),
                             season_par = list(name = "F_one"),
                             trend_par = list(name = "F_one"),
                             shock_par = list(name = "F_one")){
  with(options,{
    L_obj = list()
    class(L_obj) <- "trivialL"
    L_obj$Lambda = checkIt(Lambda, nHabitats)

    L_obj <- make_ts_obj(L_obj, list(
      F_season=F_season, F_trend=F_trend, F_shock=F_shock,
      season_par=season_par, trend_par=trend_par, shock_par=shock_par
    ))
    
    return(L_obj)
})}

#' @title Set up the module object for `trivialL` (**L**)
#' @description
#'
#' This sets up a `trivialL` model object for the **L** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_L_obj_trivial]
#' + `options` is passed to override the defaults
#'
#' There are no dynamical variables requiring initial values.
#' @note This method sets `class(xds_obj$forced_by) = "Lambda"`.
#'
#' @seealso [trivialL], [make_L_obj_trivial]
#'
#' @note The module object is stored at `xds_obj$L_obj[[s]]`
#' The compatibility name `"trivial"` creates the same `trivialL` object.
#'
#' @inheritParams setup_L_obj
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_L_obj.trivialL = function(Lname, membership, xds_obj, options=list(), s=1){
  xds_obj$Lname = "trivialL"
  forced_by = list()
  forced_by$what = "Lambda"
  class(forced_by) = "Lambda"
  xds_obj$forced_by = forced_by
  xds_obj$L_obj[[s]] = make_L_obj_trivial(length(membership), options)
  if(length(membership)!= xds_obj$nHabitats) xds_obj <- setup_habitats(xds_obj, membership)
  xds_obj <- setup_habitat_search_weights("setup", xds_obj, options=list(membership=membership), s=s)
  return(xds_obj)
}

#' @inheritParams setup_L_obj
#' @rdname setup_L_obj.trivialL
#' @export
setup_L_obj.trivial = function(Lname, membership, xds_obj, options=list(), s=1){
  setup_L_obj.trivialL(Lname, membership, xds_obj, options=options, s=s)
}


#' @title Get parameters for `trivialL` (**L**)
#' @description Get \eqn{\Lambda} and parameters that construct
#' the forcing functions
#' @inheritParams get_L_pars
#' @return a [list]
#' @keywords internal
#' @export
get_L_pars.trivialL <- function(xds_obj, s=1) {
  with(xds_obj$L_obj[[s]], list(
    Lambda=Lambda,
    season_par=season_par,
    trend_par=trend_par,
    shock_par=shock_par
  ))
}

#' @title Change parameters for `trivialL` (**L**)
#'
#' @description For the `trivialL` module (**L**) for the \eqn{s^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#'
#' Parameters: `Lambda`, `F_season`, `F_trend`, or `F_shock`.
#'
#' @inheritParams change_L_pars
#' 
#' @note Utilities for forcing functions are in `ramp.func`
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @details After the updates, call [LBionomics] at time zero with the flattened
#' initial state. Do not call [LEffectSizes]; bionomics is a no-op for this module.
change_L_pars.trivialL <- function(xds_obj, options=list(), s=1) {
  with(xds_obj$L_obj[[s]], with(options,{
    xds_obj$L_obj[[s]]$Lambda = Lambda
    xds_obj$L_obj[[s]]$F_season = F_season
    xds_obj$L_obj[[s]]$F_trend = F_trend
    xds_obj$L_obj[[s]]$F_shock = F_shock
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- LBionomics(0, y, xds_obj, s)
    return(xds_obj)
  }))}


#' @title Set up initial values for `trivialL` (**L**)
#' @description The `trivialL` module (**L**) for the \eqn{s^{th}} species
#' has no dynamical variables requiring initial values.
#' This method sets the initial values to an empty list; `options` is unused.
#' @inheritParams setup_L_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_L_inits.trivialL = function(xds_obj, options=list(), s=1){
  xds_obj$L_obj[[s]]$inits = list()
  return(xds_obj)
}


#' @title Change initial values for `trivialL` (**L**)
#' @description The `trivialL` module has no state variables. Return the
#' **`xds`** object unmodified; `options` is unused.
#' @inheritParams change_L_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_L_inits.trivialL <- function(xds_obj, options=list(), s=1) {
  return(xds_obj)
}

#' @title Set up variable indices for `trivialL` (**L**)
#' @description Return the **`xds`** object unmodified 
#' @inheritParams setup_L_ix
#' @return an **`xds`** model object
#' @keywords internal
#' @seealso [trivialL]
#' @export
setup_L_ix.trivialL <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title Parse outputs for `trivialL` (**L**)
#' @description Returns an empty list
#' @inheritParams parse_L_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_L_orbits.trivialL <- function(outputs, xds_obj, s) {
  return(list())
}

#' @title Check `trivialL` (**L**)
#' @description Run no consistency checks 
#' @inheritParams check_L
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_L.trivialL = function(xds_obj, t=0, s){
  return(xds_obj)
}

#' @title Get the skill set for `trivialL` (**L**)
#'
#' @description Return the capabilities and compatibilities of the `trivialL` module as a list.
#'
#' @inheritParams get_skillset_L
#'
#' @return the **L** module skill set, as a list
#'
#' @note The compatibility name `"trivial"` returns the same skill set as
#' `"trivialL"`.
#'
#' @keywords internal
#' @export
get_skillset_L.trivialL = function(xds_obj, s){
  skills = list(
    dts = FALSE,
    xde = TRUE,
    delay = FALSE
  )
  return(skills)
}

#' @inheritParams get_skillset_L
#' @rdname get_skillset_L.trivialL
#' @export
get_skillset_L.trivial = function(xds_obj, s){
  get_skillset_L.trivialL(xds_obj, s)
}
