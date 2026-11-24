# X
# TODO: Complete the deferred trivialMY documentation/NULL-anchor and setup review.

#' @title The trivial module for the MY component
#' 
#' @description
#' The trivial **MY** module configures one of two forcing functions, either:
#' + infectious biting:  
#' \deqn{F_{fqZ}(t, V) = fqZ \times F_S(t, V_s) \times F_T(t, V_t) \times F_K(t, V_k)} 
#' + egg laying: 
#' \deqn{F_G(t, V) = G \times F_S(t,V_s) \times F_T(t, V_t) \times F_K(t, V_k)} 
#'
#' where
#' 
#' + \eqn{G} or `eggs` is the mean egg laying rate 
#' + \eqn{fqZ} is the product of `f`, `q`, and `Z`, the mean number of infectious bites on humans, per patch, per day
#' + \eqn{F_S(t,V_s)} or `F_season` is a seasonal pattern function
#' + \eqn{F_T(t,V_t)} or `F_trend` is a trend pattern function
#' + \eqn{F_K(t,V_k)} or `F_shock` is a perturbation function
#' 
#' The implementation assumes that only one of these functions will be used, so
#' it is not possible to configure different pattern functions for egg laying and infective biting.
#'
#' @note The variables \eqn{V_s}, \eqn{V_t}, and \eqn{V_k} are
#' called by [get_variables], which dispatches on the class of
#' `season_par` or `trend_par` or `shock_par`.
#' 
#' @section Parameters:
#' \describe{
#'   \item{`eggs`}{the mean number of eggs laid, per patch, per day}
#'   \item{`Z`}{the mean density of infectious mosquitoes, per patch}
#'   \item{`f`}{the blood feeding rate}
#'   \item{`q`}{the human fraction}
#'   \item{`F_season`}{a seasonal pattern function, \eqn{{F_S(t,V_s)}}}
#'   \item{`F_trend`}{a trend function, \eqn{F_T(t,V_t)}}
#'   \item{`F_shock`}{a shock function, \eqn{F_K(t,V_k)}}
#' }
#' 
#' By default, `season_par`, `trend_par`, and `shock_par` configure
#' constant-one forcing functions.
#' 
#' For the bionomic parameters, `f=q=Z=eggs=1`. 
#' 
#' @seealso [make_MY_obj_trivial]
#' 
#' @note
#' \describe{
#'   \item{`season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#' }
#'
#' @note
#' 
#' 1. The module has no state variables.
#'   
#' 2. The size of an object saved by `saveRDS` balloons if it saves a function,
#' so the `saveXDS` function strips the functions and `readXDS` remakes the function
#' from the stored parameters.
#' 
#' `F_season`, `F_trend`, and `F_shock` can be set up manually by passing any
#' user-defined function. If so, the user should use `saveRDS` and `readRDS`
#' rather than `saveXDS` and `readXDS`.
#'
#' @name trivialMY
#' @rdname trivialMY
NULL


#' @title Compute the derivatives for `trivialMY` (**MY**)
#' @description Return an empty numeric vector because the `trivialMY` module has no state variables.
#' @inheritParams dMYdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dMYdt.trivialMY <- function(t, y, xds_obj, s){
  numeric(0)
}

#' @title Update states for `trivialMY` (**MY**)
#' @description Return an empty update vector because the `trivialMY` module has no state variables.
#' @inheritParams Update_MYt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_MYt.trivialMY <- function(t, y, xds_obj, s){
  numeric(0)
}

#' @title Get the state variables for `trivialMY` (**MY**)
#' @description Return an empty list because the `trivialMY` module has no state variables.
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.trivialMY <- function(y, xds_obj, s){
  return(list())
}


#' @title Mosquito bionomics for `trivialMY` (**MY**)
#' @description Implements [MBionomics] for the `trivialMY` module.
#' @inheritParams MBionomics
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.trivialMY <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `trivialMY` (**MY**)
#' @description Implements [MEffectSizes] for the `trivialMY` module.
#' @inheritParams MEffectSizes
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.trivialMY <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Infective mosquito biting rate for `trivialMY`
#' @description Returns 
#' \deqn{F_{fqZ}(t) = fqZ F_S(t) F_T(t) F_K(t)}
#' where 
#' + \eqn{f} is the feeding rate 
#' + \eqn{q} is the human fraction
#' + \eqn{Z} is the density of infectious mosquitoes, per patch 
#' + \eqn{F_S(t)} or `F_season` is a seasonal pattern 
#' + \eqn{F_T(t)} or `F_trend` is a trend pattern 
#' + \eqn{F_K(t)} or `F_shock` is a perturbation 
#' + `season_par` --- parameters used by [get_variables] to obtain \eqn{V_s}
#' + `trend_par` --- parameters used by [get_variables] to obtain \eqn{V_t}
#' + `shock_par` --- parameters used by [get_variables] to obtain \eqn{V_k}
#' 
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.trivialMY <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  with(xds_obj$MY_obj[[s]],{
    V_s = get_variables(season_par, t, y, xds_obj, s)
    V_t = get_variables(trend_par, t, y, xds_obj, s)
    V_k = get_variables(shock_par, t, y, xds_obj, s)
    Z =  Z*F_season(t, V_s)*F_trend(t, V_t)*F_shock(t, V_k)
    return(f*q*Z)
})}

#' @title Mosquito biting rate for `trivialMY`
#' @description The trivial model does not track an adult mosquito
#' population, so this method returns an empty numeric vector.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
F_fqM.trivialMY <- function(t, y, xds_obj, s){
  return(numeric(0))
}

#' @title Egg production rate for `trivialMY`
#' @description Returns 
#' \deqn{F_{G}(t) = G F_S(t) F_T(t) F_K(t)}
#' where 
#' + \eqn{G} is the number of eggs laid, per patch, per day 
#' + \eqn{F_S(t)} or `F_season` is a seasonal pattern 
#' + \eqn{F_T(t)} or `F_trend` is a trend pattern 
#' + \eqn{F_K(t)} or `F_shock` is a perturbation 
#' 
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.trivialMY <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    V_s = get_variables(season_par, t, y, xds_obj, s)
    V_t = get_variables(trend_par, t, y, xds_obj, s)
    V_k = get_variables(shock_par, t, y, xds_obj, s)
    return(eggs*F_season(t, V_s)*F_trend(t, V_t)*F_shock(t, V_k))
})}

#' @title Get the feeding rates
#' @inheritParams get_f
#' @return the feeding rate, as a vector
#' @keywords internal
#' @export
get_f.trivialMY = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @inheritParams get_q
#' @return the human fraction, as a vector
#' @keywords internal
#' @export
get_q.trivialMY = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @inheritParams get_g
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
get_g.trivialMY = function(xds_obj, s=1){
  numeric(0)
}

#' @title Get the patch emigration rates
#' @inheritParams get_sigma
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
get_sigma.trivialMY = function(xds_obj, s=1){
  numeric(0)
}


#' @title Make the module object for `trivialMY` (**MY**)
#' @description Use a supplied pattern function if passed, setting its corresponding
#' parameter object to an empty list. Otherwise, construct the function from
#' the corresponding parameter object using [make_function].
#' @param nPatches an integer
#' @param options a [list] of values that overrides the defaults
#' @param f the blood feeding rate
#' @param q the human fraction
#' @param Z the density of infectious mosquitoes
#' @param eggs the mean egg laying rate
#' @param F_season the seasonal pattern function
#' @param F_trend the trend function
#' @param F_shock the shock function
#' @param season_par a list of options for F_season
#' @param trend_par a list of options for F_trend
#' @param shock_par a list of options for F_shock
#' @return a [list]
#' @keywords internal
#' @export
make_MY_obj_trivial = function(nPatches, options,
                               f = 1, q = 1, Z=1, eggs=1,
                               F_season = list(),
                               F_trend = list(),
                               F_shock = list(),
                               season_par = list(name = "F_one"),
                               trend_par = list(name = "F_one"),
                               shock_par = list(name = "F_one")){
  with(options,{
    MY_obj <- list()
    class(MY_obj) <- 'trivialMY'
    MY_obj$nPatches <- nPatches

    MY_obj$eip <- 0
    MY_obj$f  <- checkIt(f, nPatches)
    MY_obj$q  <- checkIt(q, nPatches)

    base = list()
    class(base) <- c('static', 'trivialMY')
    MY_obj$baseline = base

    MY_obj$Z <- checkIt(Z, nPatches)
    MY_obj$eggs <- checkIt(eggs, nPatches)

    MY_obj <- make_ts_obj(MY_obj, list(
      F_season=F_season, F_trend=F_trend, F_shock=F_shock,
      season_par=season_par, trend_par=trend_par, shock_par=shock_par
    ))
    
    return(MY_obj)
})}


#' @title Set up the module object for `trivialMY` (**MY**)
#'
#' @description
#'
#' This sets up a `trivialMY` model object for the **MY** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_MY_obj_trivial]
#' + `options` is passed to override the defaults
#'
#' In general, this should be used for aquatic mosquito
#' ecology or human / host epidemiology.  In the former case,
#' the user configures `F_eggs`; in the latter,
#' `F_fqZ`
#' 
#' This builds a forcing function as a *composed time series function* using either functions or parameters:
#' + Use a function if passed, and set the corresponding parameter object to an empty list
#' + If no function is passed, the parameters should be a function object (see [ramp.func::F_obj])
#' + Use `make_function` to create a function \eqn{F(t,V)}
#' 
#'
#' @seealso [trivialMY], [make_MY_obj_trivial]
#'
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#' The compatibility name `"trivial"` creates the same `trivialMY` object.
#'
#' @inheritParams setup_MY_obj
#'
#' @return an **`xds`** model object
#' @keywords internal
#'
#' @export
setup_MY_obj.trivialMY = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MYname = "trivialMY"
  
  MY = "MY"
  class(MY) = "MY"
  xds_obj$forced_by = MY

  xds_obj$MY_obj[[s]] <- make_MY_obj_trivial(xds_obj$nPatches, options)
  xds_obj <- setup_F_circadian("setup", xds_obj, s)
  return(xds_obj)
}

#' @inheritParams setup_MY_obj
#' @rdname setup_MY_obj.trivialMY
#' @export
setup_MY_obj.trivial = function(MYname, xds_obj, options=list(), s=1){
  setup_MY_obj.trivialMY(MYname, xds_obj, options=options, s=s)
}

#' @title Get parameters for `trivialMY` (**MY**)
#'
#' @description Return the parameters
#' in the trivial **MY**-Component
#' model as a named list.
#'
#' @inheritParams get_MY_pars
#'
#' @return the parameters as a named list
#'
#' @keywords internal
#' @export
#'
get_MY_pars.trivialMY <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f, q=q, Z=Z, eggs=eggs,
    season_par=season_par,
    trend_par=trend_par,
    shock_par=shock_par
  ))
}

#' @title Change parameters for `trivialMY` (**MY**)
#' @description For the `trivialMY` module (**MY**) for the \eqn{s^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#' Rebuild only forcing functions whose parameters are supplied.
#' An empty parameter list resets the corresponding function to constant one.
#' @inheritParams change_MY_pars
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @details After the updates, call [MBionomics] at time zero with the flattened
#' initial state. Do not call [MEffectSizes]; bionomics is a no-op for this module.
change_MY_pars.trivialMY <- function(xds_obj, options=list(), s=1) {
  for(par in intersect(c("f", "q", "Z", "eggs"), names(options)))
    xds_obj$MY_obj[[s]][[par]] = options[[par]]

  forcing_functions = c(season_par="F_season", trend_par="F_trend",
                        shock_par="F_shock")
  for(par in names(forcing_functions)){
    fun = forcing_functions[[par]]
    if(fun %in% names(options)){
      stopifnot(is.function(options[[fun]]))
      xds_obj$MY_obj[[s]][[fun]] = options[[fun]]
      xds_obj$MY_obj[[s]][[par]] = list()
    } else if(par %in% names(options)){
      xds_obj$MY_obj[[s]][[par]] = options[[par]]
      xds_obj$MY_obj[[s]][[fun]] = make_function(options[[par]])
    }
  }
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- MBionomics(0, y, xds_obj, s)
  return(xds_obj)
}

#' @title Set up initial values for `trivialMY` (**MY**)
#' @description The `trivialMY` module (**MY**) for the \eqn{s^{th}} species
#' has no dynamical variables requiring initial values.
#' This method returns the unmodified **`xds`** object; `options` is unused.
#' @inheritParams setup_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_inits.trivialMY = function(xds_obj, options=list(), s=1){
  return(xds_obj)
}


#' @title Change initial values for `trivialMY` (**MY**)
#' @description The module has no state variables; return the model object unchanged.
#' `options` is unused.
#' @inheritParams change_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_inits.trivialMY <- function(xds_obj, options=list(), s=1) {
  return(xds_obj)
}

#' @title Set up variable indices for `trivialMY` (**MY**)
#' @description The module has no state variables; return the model object unchanged.
#' @inheritParams setup_MY_ix
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_ix.trivialMY <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title Parse outputs for `trivialMY` (**MY**)
#' @description Return an empty list.
#' @inheritParams parse_MY_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_MY_orbits.trivialMY <- function(outputs, xds_obj, s) {
  return(list())
}

#' @title Check `trivialMY` (**MY**)
#' @description Run no consistency checks.
#' @inheritParams check_MY
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.trivialMY = function(xds_obj, s){
  return(xds_obj)
}



#' @title Get the skill set for `trivialMY` (**MY**)
#'
#' @description Return the capabilities and compatibilities of the `trivialMY` module as a list.
#'
#' @inheritParams get_skillset_MY
#'
#' @note The compatibility name `"trivial"` returns the same skill set as
#' `"trivialMY"`.
#'
#' @return the **MY** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_MY.trivialMY = function(xds_obj, s){
  skills = list(
    dts = FALSE,
    xde = TRUE,
    delay = FALSE
  )
  return(skills)
}

#' @inheritParams get_skillset_MY
#' @rdname get_skillset_MY.trivialMY
#' @export
get_skillset_MY.trivial = function(xds_obj, s){
  get_skillset_MY.trivialMY(xds_obj, s)
}
