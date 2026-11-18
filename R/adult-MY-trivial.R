

#' @title `trivialMY` --- **MY** module
#' 
#' @description
#' The trivial **MY** module configures two trace functions:
#' + infectious biting:  
#' \deqn{F_{fqZ}(t, V) = fqZ \times F_S(t, V_s) \times F_T(t, V_t) \times F_K(t, V_k)} 
#' + egg laying: 
#' \deqn{F_G(t, V) = G \times F_S(t,V_s) \times F_T(t, V_t) \times F_K(t, V_k)} 
#'
#' where
#' 
#' + \eqn{G} or `eggs` is the mean egg laying rate 
#' + \eqn{fqZ} or `fqZ` is the mean number of infectious bites on humans, per patch 
#' + \eqn{F_S(t,V_s)} or `F_season` is a seasonal pattern function
#' + \eqn{F_T(t,V_t)} or `F_trend` is a trend pattern function
#' + \eqn{F_K(t,V_k)} or `F_shock` is a perturbation function
#' 
#' The variables \eqn{V_s}, \eqn{V_t}, and \eqn{V_t} are  
#' called by [get_variables], which dispatches on the class of 
#' `season_par` or `trend_par` or `shock_par`.
#' 
#' The implementation assumes that only one of these functions will be used, so
#' it is not possible to configure different pattern functions for egg laying and infective biting.
#' 
#' @section Parameters:
#' \describe{
#'   \item{`eggs`}{the mean number of eggs laid, per day}
#'   \item{`Z`}{the mean density of infectious mosquitoes}
#'   \item{`f`}{the blood feeding rate}
#'   \item{`q`}{the human fraction}
#'   \item{`F_season`}{a seasonal pattern function, \eqn{{F_S(t,V_s)}}}
#'   \item{`F_trend`}{a trend function, \eqn{F_T(t,V_t)}}
#'   \item{`F_shock`}{a shock function, \eqn{F_K(t,V_k)}}
#'   \item{`season_par`}{dispatches [get_variables] to get \eqn{V_s}}
#'   \item{`trend_par`}{dispatches [get_variables] to get \eqn{V_t}}
#'   \item{`shock_par`}{dispatches [get_variables] to get \eqn{V_k}}
#' }
#' 
#' The default values are `F_season=F_trend=F_shock=F_one` and the 
#' classes of the objects `season_par` and `trend_par` and `shock_par` are all 
#' 'list'
#' 
#' For the bionomic parameters, `f=q=Z=eggs=1`. 
#' 
#' @section Get: 
#' 
#' + `get_MY_pars` --- the `trivialMY` method returns all the parameters
#' + `get_mean_forcing` --- returns `Z`
#' + `get_f` --- returns `f`
#' + `get_q` --- returns `q`
#' + `get_season` --- returns `season_par` 
#' + `get_trend`  --- returns `trend_par` 
#' + `get_shock`  --- returns `shock_par`
#' 
#' Note: use `get_MY_pars` to inspect `eggs`
#' 
#' @section Change: 
#' 
#' + `change_MY_pars` --- change bionomic parameters by name
#' + `change_mean_forcing` --- changes `Z`
#' + `change_F_season` --- changes `F_season`
#' + `change_F_trend`  --- changes `F_trend`
#' + `change_F_shock`  --- changes `F_shock`
#' 
#' 
#' @section Notes:
#' 
#' 1. The module has no state variables.
#'   
#' 2. The size of an object saved by `saveRDS` balloons if it saves a function,
#' so `saveXDS` function strips the functions and `readRDS` remakes the function
#' from the stored parameters.
#' 
#' `F_season`, `F_trend`, and `F_shock` can be set up manually by passing any
#' user defined function. If so, the user should use `saveRDS` and `readRDS` 
#' rather than `saveXDS` and `readXDS` 
#'
#' @name trivialMY
#' @rdname trivialMY
NULL

#' @title Check the `trivialMY` module (**M**)
#' @description Run no consistency checks
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY.trivialMY = function(xds_obj, s){
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
#' + \eqn{season_par} a list to dispatch options for 
#' + \eqn{trend_par} or `F_season` is a seasonal pattern 
#' + \eqn{shock_par} or `F_season` is a seasonal pattern 
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

#' @title Net egg laying rate
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


#' @title Mosquito bionomics for `trivialMY` (**MY**)
#' @description Implements [MBionomics] for the `trivialMY` module
#' @inheritParams MEffectSizes
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics.trivialMY <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `trivialMY` (**MY**)
#' @description Implements [MEffectSizes] for the `trivialMY` module
#' @inheritParams MEffectSizes
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes.trivialMY <- function(t, y, xds_obj, s) {
  return(xds_obj)
}


#' @title Compute derivatives for `trivialMY` (**MY**)
#' @description Implements [dMYdt] for the trivial (forced emergence) model.
#' @inheritParams dMYdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dMYdt.trivialMY <- function(t, y, xds_obj, s){
  numeric(0)
}

#' @title Update state variables for `trivialMY` (**MY**)
#' @description Implements [Update_MYt] for the trivial (forced emergence) model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_MYt.trivialMY <- function(t, y, xds_obj, s){
  numeric(0)
}


#' @title Set up `trivialMY` (**MY**)
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
#' This builds a trace function as a *composed time series function* using either functions or parameters:
#' + Use a function if passed, and set the function object to an empty list
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
  xds_obj$MY_obj[[s]]$skill_set <- get_skillset_MY(xds_obj, s)
  xds_obj <- setup_F_circadian("setup", xds_obj, s)
  return(xds_obj)
}

#' @rdname setup_MY_obj.trivialMY
#' @export
setup_MY_obj.trivial = function(MYname, xds_obj, options=list(), s=1){
  setup_MY_obj.trivialMY(MYname, xds_obj, options=options, s=s)
}


#' @title Make parameters for trivial aquatic mosquito model
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
                               F_season = F_one, 
                               F_trend = F_one, 
                               F_shock = F_one,
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

    if(length(F_season)>1){
      MY_obj$season_par = list()
      MY_obj$F_season = F_season  
    } else {
      MY_obj$season_par = season_par
      MY_obj$F_season = make_function(season_par)  
    }
    
    if(length(F_trend)>1){
      MY_obj$trend_par = list()
      MY_obj$F_trend = F_trend  
    } else {
      MY_obj$trend_par = trend_par
      MY_obj$F_trend = make_function(trend_par)  
    }
    
    if(length(F_shock)>1){
      MY_obj$shock_par = list() 
      MY_obj$F_shock = F_shock  
    } else {
      MY_obj$shock_par = shock_par
      MY_obj$F_shock = make_function(shock_par)  
    }
    
    return(MY_obj)
})}

#' @title List variables for `trivialMY` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.trivialMY <- function(y, xds_obj, s){
  return(list())
}

#' @title Get parameters for `trivialMY` (**MY**)
#'
#' @description Return the parameters
#' in the trivial **MY**-Component
#' model as a named list
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
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
    F_season=F_season,
    trend_par=trend_par,
    F_trend=F_trend,
    shock_par=shock_par,
    F_shock=F_shock
  ))
}


#' @title Change initial values for `trivialMY` (**MY**)
#' @description For the `trivialMY` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#' @inheritParams change_MY_inits
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_inits.trivialMY <- function(xds_obj, options=list(), s=1) {
  return(xds_obj)
}

#' @title Change parameters for `trivialMY` (**MY**)
#' @description For the `trivialMY` module (**MY**) for the \eqn{s^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#' @inheritParams change_MY_pars
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_MY_pars.trivialMY <- function(xds_obj, options=list(), s=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$F_season = F_season
    xds_obj$MY_obj[[s]]$F_trend = F_trend
    xds_obj$MY_obj[[s]]$F_shock = F_shock
    return(xds_obj)
  }))}



#' @title The skill set
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
    delay = FALSE,
    not_implemented = TRUE
  )
  return(skills)
}

#' @rdname get_skillset_MY.trivialMY
#' @export
get_skillset_MY.trivial = function(xds_obj, s){
  get_skillset_MY.trivialMY(xds_obj, s)
}

#' @title Setup initial values for `trivialMY` (**MY**)
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

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for trivial (forced emergence) model.
#' @inheritParams setup_MY_ix
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_MY_ix.trivialMY <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title Parse outputs for `trivialMY` (**MY**)
#' @description Return an empty list
#' @inheritParams parse_MY_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_MY_orbits.trivialMY <- function(outputs, xds_obj, s) {
  return(list())
}

#' @title Get inits for `trivialMY` (MY)
#' @description Return a [numeric] vector of length 0
#' @inheritParams get_MY_inits
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
get_MY_inits.trivialMY <- function(xds_obj, s) {
  return(numeric(0))
}



#' @title Get the feeding rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector
#' @keywords internal
#' @export
get_f.trivialMY = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector
#' @keywords internal
#' @export
get_q.trivialMY = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector
#' @keywords internal
#' @export
get_g.trivialMY = function(xds_obj, s=1){
  numeric(0)
}

#' @title Get the patch emigration rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector
#' @keywords internal
#' @export
get_sigma.trivialMY = function(xds_obj, s=1){
  numeric(0)
}
