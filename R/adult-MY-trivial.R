

#' @title `trivial` --- **MY** module
#' 
#' @description
#' The trivial **MY** module configures two trace functions:
#' + infectious biting:  
#' \deqn{F_{fqZ}(t, V) = fqZ \times S(t, V_s) \times T(t, V_t) \times K(t, V_k)} 
#' + egg laying: 
#' \deqn{F_G(t, V) = G \times S(t,V_s) \times T(t, V_t) \times K(t, V_k)} 
#'
#' where
#' 
#' + \eqn{G} or `eggs` is the mean egg laying rate 
#' + \eqn{fqZ} or `fqZ` is the mean number of infectious bites on humans, per patch 
#' + \eqn{S(t,V_s)} or `F_season` is a seasonal pattern function
#' + \eqn{T(t,V_t)} or `F_trend` is a trend pattern function
#' + \eqn{K(t,V_k)} or `F_shock` is a perturbation function
#' 
#' The variables \eqn{V_s}, \eqn{V_t}, and \eqn{V_t} are  
#' called by [get_variables], which dispatches on the class of 
#' `season_par` or `trend_par` or `shock_par`.
#' 
#' The implementation assumes that only one of these functions gets used.
#' 
#' @section Parameters:
#' \describe{
#'   \item{`eggs`}{the mean number of eggs laid, per day}
#'   \item{`Z`}{the mean density of infectious mosquitoes}
#'   \item{`f`}{the blood feeding rate}
#'   \item{`q`}{the human fraction}
#'   \item{`F_season`}{a seasonal pattern function, \eqn{{S(t,V_s)}}}
#'   \item{`F_trend`}{a trend function, \eqn{T(t,V_t)}}
#'   \item{`F_shock`}{a shock function, \eqn{K(t,V_k)}}
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
#' + `get_MY_pars` --- the `trivial` method returns all the parameters
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
#' @name trivial_MY
#' @rdname trivial_MY
NULL

#' @title Check the `trivial` module (**M**)
#' @description Run no consistency checks
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_MY.trivial = function(xds_obj, s){
  return(xds_obj)
}

#' @title Net infectious biting rate
#' @description Returns 
#' \deqn{F_{fqZ}(t) = fqZ S(t) T(t) K(t)}
#' where 
#' + \eqn{f} is the feeding rate 
#' + \eqn{q} is the human fraction
#' + \eqn{Z} is the density of infectious mosquitoes, per patch 
#' + \eqn{S(t)} or `F_season` is a seasonal pattern 
#' + \eqn{T(t)} or `F_trend` is a trend pattern 
#' + \eqn{K(t)} or `F_shock` is a perturbation 
#' + \eqn{season_par} a list to dispatch options for 
#' + \eqn{trend_par} or `F_season` is a seasonal pattern 
#' + \eqn{shock_par} or `F_season` is a seasonal pattern 
#' 
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.trivial <- function(t, y, xds_obj, s) {
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
#' \deqn{F_{G}(t) = G S(t) T(t) K(t)}
#' where 
#' + \eqn{G} is the number of eggs laid, per patch, per day 
#' + \eqn{S(t)} or `F_season` is a seasonal pattern 
#' + \eqn{T(t)} or `F_trend` is a trend pattern 
#' + \eqn{K(t)} or `F_shock` is a perturbation 
#' 
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.trivial <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    V_s = get_variables(season_par, t, y, xds_obj, s)
    V_t = get_variables(trend_par, t, y, xds_obj, s)
    V_k = get_variables(shock_par, t, y, xds_obj, s)
    return(eggs*F_season(t, V_s)*F_trend(t, V_t)*F_shock(t, V_k))
})}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqM] for the trivial model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
F_fqM.trivial <- function(t, y, xds_obj, s){
  return(numeric(0))
}


#' @title Mosquito bionomics for `trivial` (**MY**)
#' @description Implements [MBionomics] for the `trivial` module
#' @inheritParams MEffectSizes
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBionomics.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `trivial` (**MY**)
#' @description Implements [MEffectSizes] for the `trivial` module
#' @inheritParams MEffectSizes
#' @return an **`xds`** object
#' @keywords internal
#' @export
MEffectSizes.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}


#' @title Compute derivatives for `trivial` (**MY**)
#' @description Implements [dMYdt] for the trivial (forced emergence) model.
#' @inheritParams dMYdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dMYdt.trivial <- function(t, y, xds_obj, s){
  numeric(0)
}

#' @title Update state variables for `trivial` (**MY**)
#' @description Implements [Update_MYt] for the trivial (forced emergence) model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_MYt.trivial <- function(t, y, xds_obj, s){
  numeric(0)
}


#' @title Set up `trivial` (**MY**)
#'
#' @description Set up the trivial adult mosquito model.
#' In general, this should be used for aquatic mosquito
#' ecology or human / host epidemiology.  In the former case,
#' the user configures `F_eggs`; in the latter,
#' `F_fqZ`
#'
#' @inheritParams setup_MY_obj
#'
#' @return an **`xds`** object
#' @keywords internal
#'
#' @export
setup_MY_obj.trivial = function(MYname, xds_obj, s, options=list()){
  MY = "MY"
  class(MY) = "MY"
  xds_obj$forced_by = MY

  MY_obj <- make_MY_obj_trivial(xds_obj$nPatches, options)
  class(MY_obj) <- 'trivial'
  xds_obj$MY_obj[[s]] <- MY_obj
  return(xds_obj)
}


#' @title Make parameters for trivial aquatic mosquito model
#' @param nPatches an integer
#' @param options a [list] of values that overwrites the defaults
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
    MY_obj$nPatches <- nPatches

    MY_obj$eip <- 0
    MY_obj$f  <- checkIt(f, nPatches)
    MY_obj$q  <- checkIt(q, nPatches)

    base = list()
    class(base) <- c('static', 'trivial')
    MY_obj$baseline = base

    MY_obj$Z <- checkIt(Z, nPatches)
    MY_obj$eggs <- checkIt(eggs, nPatches)

    MY_obj$F_season = F_season
    MY_obj$F_trend = F_trend
    MY_obj$F_shock = F_shock

    MY_obj$season_par = season_par
    MY_obj$trend_par = trend_par
    MY_obj$shock_par = shock_par
    
    return(MY_obj)
})}

#' @title List variables for `trivial` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.trivial <- function(y, xds_obj, s){
  return(list())
}

#' @title Get parameters for `trivial` (**MY**)
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
get_MY_pars.trivial <- function(xds_obj, s=1) {
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


#' @title Change initial values for `trivial` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_inits.trivial <- function(xds_obj, s=1, options=list()) {
  return(xds_obj)
}

#' @title Change parameters for `trivial` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_pars.trivial <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$F_season = F_season
    xds_obj$MY_obj[[s]]$F_trend = F_trend
    xds_obj$MY_obj[[s]]$F_shock = F_shock
    return(xds_obj)
  }))}


#' @title The **trivial** module skill set
#'
#' @description The **MY** skill set is a list of
#' a module's capabilities
#'
#' @inheritParams skill_set_MY
#'
#' @return *MY* module skill set, as a list
#'
#' @keywords internal
#' @export
skill_set_MY.trivial = function(MYname){
  return(list())
}

#' @title Setup initial values for `trivial` (**MY**)
#' @description Return the **`xds`** object unmodified
#' @inheritParams setup_MY_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_MY_inits.trivial = function(xds_obj, s, options=list()){
  return(xds_obj)
}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for trivial (forced emergence) model.
#' @inheritParams setup_MY_ix
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_MY_ix.trivial <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title Parse outputs for `trivial` (**MY**)
#' @description Return an empty list
#' @inheritParams parse_MY_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_MY_orbits.trivial <- function(outputs, xds_obj, s) {
  return(list())
}

#' @title Get inits for `trivial` (MY)
#' @description Return a [numeric] vector of length 0
#' @inheritParams get_MY_inits
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
get_MY_inits.trivial <- function(xds_obj, s) {
  return(numeric(0))
}



#' @title Get the feeding rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector
#' @keywords internal
#' @export
get_f.trivial = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector
#' @keywords internal
#' @export
get_q.trivial = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector
#' @keywords internal
#' @export
get_g.trivial = function(xds_obj, s=1){
  numeric(0)
}

#' @title Get the patch emigration rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector
#' @keywords internal
#' @export
get_sigma.trivial = function(xds_obj, s=1){
  numeric(0)
}
