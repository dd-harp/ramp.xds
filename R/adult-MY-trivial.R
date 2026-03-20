

#' @title `trivial` --- **MY** Module 
#' 
#' @description
#' The trivial **MY** module configures two trace functions:
#' + infectious biting:  
#' \deqn{F_{fqZ}(t) = fqZ S(t) T(t) K(t)} 
#' + egg laying: 
#' \deqn{F_G(t) = G S(t) T(t) K(t)} 
#'  
#' where
#' + \eqn{G} or `eggs` is the mean egg laying rate 
#' + \eqn{fqZ} or `fqZ` is the mean number of infectious bites on humans, per patch 
#' + \eqn{S(t)} or `F_season` is a seasonal pattern 
#' + \eqn{T(t)} or `F_trend` is a trend pattern 
#' + \eqn{K(t)} or `F_shock` is a perturbation 
#' 
#' @section Parameters:
#' \describe{
#'   \item{`eggs`}{the mean number of eggs laid, per day}
#'   \item{`Z`}{the mean density of infectious mosquitoes}
#'   \item{`f`}{the blood feeding rate}
#'   \item{`q`}{the human fraction}
#'   \item{`season_par`}{parameters for [make_function]: `F_season=make_par(season_par`)}
#'   \item{`trend_par`}{parameters for [make_function]: `F_trend=make_par(trend_par)`}
#'   \item{`shock_par`}{parameters for [make_function]: `F_shock=make_par(shock_par)`}
#' }
#'  
#' The default setup options: 
#' + for the [make_function] parameters, `season_par = trend_par = shock_par = makepar_F_one()`. 
#' + for the bionomic parameters, `f=q=Z=eggs=1`. 
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
#' + `change_MY_pars` --- change parameters by name 
#' + `change_mean_forcing` --- changes `Z`
#' + `change_season` --- changes elements of `season_par` 
#' + `change_trend`  --- changes elements of `trend_par` 
#' + `change_shock`  --- changes elements of `shock_par`
#' 
#' Note: use `change_MY_pars` to change `eggs`
#' 
#' @section Notes:
#' 
#' 1. The module has no state variables.
#'   
#' 2. The size of an object saved by `saveRDS` balloons if it saves a function,
#' so `saveXDS` function strips the functions and `readRDS` remakes the function
#' from the stored parameters.
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

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqZ] for the trivial model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ.trivial <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  Z = with(xds_obj$MY_obj[[s]], Z*F_season(t)*F_trend(t)*F_shock(t))
  return(f*q*Z)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the trivial model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.trivial <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],
       return(eggs*F_season(t)*F_trend(t)*F_shock(t))
  )}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqM] for the trivial model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
F_fqM.trivial <- function(t, y, xds_obj, s){
  return(numeric(0))
}


#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBaseline.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBionomics.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}


#' @title Handle derivatives for the `trivial` **MY** module
#' @description Implements [dMYdt] for the trivial (forced emergence) model.
#' @inheritParams dMYdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dMYdt.trivial <- function(t, y, xds_obj, s){
  numeric(0)
}

#' @title Handle state updates for the `trivial` **MY** module
#' @description Implements [Update_MYt] for the trivial (forced emergence) model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_MYt.trivial <- function(t, y, xds_obj, s){
  numeric(0)
}


#' @title Setup the trivial model for an adult mosquito model
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
#'
setup_MY_obj.trivial = function(MYname, xds_obj, s, options=list()){
  MY = "MY"
  class(MY) = "MY"
  xds_obj$forced_by = MY

  MY_obj <- make_MY_obj_trivial(xds_obj$nPatches, options)
  class(MY_obj) <- 'trivial'
  xds_obj$MY_obj[[s]] <- MY_obj
  xds_obj <- rebuild_forcing_functions(xds_obj, s)
  return(xds_obj)
}


#' @title Make parameters for trivial aquatic mosquito model
#' @param nPatches an integer
#' @param options a [list] of values that overwrites the defaults
#' @param f the blood feeding rate
#' @param q the human fraction
#' @param Z the density of infectious mosquitoes
#' @param eggs the mean egg laying rate
#' @param season_par parameters to configure a `F_season` using [make_function]
#' @param trend_par parameters to configure `F_trend` using [make_function]
#' @param shock_par parameters to configure `F_shock` using [make_function]
#' @return a [list]
#' @keywords internal
#' @export
make_MY_obj_trivial = function(nPatches, options,
                               f = 1, q = 1, Z=1, eggs=1,
                               season_par = makepar_F_one(),
                               trend_par = makepar_F_one(),
                               shock_par = makepar_F_one()){
  with(options,{
    MY_obj <- list()
    MY_obj$nPatches <- nPatches

    MY_obj$eip <- 0
    MY_obj$f_t  <- checkIt(f, nPatches)
    MY_obj$es_f <- rep(1, nPatches)
    MY_obj$q_t  <- checkIt(q, nPatches)
    MY_obj$es_q <- rep(1, nPatches)
    MY_obj$g_t  <- rep(0, nPatches)
    MY_obj$es_g <- rep(1, nPatches)
    MY_obj$sigma_t  <- rep(0, nPatches)
    MY_obj$es_sigma <- rep(1, nPatches)

    base = list()
    class(base) <- c('static', 'trivial')
    MY_obj$baseline = base

    MY_obj$Z <- checkIt(Z, nPatches)
    MY_obj$eggs <- checkIt(eggs, nPatches)

    MY_obj$season_par <- season_par
    MY_obj$trend_par <- trend_par
    MY_obj$shock_par <- shock_par


    return(MY_obj)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.trivial <- function(y, xds_obj, s){
  return(list())
}

#' @title Return the parameters as a list
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


#' @title Set new MY parameter values
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_inits.trivial <- function(xds_obj, s=1, options=list()) {
  return(xds_obj)
}

#' @title Set **MY** Component parameters for `trivial`
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_pars.trivial <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$F_season = F_season
    if(exists("season_par")){
      MY_obj[[s]]$F_season <- make_function(season_par)
      MY_obj[[s]]$season_par <- season_par
    }
    xds_obj$MY_obj[[s]]$F_trend = F_trend
    if(exists("trend_par")){
      MY_obj[[s]]$F_trend <- make_function(trend_par)
      MY_obj[[s]]$trend_par <- trend_par
    }

    xds_obj$MY_obj[[s]]$F_shock = F_shock
    if(exists("shock_par")){
      MY_obj[[s]]$F_shock <- make_function(shock_par)
      MY_obj[[s]]$shock_par <- shock_par
    }

    return(xds_obj)
  }))}


#' @title The **trivial** Module Skill Set
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

#' @title Setup Inits for `trivial` (**MY**) 
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

#' @title parse the output of deSolve and return variables for the trivial model
#' @description Implements [parse_MY_orbits] for trivial
#' @inheritParams parse_MY_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_MY_orbits.trivial <- function(outputs, xds_obj, s) {
  return(list())
}

#' @title Return initial values as a vector
#' @description Implements [get_MY_inits] for the trivial model.
#' @inheritParams get_MY_inits
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
get_MY_inits.trivial <- function(xds_obj, s) {
  return(numeric(0))
}



#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_f.trivial = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f_t*es_f)
}

#' @title Get the human fraction 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_q.trivial = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q_t*es_q)
}

#' @title Get the mortality rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_g.trivial = function(xds_obj, s=1){
  numeric(0)
}

#' @title Get the patch emigration rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_sigma.trivial = function(xds_obj, s=1){
  numeric(0)
}
