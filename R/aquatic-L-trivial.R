# specialized methods for the aquatic mosquito trivial model

#' @title `trivial` --- **L** Module 
#' @description
#' The trivial module outputs the emergence 
#' rate of adult mosquitoes by calling a
#' *trace function,* 
#' \deqn{F_\alpha(t) = \Lambda S(t) T(t) K(t)} 
#' where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging per day
#' + \eqn{S(t)} or `F_season` is a seasonal pattern 
#' + \eqn{T(t)} or `F_trend` is a trend pattern 
#' + \eqn{K(t)} or `F_shock` is a perturbation 
#' 
#' @section Parameters:
#' \describe{
#'   \item{`Lambda`}{the mean annual emergence rate}
#'   \item{`season_par`}{parameters for [make_function]: `F_season=make_par(season_par`)}
#'   \item{`trend_par`}{parameters for [make_function]: `F_trend=make_par(trend_par)`}
#'   \item{`shock_par`}{parameters for [make_function]: `F_shock=make_par(shock_par)`}
#' }
#'  
#' The default setup option is `season_par = trend_par = shock_par = makepar_F_one()`. 
#' 
#' @section Get: 
#' 
#' + `get_L_pars` --- the `trivial` method returns all the parameters
#' + `get_mean_forcing` --- get `Lambda`
#' + `get_season` --- returns `season_par` 
#' + `get_trend`  --- returns `trend_par` 
#' + `get_shock`  --- returns `shock_par`
#' 
#' @section Change: 
#' 
#' + `change_L_pars` --- change parameters by name 
#' + `change_mean_forcing` --- changes `Lambda`
#' + `change_season` --- changes elements of `season_par` 
#' + `change_trend`  --- changes elements of `trend_par` 
#' + `change_shock`  --- changes elements of `shock_par`
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
#' 3. Setup expects that `membership= c(1:nPatches),` but any membership vector 
#' works.  
#' 
#'
#' @name trivial_L
NULL

#' @title The **L** Module Skill Set
#'
#' @description The **L** skill set is a list of
#' a module's capabilities
#'
#' @param Lname  the name of the **L** module
#'
#' @return *L* module skill set, as a list
#'
#' @keywords internal
#' @export
skill_set_L.trivial = function(Lname="trivial"){
  list(trivial=TRUE)
}

#' @title Check the `trivial` module
#' @description Run no consistency checks 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_L.trivial = function(xds_obj, s){
  return(xds_obj)
}

#' @title Derivatives for `trivial` (**L**) 
#' @description Returns a numeric vector of length 0
#' @inheritParams dLdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dLdt.trivial <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Update State Variables for `trivial` (**L**) 
#' @description Returns a numeric vector of length 0
#' @inheritParams Update_Lt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_Lt.trivial <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Compute Emergent Adults for `trivial` (**L** Component)
#' @description The number of emerging adults is a function \deqn{\Lambda S(t) T(t) K(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging per day
#' + \eqn{S(t)} or `F_season` is a seasonal signal (ideally, with an average annual mean of 1)
#' + \eqn{T(t)} or `F_trend` is a function returning a trend (ideally, with an average value of 1)
#' + \eqn{K(t)} or `F_shock` is a function describing a perturbation (by default, set to 1)
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
F_emerge.trivial <- function(t, y, xds_obj, s) {
  with(xds_obj$L_obj[[s]],{
    return(Lambda*F_season(t)*F_trend(t)*F_shock(t))
})}

#' @title Bionomics for `trivial` (**L** Component)
#' @description Implements [LBionomics] for the `trivial` module
#' @inheritParams LBionomics
#' @return an **`xds`** object
#' @keywords internal
#' @export
LBionomics.trivial<- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Bionomics for `trivial` (**L** Component)
#' @description Implements [LEffectSizes] for the `trivial` module
#' @inheritParams LEffectSizes
#' @return an **`xds`** object
#' @keywords internal
#' @export
LEffectSizes.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Setup `L_obj` for the `trivial` module
#' @description Implements [setup_L_obj] for the trivial model
#' @inheritParams setup_L_obj
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_obj.trivial = function(Lname, xds_obj, s, options=list()){
  forced_by = "Lambda"
  class(forced_by) = "Lambda"
  xds_obj$forced_by = forced_by
  xds_obj$L_obj[[s]] = make_L_obj_trivial(xds_obj$nHabitats, options)
  xds_obj = rebuild_forcing_functions(xds_obj, s)
  return(xds_obj)
}


#' @title Make `L_obj` for `trivial` (**L** Component)
#' @description The number of emerging adults is a function \deqn{\Lambda S(t) T(t) K(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging per day
#' + \eqn{S(t)} or `F_season` is a seasonal signal (ideally, with an average annual mean of 1)
#' + \eqn{T(t)} or `F_trend` is a function returning a trend (ideally, with an average value of 1)
#' + \eqn{K(t)} or `F_shock` is a function returning a perturbation (by default, set to 1)
#' @param nHabitats the number of habitats in the model
#' @param options a [list] that overwrites default values
#' @param Lambda vector of mean emergence rates from each aquatic habitat
#' @param season_par an object to configure a seasonality function using [make_function]
#' @param trend_par an object to configure a trends function using [make_function]
#' @param shock_par an object to configure a shocks function using [make_function]
#' @return a [list]: an L module object 
#' @keywords internal
#' @export
make_L_obj_trivial = function(nHabitats, options=list(),
                             Lambda=1000,
                             season_par = makepar_F_one(),
                             trend_par = makepar_F_one(),
                             shock_par = makepar_F_one()){
  with(options,{
    L_obj = list()
    class(L_obj) <- "trivial"
    L_obj$Lambda = checkIt(Lambda, nHabitats)

    L_obj$season_par <- season_par
    L_obj$trend_par <- trend_par
    L_obj$shock_par <- shock_par

    return(L_obj)
})}


#' @title Get **L** Component Parameters for `trivial`
#' @description Get \eqn{\Lambda} and parameters that construct
#' the forcing functions  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
get_L_pars.trivial <- function(xds_obj, s=1) {
  with(xds_obj$L_obj[[s]], list(
    Lambda=Lambda,
    season_par=season_par,
    trend_par=trend_par,
    shock_par=shock_par
  ))
}

#' @title Set **L** Component parameters for `trivial`
#'
#' @description If `Lambda` or `F_season` or `F_trend` or `F_shock`
#' are named in a list `options`, the old value is replaced
#'
#' @inheritParams change_L_pars
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_pars.trivial <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$L_obj[[s]], with(options,{
    xds_obj$L_obj[[s]]$Lambda = Lambda
    xds_obj$L_obj[[s]]$F_season = F_season
    xds_obj$L_obj[[s]]$F_trend = F_trend
    xds_obj$L_obj[[s]]$F_shock = F_shock
    return(xds_obj)
  }))}


#' @title Setup Initial Values for the **L** Component `trivial` Module
#' @description The `trivial` module initial values are an empty list
#' @inheritParams setup_L_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_inits.trivial = function(xds_obj, s, options=list()){
  xds_obj$L_obj[[s]]$inits = list()
  return(xds_obj)
}

#' @title List **L** Component Variables for `trivial`
#' @description This method dispatches on the type of `xds_obj$L_obj[[s]]`
#' @inheritParams get_L_vars
#' @return an empty [list]
#' @keywords internal
#' @export
get_L_vars.trivial <- function(y, xds_obj, s){
  return(list())
}


#' @title Set the Initial Values for `trivial` (**L**) 
#' @description Returns the unmodified **`xds`** object
#' @inheritParams change_L_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_inits.trivial <- function(xds_obj, s=1, options=list()) {
  return(xds_obj)
}

#' @title Set up Indices for `trivial` (**L**)
#' @description Return the **`xds`** object unmodified 
#' @inheritParams setup_L_ix
#' @return an **`xds`** object
#' @keywords internal
#' @seealso [trivial_L]
#' @export
setup_L_ix.trivial <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title Parse for `trivial` (**L**)
#' @description Returns an empty list
#' @inheritParams parse_L_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_L_orbits.trivial <- function(outputs, xds_obj, s) {
  return(list())
}
