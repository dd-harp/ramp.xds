# specialized methods for the aquatic mosquito trivial model

#' @title `trivial` --- **L** module
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
#'   \item{`Lambda`}{the mean daily emergence rate}
#'   \item{`season_par`}{parameters for [make_function]: `F_season=make_function(season_par)`}
#'   \item{`trend_par`}{parameters for [make_function]: `F_trend=make_function(trend_par)`}
#'   \item{`shock_par`}{parameters for [make_function]: `F_shock=make_function(shock_par)`}
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

#' @title The **L** module skill set
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

#' @title Compute derivatives for `trivial` (**L**)
#' @description Returns a numeric vector of length 0
#' @inheritParams dLdt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
dLdt.trivial <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Update state variables for `trivial` (**L**)
#' @description Returns a numeric vector of length 0
#' @inheritParams Update_Lt
#' @return a [numeric] vector of length 0
#' @keywords internal
#' @export
Update_Lt.trivial <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Compute emergent adults for `trivial` (**L** component)
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

#' @title Mosquito bionomics for `trivial` (**L**)
#' @description Implements [LBionomics] for the `trivial` module
#' @inheritParams LBionomics
#' @return an **`xds`** object
#' @keywords internal
#' @export
LBionomics.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Apply effect sizes for `trivial` (**L**)
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


#' @title Make `L_obj` for `trivial` (**L** component)
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


#' @title Get **L** component parameters for `trivial`
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

#' @title Set **L** component parameters for `trivial`
#'
#' @description If `Lambda`, `season_par`, `trend_par`, or `shock_par`
#' are named in `options`, the old value is replaced. After updating
#' the parameter objects, `F_season`, `F_trend`, and `F_shock` are
#' recompiled by calling [make_function] on the updated parameters
#' via [rebuild_forcing_functions].
#'
#' @inheritParams change_L_pars
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_pars.trivial <- function(xds_obj, s=1, options=list()) {
  with(xds_obj$L_obj[[s]], with(options,{
    xds_obj$L_obj[[s]]$Lambda = Lambda
    xds_obj$L_obj[[s]]$season_par = season_par
    xds_obj$L_obj[[s]]$trend_par = trend_par
    xds_obj$L_obj[[s]]$shock_par = shock_par
    xds_obj = rebuild_forcing_functions(xds_obj, s)
    return(xds_obj)
  }))}


#' @title Setup initial values for the **L** component `trivial` module
#' @description The `trivial` module initial values are an empty list
#' @inheritParams setup_L_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_inits.trivial = function(xds_obj, s, options=list()){
  xds_obj$L_obj[[s]]$inits = list()
  return(xds_obj)
}

#' @title List variables for `trivial` (**L**)
#' @description Returns an empty list; the `trivial` module has no state variables
#' @inheritParams get_L_vars
#' @return an empty [list]
#' @keywords internal
#' @export
get_L_vars.trivial <- function(y, xds_obj, s){
  return(list())
}


#' @title Set the initial values for `trivial` (**L**)
#' @description Returns the unmodified **`xds`** object
#' @inheritParams change_L_inits
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_inits.trivial <- function(xds_obj, s=1, options=list()) {
  return(xds_obj)
}

#' @title Set up indices for `trivial` (**L**)
#' @description Return the **`xds`** object unmodified 
#' @inheritParams setup_L_ix
#' @return an **`xds`** object
#' @keywords internal
#' @seealso [trivial_L]
#' @export
setup_L_ix.trivial <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title Parse outputs for `trivial` (**L**)
#' @description Returns an empty list
#' @inheritParams parse_L_orbits
#' @return an empty [list]
#' @keywords internal
#' @export
parse_L_orbits.trivial <- function(outputs, xds_obj, s) {
  return(list())
}
