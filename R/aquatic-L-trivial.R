# specialized methods for the aquatic mosquito trivial model

#' @title The **L** Module Skill Set 
#' 
#' @description The **L** skill set is a list of 
#' an module's capabilities 
#'
#' @param Lname  the name of the **L** module
#' 
#' @return *L* module skill set, as a list 
#' 
#' @export
skill_set_L.trivial = function(Lname="trivial"){
  list(trivial=TRUE)  
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_L.trivial = function(xds_obj, s){
  return(xds_obj)
}

#' @title Derivatives for the `trivial` **L** module 
#' @description Returns a numeric vector of length 0
#' @inheritParams dLdt
#' @return an empty [list]
#' @export
dLdt.trivial <- function(t, y, xds_obj, s) {
  return(numeric(0))
}

#' @title Update State Variables for `trivial` (**L** Component)
#' @description Implements [Update_Lt] for the trivial (forced emergence) model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.trivial <- function(t, y, xds_obj, s) {
  return(list())
}

#' @title Compute Emergent Adults for `trivial` (**L** Component)
#' @description The number of emerging adults is a function \deqn{\Lambda S(t) T(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging per day
#' + \eqn{S(t)} or `F_season` is a seasonal signal (ideally, with an average annual mean of 1)
#' + \eqn{T(t)} or `F_trend` is a function returning a trend (ideally, with an average value of 1)
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge.trivial <- function(t, y, xds_obj, s) {
  with(xds_obj$L_obj[[s]],{
    return(Lambda*F_season(t)*F_trend(t)) 
})}

#' @title Baseline Bionomics for `trivial` (**L** Component)
#' @description Implements [LBaseline] for the RM model
#' @inheritParams LBaseline
#' @return a named [list]
#' @export
LBaseline.trivial<- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Bionomics for `trivial` (**L** Component)
#' @description Implements [LBionomics] for the RM model
#' @inheritParams LBionomics
#' @return an **`xds`** object
#' @export
LBionomics.trivial <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title Setup `L_obj` for the `trivial` module
#' @description Implements [setup_L_obj] for the trivial model
#' @inheritParams setup_L_obj
#' @return a [list] vector
#' @keywords internal
#' @export
setup_L_obj.trivial = function(Lname, xds_obj, s, options=list()){
  forced_by = "Lambda"
  class(forced_by) = "Lambda"
  xds_obj$forced_by = forced_by 
  xds_obj$L_obj[[s]] = make_L_obj_trivial(xds_obj$nHabitats, options)
  return(xds_obj)
}


#' @title Make `L_obj` for `trivial` (**L** Component)
#' @description The number of emerging adults is a function \deqn{\Lambda S(t) T(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging per day
#' + \eqn{S(t)} or `F_season` is a seasonal signal (ideally, with an average annual mean of 1)
#' + \eqn{T(t)} or `F_trend` is a function returning a trend (ideally, with an average value of 1)
#' @param nHabitats the number of habitats in the model
#' @param options a [list] that overwrites default values
#' @param Lambda vector of mean emergence rates from each aquatic habitat
#' @param F_season a function describing a seasonal pattern over time
#' @param season_par an object to configure a seasonality function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param trend_par an object to configure a trends function using [make_function]
#' @return none
#' @export
make_L_obj_trivial = function(nHabitats, options=list(),
                             Lambda=1000,
                             F_season=F_flat, season_par = list(), 
                             F_trend=F_flat, trend_par = list()){
  with(options,{
    L_obj = list()
    class(L_obj) <- "trivial"
    L_obj$Lambda = checkIt(Lambda, nHabitats)
    
    L_obj$F_season = F_season
    L_obj$season_par <- season_par
    if(length(season_par)>0)
      L_obj$F_season <- make_function(season_par) 

    L_obj$F_trend = F_trend
    L_obj$trend_par <- trend_par
    if(length(trend_par)>0)
      L_obj$F_trend <- make_function(trend_par) 
    
    return(L_obj)
})}


#' @title Get **L** Component Parameters for `trivial`
#' @description Show the trace function
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @export
get_L_pars.trivial <- function(xds_obj, s=1) {
  with(xds_obj$L_obj[[s]], list(
    Lambda=Lambda,
    F_season=F_season,
    F_trend=F_trend
  ))
}

#' @title Set **L** Component parameters for `trivial`
#' @description If `Lambda` or `F_season` or `F_trend`
#' are named in a list `options`, the old value is replaced
#' @inheritParams change_L_pars
#' @return an **`xds`** object
#' @export
change_L_pars.trivial <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$L_obj[[s]], with(options,{
    xds_obj$L_obj[[s]]$Lambda = Lambda
    xds_obj$L_obj[[s]]$F_season = F_season
    xds_obj$L_obj[[s]]$F_trend = F_trend
    return(xds_obj)
  }))}


#' @title Setup Initial Values for the **L** Component `trivial` Module
#' @description The `trivial` module initial values are an empty list
#' @inheritParams setup_L_inits
#' @return a [list]
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
#' @export
get_L_vars.trivial <- function(y, xds_obj, s){
  return(list())
}


#' @title Set the Initial Values for `trivial` (**L** Component)
#' @description Returns the unmodified **`xds`** object
#' @inheritParams change_L_inits
#' @return an **`xds`** object
#' @export
change_L_inits.trivial <- function(xds_obj, s=1, options=list()) {
  return(xds_obj)
}

#' @title Setup Variable Indices for `trivial` (**L** Component)
#' @description Implements [setup_L_ix] for trivial (forced emergence) model.
#' @inheritParams setup_L_ix
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_ix.trivial <- function(xds_obj, s) {
  return(xds_obj)
}

#' @title parse **L** Component Variables for `basicL`
#' @description Return a numeric vector of length 0
#' @inheritParams parse_L_orbits
#' @return none
#' @export
parse_L_orbits.trivial <- function(outputs, xds_obj, s) {
  return(list())
}
