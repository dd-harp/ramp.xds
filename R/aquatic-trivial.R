# specialized methods for the aquatic mosquito trivial model

#' @title Derivatives for `trivial` (**L** Component)
#' @description Return a numeric vector of length 0
#' @inheritParams dLdt
#' @return an empty [list]
#' @export
dLdt.trivial <- function(t, y, pars, s) {
  return(numeric(0))
}

#' @title Update State Variables for `trivial` (**L** Component)
#' @description Implements [Update_Lt] for the trivial (forced emergence) model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.trivial <- function(t, y, pars, s) {
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
F_emerge.trivial <- function(t, y, pars, s) {with(pars$Lpar[[s]],{
  emergents = Lambda*F_season(t)*F_trend(t)
  return(emergents)
})}

#' @title Baseline Bionomics for `trivial` (**L** Component)
#' @description Implements [LBaseline] for the RM model
#' @inheritParams LBaseline
#' @return a named [list]
#' @export
LBaseline.trivial<- function(t, y, pars, s) {
  return(pars)
}

#' @title Bionomics for `trivial` (**L** Component)
#' @description Implements [LBionomics] for the RM model
#' @inheritParams LBionomics
#' @return an **`xds`** object
#' @export
LBionomics.trivial <- function(t, y, pars, s) {
  return(pars)
}

#' @title Setup `Lpar` for the `trivial` module
#' @description Implements [setup_Lpar] for the trivial model
#' @inheritParams setup_Lpar
#' @return a [list] vector
#' @export
setup_Lpar.trivial = function(Lname, pars, s, Lopts=list()){
  pars$Lpar[[s]] = make_Lpar_trivial(pars$nHabitats, Lopts)
  return(pars)
}


#' @title Make `Lpar` for `trivial` (**L** Component)
#' @description The number of emerging adults is a function \deqn{\Lambda S(t) T(t)} where
#' + \eqn{\Lambda} or `Lambda` is the mean number of adult female mosquitoes emerging per day
#' + \eqn{S(t)} or `F_season` is a seasonal signal (ideally, with an average annual mean of 1)
#' + \eqn{T(t)} or `F_trend` is a function returning a trend (ideally, with an average value of 1)
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param Lambda vector of mean emergence rates from each aquatic habitat
#' @param F_season a F_seasonality function
#' @param F_trend a F_trend function
#' @return none
#' @export
make_Lpar_trivial = function(nHabitats, Lopts=list(),
                             Lambda=1000,
                             F_season=F_flat, F_trend=F_flat){
  with(Lopts,{
    Lpar = list()
    class(Lpar) <- "trivial"
    Lpar$Lambda = checkIt(Lambda, nHabitats)
    Lpar$F_season = F_season
    Lpar$F_trend = F_trend
    return(Lpar)
  })}


#' @title Get **L** Component Parameters for `trivial`
#' @description Show the trace function
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_Lpars.trivial <- function(pars, s=1) {
  with(pars$Lpar[[s]], list(
    Lambda=Lambda,
    F_season=F_season,
    F_trend=F_trend
  ))
}

#' @title Set **L** Component parameters for `trivial`
#' @description If `Lambda` or `F_season` or `F_trend`
#' are named in a list `Lopts`, the old value is replaced
#' @inheritParams set_Lpars
#' @return an **`xds`** object
#' @export
set_Lpars.trivial <- function(pars, s=1, Lopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$Lpar[[s]], with(Lopts,{
    pars$Lpar[[s]]$Lambda = Lambda
    pars$Lpar[[s]]$F_season = F_season
    pars$Lpar[[s]]$F_trend = F_trend
    return(pars)
  }))}


#' @title Setup Initial Values for the **L** Component `trivial` Module
#' @description The `trivial` module initial values are an empty list
#' @inheritParams setup_Linits
#' @return a [list]
#' @export
setup_Linits.trivial = function(pars, s, Lopts=list()){
  pars$Linits[[s]] = list()
  return(pars)
}

#' @title List **L** Component Variables for `trivial`
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams list_Lvars
#' @return an empty [list]
#' @export
list_Lvars.trivial <- function(y, pars, s){
  return(list())
}


#' @title Set the Initial Values for `trivial` (**L** Component)
#' @description Returns the unmodified **`xds`** object
#' @inheritParams set_Linits
#' @return an **`xds`** object
#' @export
set_Linits.trivial <- function(pars, s=1, Lopts=list()) {
  return(pars)
}

#' @title Update Initial Values for `basicL` from a state vector `y`
#' @description Returns the unmodified **`xds`** object
#' @inheritParams update_Linits
#' @return an **`xds`** object
#' @export
update_Linits.trivial<- function(pars, y, s) {
  return(pars)
}

#' @title Setup Variable Indices for `trivial` (**L** Component)
#' @description Implements [setup_Lix] for trivial (forced emergence) model.
#' @inheritParams setup_Lix
#' @return an **`xds`** object
#' @export
setup_Lix.trivial <- function(pars, s) {
  return(pars)
}

#' @title Parse **L** Component Variables for `basicL`
#' @description Return a numeric vector of length 0
#' @inheritParams parse_Lorbits
#' @return none
#' @export
parse_Lorbits.trivial <- function(outputs, pars, s) {
  return(numeric(0))
}

#' @title Compute the Steady State of `dLdt.trivial` (**L** Component)
#' @description Return a numeric vector of length 0
#' @inheritParams xde_steady_state_L
#' @return none
#' @export
xde_steady_state_L.trivial = function(eta, Lpar){
  return(numeric(0))
}

