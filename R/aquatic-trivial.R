# specialized methods for the aquatic mosquito trivial model

#' @title Number of newly emerging adults from each larval habitat
#' @description Implements [F_emerge] for the trivial (forced emergence) model.
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge.trivial <- function(t, y, pars, s) {with(pars$Lpar[[s]],{
  emergents = Lambda*F_season(t)*F_trend(t)
  return(emergents)
})}

#' @title Make parameters for trivial aquatic mosquito model
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param Lambda vector of mean emergence rates from each aquatic habitat
#' @param F_season a function that gives a F_seasonal pattern
#' @param F_trend a function that returns a temporal F_trend
#' @return none
#' @export
create_Lpar_trivial = function(nHabitats, Lopts=list(),
                               Lambda=1000, F_season=F_flat, F_trend=F_flat){
  with(Lopts,{
    Lpar = list()
    class(Lpar) <- "trivial"
    Lpar$Lambda = checkIt(Lambda, nHabitats)
    Lpar$F_season = F_season
    Lpar$F_trend = F_trend
    Lpar$baseline = "trivial"
    class(Lpar$baseline) = "trivial"
    return(Lpar)
  })}

#' @title Reset aquatic parameters to baseline
#' @description Implements [LBionomics] for the RM model
#' @inheritParams LBionomics
#' @return an **`xds`** object
#' @export
LBionomics.trivial <- function(t, y, pars, s) {
  return(pars)
}

#' @title Compute the steady state as a function of the egg deposition rate eta
#' @description This method dispatches on the type of `Lpar`.
#' @inheritParams xde_steady_state_L
#' @return none
#' @export
xde_steady_state_L.trivial = function(eta, Lpar){
  return(numeric(0))
}

#' @title \eqn{\cal L} Component Derivatives for the `trivial` model
#' @description Return a numeric vector of length 0
#' @inheritParams dLdt
#' @return an empty [list]
#' @export
dLdt.trivial <- function(t, y, pars, s) {
  return(numeric(0))
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams list_Lvars
#' @return an empty [list]
#' @export
list_Lvars.trivial <- function(y, pars, s){
  return(list())
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams put_Lvars
#' @return a [list]
#' @export
put_Lvars.trivial <- function(Lvars, y, pars, s){
  return(y)
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [Update_Lt] for the trivial (forced emergence) model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.trivial <- function(t, y, pars, s) {
  return(list())
}

#' @title xde_setup Lpar for the trivial model
#' @description Implements [make_Lpar] for the trivial model
#' @inheritParams make_Lpar
#' @return a [list] vector
#' @export
make_Lpar.trivial = function(Lname, pars, s, Lopts=list()){
  pars$Lpar[[s]] = create_Lpar_trivial(pars$nHabitats, Lopts)
  return(pars)
}


#' @title Setup \eqn{\cal L}-trivial
#' @description Implements [make_Linits] for the trivial model
#' @inheritParams make_Linits
#' @return a [list]
#' @export
make_Linits.trivial = function(pars, s, Lopts=list()){
  pars$Linits[[s]] = list()
  return(pars)
}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [make_indices_L] for trivial (forced emergence) model.
#' @inheritParams make_indices_L
#' @return none
#' @export
make_indices_L.trivial <- function(pars, s) {
  return(pars)
}

#' @title Update inits for the basic aquatic mosquito competition model
#' @inheritParams update_Linits
#' @return none
#' @export
update_Linits.trivial<- function(pars, y0, s) {
  return(pars)
}

#' @title Parse the variable names for the trivial model
#' @description Implements [parse_Lorbits] for the trivial model.
#' @inheritParams parse_Lorbits
#' @return [list]
#' @export
parse_Lorbits.trivial <- function(outputs, pars, s) {
  return(numeric(0))
}
