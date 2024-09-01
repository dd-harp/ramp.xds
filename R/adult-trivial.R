# specialized methods for the adult mosquito trivial model

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqZ] for the trivial model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqZ.trivial <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  Z = with(pars$MYZpar[[s]], Z*season(t)*trend(t))
  return(f*q*Z)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the trivial model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.trivial <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],
    return(eggs*season(t)*trend(t))
)}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqM] for the trivial model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqM.trivial <- function(t, y, pars, s){
  return(numeric(0))
}


#' @title \eqn{\cal MYZ} Component Derivatives for the `trivial` model
#' @description Implements [dMYZdt] for the trivial (forced emergence) model.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.trivial <- function(t, y, pars, s){
  numeric(0)
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [Update_MYZt] for the trivial (forced emergence) model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.trivial <- function(t, y, pars, s){
  numeric(0)
}


#' @title Setup the trivial
#' @description Implements [make_MYZpar] for the trivial model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.trivial = function(MYZname, pars, s, MYZopts=NULL){
  MYZpar <- create_MYZpar_trivial(pars$nPatches, MYZopts)
  class(MYZpar) <- 'trivial'
  pars$MYZpar[[s]] <- MYZpar
  return(pars)
}

#' @title Steady States: MYZ-trivial
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
xde_steady_state_MYZ.trivial = function(Lambda, kappa, MYZpar){with(MYZpar,{
  return(numeric(0))
})}

#' @title Make parameters for trivial aquatic mosquito model
#' @param nPatches an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param f the blood feeding rate
#' @param q the human fraction
#' @param Z the human fraction
#' @param eggs the human fraction
#' @param season a seasonality function
#' @param trend a trend function
#' @return none
#' @export
create_MYZpar_trivial = function(nPatches, MYZopts,
                               f = 1, q = 1, Z=1, eggs=1,
                               season = F_flat, trend=F_flat){
  with(MYZopts,{
    MYZpar <- list()
    MYZpar$nPatches <- nPatches

    MYZpar$eip <- 0
    MYZpar$f_t  <- checkIt(f, nPatches)
    MYZpar$es_f <- rep(1, nPatches)
    MYZpar$q_t  <- checkIt(q, nPatches)
    MYZpar$es_q <- rep(1, nPatches)
    MYZpar$g_t  <- rep(0, nPatches)
    MYZpar$es_g <- rep(1, nPatches)
    MYZpar$sigma_t  <- rep(0, nPatches)
    MYZpar$es_sigma <- rep(1, nPatches)

    base = list()
    class(base) <- c('static', 'trivial')
    MYZpar$baseline = base

    MYZpar$Z <- checkIt(Z, nPatches)
    MYZpar$eggs <- checkIt(eggs, nPatches)
    MYZpar$season <- season
    MYZpar$trend <- trend
    return(MYZpar)
  })}

#' @title Setup the trivial model
#' @description Implements [make_MYZinits] for the trivial model
#' @inheritParams make_MYZinits
#' @return a [list] vector
#' @export
make_MYZinits.trivial = function(pars, s, MYZopts=NULL){
  return(pars)
}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for trivial (forced emergence) model.
#' @inheritParams make_indices_MYZ
#' @return none
#' @export
make_indices_MYZ.trivial <- function(pars, s) {
  return(pars)
}

#' @title Parse the output of deSolve and return variables for the trivial model
#' @description Implements [parse_MYZorbits] for trivial
#' @inheritParams parse_MYZorbits
#' @return [list]
#' @export
parse_MYZorbits.trivial <- function(outputs, pars, s) {
  return(list())
}

#' @title Update inits for trivial
#' @inheritParams update_MYZinits
#' @return none
#' @export
update_MYZinits.trivial <- function(pars, y0, s) {
  return(pars)
}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the GeRM model.
#' @inheritParams get_MYZinits
#' @return none
#' @export
get_MYZinits.trivial <- function(pars, s){
  return(c())
}



#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.trivial = function(pars, s=1){
  with(pars$MYZpar[[s]], f_t*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.trivial = function(pars, s=1){
  with(pars$MYZpar[[s]], q_t*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.trivial = function(pars, s=1){
  numeric(0)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.trivial = function(pars, s=1){
  numeric(0)
}
