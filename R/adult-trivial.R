# SPECIALIZED METHODS FOR THE ADULT MOSQUITO TRIVIAL MODEL

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqZ] for the trivial model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqZ.trivial <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  Z = with(pars$MYZpar[[s]], Z*F_season(t)*F_trend(t))
  return(f*q*Z)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the trivial model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.trivial <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],
       return(eggs*F_season(t)*F_trend(t))
  )}

#' @title Blood feeding rate of the infective mosquito population
#' @description Implements [F_fqM] for the trivial model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_fqM.trivial <- function(t, y, pars, s){
  return(numeric(0))
}


#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect trivialzes for static models
#' @description
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBaseline.trivial <- function(t, y, pars, s) {
  return(pars)
}

#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models
#' @description
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.trivial <- function(t, y, pars, s) {
  return(pars)
}


#' @title **MYZ** Component Derivatives for the `trivial` model
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
#' @description Implements [setup_MYZpar] for the trivial model
#' @inheritParams setup_MYZpar
#' @return a [list] vector
#' @export
setup_MYZpar.trivial = function(MYZname, pars, s, MYZopts=NULL){
  MYZpar <- make_MYZpar_trivial(pars$nPatches, MYZopts)
  class(MYZpar) <- 'trivial'
  pars$MYZpar[[s]] <- MYZpar
  return(pars)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MYZpars.trivial <- function(pars, s=1) {
  with(pars$MYZpar[[s]], list(
    f=f, q=q, Z=Z, eggs=eggs,
    F_season=F_season,
    F_trend=F_trend
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @inheritParams set_Xinits
#' @return an **`xds`** object
#' @export
set_Xinits.trivial <- function(pars, i=1, Xopts=list()) {
  return(pars)
}

#' @title Set new MYZ parameter values
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZinits
#' @return an `xds` object
#' @export
set_MYZinits.trivial <- function(pars, s=1, MYZopts=list()) {
  return(pars)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZpars
#' @return an **`xds`** object
#' @export
set_MYZpars.trivial <- function(pars, s=1, MYZopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZpar[[s]]$F_season = F_season
    if(exits("season_par")){
      MYZpar[[s]]$F_season <- make_function(season_par) 
      MYZpar[[s]]$season_par <- season_par
    } 
    pars$MYZpar[[s]]$F_trend = F_trend
    if(exists("trend_par")){
      MYZpar[[s]]$F_trend <- make_function(trend_par) 
      MYZpar[[s]]$trend_par <- trend_par
    }   
    
    return(pars)
  }))}


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
#' @param F_season a function describing a seasonal pattern 
#' @param season_par parameters to configure a `F_season` using [make_function]
#' @param F_trend a function describing a temporal trend 
#' @param trend_par parameters to configure `F_trend` using [make_function]
#' @return none
#' @export
make_MYZpar_trivial = function(nPatches, MYZopts,
                               f = 1, q = 1, Z=1, eggs=1,
                               F_season=F_flat, season_par = list(), 
                               F_trend=F_flat, trend_par = list()){
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
   
    MYZpar$F_season = F_season
    MYZpar$season_par <- season_par
    if(length(season_par)>0)
      MYZpar$F_season <- make_function(season_par) 

    MYZpar$F_trend = F_trend
    MYZpar$trend_par <- trend_par
    if(length(trend_par)>0)
      MYZpar$F_trend <- make_function(trend_par) 

    
    return(MYZpar)
  })}

#' @title Setup the trivial model
#' @description Implements [setup_MYZinits] for the trivial model
#' @inheritParams setup_MYZinits
#' @return a [list] vector
#' @export
setup_MYZinits.trivial = function(pars, s, MYZopts=NULL){
  return(pars)
}

#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description Implements [setup_MYZix] for trivial (forced emergence) model.
#' @inheritParams setup_MYZix
#' @return none
#' @export
setup_MYZix.trivial <- function(pars, s) {
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
