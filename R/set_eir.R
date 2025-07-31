
#' @title Reset the mean daily EIR
#' @description For cohort models, reset the EIR
#' @param eir the mean eir
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
set_eir  = function(eir, pars){
  UseMethod("set_eir", pars$frame)
}

#' @title Reset the mean daily EIR
#' 
#' @description For cohort dynamcis models, set the mean EIR
#' 
#' @inheritParams set_eir
#' 
#' @return an **`xds`** object
#' 
#' @export
#' 
set_eir.cohort  = function(eir, pars){
  
  pars$EIRpar$eir <- eir  
  if(length(pars$EIRpar$season_par)>0)  
    pars$EIRpar$F_season <- make_function(pars$EIRpar$season_par)
  if(length(pars$EIRpar$trend_par)>0)  
    pars$EIRpar$F_trend <- make_function(pars$EIRpar$trend_par)
  if(length(pars$EIRpar$age_par)>0)  
    pars$EIRpar$F_age <- make_function(pars$EIRpar$age_par)
  
  pars$F_eir <- with(pars$EIRpar,{
    function(age, bday){
      eir/scale*F_season(age+bday)*F_trend(age+bday)*F_age(age)}
  })
  return(pars)
}

#' @title Reset the mean daily EIR
#' 
#' @description For cohort models, reset the EIR
#' 
#' @inheritParams set_eir
#' 
#' @return an **`xds`** object
#' 
#' @export
#' 
set_eir.eir = function(eir, pars){
  
  pars$EIRpar$eir <- eir  
  if(length(pars$EIRpar$season_par)>0)  
    pars$EIRpar$F_season <- make_function(pars$EIRpar$season_par)
  if(length(pars$EIRpar$trend_par)>0)  
    pars$EIRpar$F_trend <- make_function(pars$EIRpar$trend_par)
  
  pars$F_eir <- with(pars$EIRpar,{
    function(t){
      eir/scale*F_season(t)*F_trend(t)}
  })
  
  return(pars)
}
