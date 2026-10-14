#' @title Setup births
#' 
#' @description
#' Configure the births function
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_demog_obj = function(xds_obj, i=1){
  demog = list() 
  class(demog) = "zero"
  demog$name = "zero"
  xds_obj$XH_obj[[i]]$birth_obj = demog
  xds_obj <- setup_births("zero", xds_obj, i=i)
  xds_obj$XH_obj[[i]]$death_obj = demog
  xds_obj <- setup_deaths("zero", xds_obj, i=i)
  return(xds_obj)
}

#' @title Human/host births
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births <- function(t, xds_obj, i){
  UseMethod("Births", xds_obj$XH_obj[[i]]$birth_obj)
}

#' @title Human/host births
#'
#' @description No births
#'
#' @inheritParams Births
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births.zero <- function(t, xds_obj, i){rep(0, xds_obj$nStrata[i])}


#' @title Human/host births
#'
#' @description No births
#'
#' @inheritParams Births
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births.constant <- function(t, xds_obj, i){xds_obj$XH_obj[[i]]$birth_obj$birth_rates}



#' @title Human/host deaths
#'
#' @param t current simulation time
#' @param X a state variable
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return mortality rate for \eqn{X}
#' 
#' @keywords internal
#' @export
Deaths <- function(t, X, xds_obj, i){
  UseMethod("Deaths", xds_obj$XH_obj[[i]]$death_obj)
}

#' @title Human/host deaths
#'
#' @param t current simulation time
#' @param X a state variable
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return mortality rate for \eqn{X}
#' 
#' @keywords internal
#' @export
Deaths.zero <- function(t, X, xds_obj, i){
  return(0*X)
}

#' @title Human/host deaths
#'
#' @param t current simulation time
#' @param X a state variable
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return mortality rate for \eqn{X}
#' 
#' @keywords internal
#' @export
Deaths.constant <- function(t, X, xds_obj, i){
  return(xds_obj$XH_obj[[i]]$death_obj$death_rates*X)
}

#' @title Check 
#' 
#' @description
#' Configure the births function
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_dHdt = function(xds_obj, i=1){
  H <- get_H(xds_obj, i)
  dHdt <- Births(t, xds_obj, i) - Deaths(t, H, xds_obj, i)
  if(sum(abs(dHdt)>1e-12)>0){
    class(xds_obj$blood_feeding_obj) = "dynamic"
    class(xds_obj$transmission_obj) = "dynamic"
  }
  return(xds_obj)
}


