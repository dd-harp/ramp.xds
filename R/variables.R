
#' @title Compute Other Variables 
#' @description
#' The description for each method should include the equations.
#'
#' @note This is the `S3` generic. Methods dispatch on `Vpar`
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param i the i^th auxiliary variable 
#' @return derivatives for the \eqn{\cal MYZ} component as a [vector]
#' @export
dVdt <- function(t, y, pars, i) {
  UseMethod("dVdt", pars$Vpar[i])
}

#' @title Compute Other Variables 
#' @description
#' The description for each method should include the equations.
#'
#' @note Index the variables
#' @param pars an `xds` object
#' @param i the variable index
#' @return an `xds` object
#' @export
setup_Vix <- function(pars, i){
  var_ix = pars$max_ix + c(1:pars$Vpar[[i]]$nVars) 
  pars$Vpar[[i]]$ix <- var_ix 
  pars$max_ix = max(var_ix)
  return(pars)
}


#' @title Compute Other Variables 
#' @description
#' The description for each method should include the equations.
#'
#' @inheritParams dVdt
#' @return Derivatives for auxiliary variables as a [vector]
#' @export
dVdt.none <- function(t, y, pars, i){
  return(list())   
}

#' @title Set Up the first (null) other variable 
#' @description
#' The description for each method should include the equations.
#'
#' @param pars an `xds` object
#' @return the `xds` object
#' @export
setup_null_auxiliary_variable = function(pars){
  pars$nOtherVariables = 1 
  none <- list() 
  class(none) <- "none" 
  pars$Vpar = list() 
  pars$Vpar[[1]] = none
  return(pars)
}

#' @title Add a new variable 
#' @description
#' The description for each method should include the equations.
#'
#' @param pars an `xds` object
#' @param var_object an object to dispatch and compute a variable 
#' @return the `xds` object
#' @export
add_variable = function(pars, var_object){
  i <- pars$nOtherVariables + 1 
  pars$nOtherVariables = i 
  pars$Vpar[[i]] <- var_object 
  pars <- setup_Vix(pars, i)
  return(pars)
}