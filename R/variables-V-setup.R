
#' @title Compute Other Variables 
#' @description
#' The description for each method should include the equations.
#'
#' @note Index the variables
#' @param xds_obj an `xds` object
#' @param i the variable index
#' @return an `xds` object
#' @export
setup_V_ix.setup <- function(xds_obj, i){
  var_ix = xds_obj$max_ix + c(1:xds_obj$V_obj[[i]]$nVars) 
  xds_obj$V_obj[[i]]$ix <- var_ix 
  xds_obj$max_ix = max(var_ix)
  return(xds_obj)
}


#' @title Compute Other Variables 
#' @description
#' The description for each method should include the equations.
#'
#' @inheritParams dVdt
#' @return Derivatives for auxiliary variables as a [vector]
#' @export
dVdt.setup <- function(t, y, xds_obj, i){
  return(list())   
}

#' @title Set Up the first (null) other variable 
#' @description
#' The description for each method should include the equations.
#'
#' @param xds_obj an `xds` object
#' @return the `xds` object
#' @export
setup_other_variables = function(xds_obj){
  xds_obj$nOtherVariables = 1 
  setup <- list() 
  class(setup) <- "setup" 
  xds_obj$V_obj = list() 
  xds_obj$V_obj[[1]] = setup 
  return(xds_obj)
}

#' @title Add Variable 
#' 
#' @description
#' This is a generic method to add new dependent state variables 
#' that are computed by `dVdt.` 
#' 
#' The model object for the new variable should fully define 
#' an other variables module, including methods to set up
#' initial values.  
#' 
#' The description for each method should include the equations.
#'
#' @param var_object an object to dispatch and compute a variable 
#' @param xds_obj an **`xds`** model object
#' 
#' @return the **`xds`** model object with a new variable
#' 
#' @export
add_variable = function(var_object, xds_obj){
  i <- xds_obj$nOtherVariables + 1 
  xds_obj$nOtherVariables = i 
  xds_obj$V_obj[[i]] <- var_object 
  xds_obj <- setup_V_ix(xds_obj, i)
  return(xds_obj)
}