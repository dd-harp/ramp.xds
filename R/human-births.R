# X

#' @title Check births 
#' 
#' @description
#' Check that 
#' + the birth rate is numeric
#' + the birth rate is not negative
#' + the length of the birth rates is the number of strata
#' 
#' @param birth_rates the birth rates
#' @param Nh the number of population strata
#' 
#' @export
check_births = function(birth_rates, Nh){
  stopifnot(is.numeric(birth_rates))
  stopifnot(birth_rates>=0)
  stopifnot(length(birth_rates)==Nh) 
}

#' @title Change the birth rates
#' 
#' @description
#' Modify the birth rates for a model with static population 
#' rates. 
#' 
#' 
#' @param birth_rates birth rates 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_births = function(birth_rates, xds_obj, i=1){
  check_births(birth_rates, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$birth_obj$birth_rates <- birth_rates
  class(xds_obj$XH_obj[[i]]$birth_obj) <- "constant" 
  xds_obj <- check_dHdt(xds_obj, i)
  return(xds_obj)
}

#' @title Set up births 
#'
#' @param name a numeric vector, a setup function name, or an options list
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** model object
#'
#' @export
setup_births = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_births", options)
}

#' @title Set up births
#'
#' @description Set up births using an options list.
#'
#' @note If an options list is passed
#' as the first argument, then set 
#' + `births_name = name$name`
#' + `options = name`
#' and call `setup_births(births_name, xds_obj, options, i)`.
#'
#' @inheritParams setup_births
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_births.list = function(name, xds_obj, options=list(), i=1){
  options = name
  births_name = name$name
  if(is.null(births_name)) births_name = "constant"
  xds_obj <- setup_births(births_name, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up no births  
#' @description Don't change anything.
#' @inheritParams setup_births
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_births.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up no births  
#' @description Set birth rates to zero.
#' @inheritParams setup_births
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_births.zero = function(name, xds_obj, options = list(), i=1){
  xds_obj$XH_obj[[i]]$birth_obj$birth_rates = rep(0, xds_obj$nStrata[i])
  class(xds_obj$XH_obj[[i]]$birth_obj) = "zero" 
  return(xds_obj)
}


#' @title Set up birth rates
#' 
#' @description 
#' Pass pre-configured constant birth rates. If they pass
#' the checks, they replace the current birth rates.
#' 
#' The birth rates must be at `options$birth_rates`.
#' 
#' @inheritParams setup_births
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_births.constant = function(name, xds_obj, options=list(), i=1){
  birth_rates <- options$birth_rates
  xds_obj <- change_births(birth_rates, xds_obj, i)
  return(xds_obj)
}

#' @title Set up births
#' @description 
#' Pass pre-configured births. If they pass
#' the checks, they replace the current births.
#' 
#' If called with `name = "as_is"`, the births
#' must be at `options$birth_rates`.
#' 
#' @inheritParams setup_births
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_births.as_is = function(name, xds_obj, options=list(), i=1){
  
  if(is.list(options)) 
    birth_rates = options$birth_rates
  if(is.numeric(name))
    birth_rates = name
  
  xds_obj <- change_births(birth_rates, xds_obj, i)
  
  return(xds_obj)
}

#' @title Get the births 
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [numeric] vector
#' @export
get_birth_rates = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$birth_obj$birth_rates)
}
