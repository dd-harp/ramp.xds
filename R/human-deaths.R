# X

#' @title Check deaths 
#' 
#' @description
#' Check that 
#' + the death rate is numeric
#' + the death rate is not negative
#' + the length of the death rates is the number of strata
#' 
#' @param death_rates death rates
#' @param Nh the number of population strata
#' 
#' @seealso [xds_info_time_spent]
#' @export
check_deaths = function(death_rates, Nh){
  stopifnot(is.numeric(death_rates))
  stopifnot(death_rates>=0)
  stopifnot(length(death_rates)==Nh) 
}

#' @title Change the deaths
#' 
#' @description
#' Modify the death rates. Birth rates are unchanged;
#' [setup_deaths.constant] also adjusts births to balance deaths
#' at the configured population density.
#' 
#' 
#' @param death_rates death rates 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_deaths = function(death_rates, xds_obj, i=1){
  check_deaths(death_rates, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$death_obj$death_rates <- death_rates
  class(xds_obj$XH_obj[[i]]$death_obj) <- "constant"
  return(xds_obj)
}

#' @title Set up deaths 
#'
#' @param name a numeric vector, a setup function name, or an options list
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** model object
#'
#' @export
setup_deaths = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_deaths", options)
}

#' @title Set up deaths
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `deaths_name = name$name` 
#' + `options = name`
#' and call `setup_deaths(deaths_name, xds_obj, options, i)`.
#'
#' @inheritParams setup_deaths
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_deaths.list = function(name, xds_obj, options=list(), i=1){
  options = name
  deaths_name = name$name
  if(is.null(deaths_name)) deaths_name = "constant"
  xds_obj <- setup_deaths(deaths_name, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up death rates
#' @description Change nothing.
#' @inheritParams setup_deaths
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_deaths.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up death rates
#' 
#' @description 
#' Pass pre-configured constant death rates. If they pass
#' the checks, they replace the current death rates.
#' 
#' The death rates must be at `options$death_rates`.
#' 
#' @inheritParams setup_deaths
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_deaths.constant = function(name, xds_obj, options=list(), i=1){
  death_rates <- options$death_rates
  xds_obj <- change_deaths(death_rates, xds_obj, i)
  opts = list(birth_rates = death_rates*get_H(xds_obj, i)) 
  xds_obj <- setup_births("constant", xds_obj, opts, i)
  return(xds_obj)
}

#' @title Set up death rates
#' 
#' @description 
#' Set death rates to zero.
#' 
#' @inheritParams setup_deaths
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_deaths.zero = function(name, xds_obj, options=list(), i=1){
  xds_obj$XH_obj[[i]]$death_obj$death_rates = rep(0, xds_obj$nStrata[i])
  class(xds_obj$XH_obj[[i]]$death_obj) = "zero"
  return(xds_obj)
}

#' @title Set up deaths
#' @description 
#' Pass pre-configured deaths. If they pass
#' the checks, they replace the current deaths.
#' 
#' If called with `name = "as_is"`, the deaths
#' must be at `options$death_rates`.
#' 
#' @inheritParams setup_deaths
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_deaths.as_is = function(name, xds_obj, options=list(), i=1){
  
  if(is.list(options)) 
    death_rates = options$death_rates
  if(is.numeric(name))
    death_rates = name
  
  xds_obj <- change_deaths(death_rates, xds_obj, i)
  
  return(xds_obj)
}

#' @title Get the deaths 
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return the death rates
#' @export
get_death_rates = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$death_obj$death_rates)
}
