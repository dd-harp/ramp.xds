# X

#' @title Check the travel eir 
#' 
#' @description
#' Check that 
#' + the input is numeric
#' + the travel eir has length `Nh`
#' + all elements are non-negative
#' 
#' @param teir a travel eir vector 
#' @param Nh the number of population strata
#' 
#' @seealso [xds_info_time_spent]
#' @export
check_travel_eir = function(teir, Nh){
  stopifnot(is.numeric(teir))
  stopifnot(teir>=0)
  stopifnot(length(teir)==Nh) 
}

#' @title Change the travel eir 
#' @description
#' Update the travel eir for the \eqn{i^{th}} host species
#' 
#' @param travel_eir a travel eir vector 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @seealso [travel_eir], [setup_travel_eir], [get_travel_eir],
#'   [malaria_importation]
#'
#' @return an **`xds`** model object
#' @export
change_travel_eir = function(travel_eir, xds_obj, i=1){
  check_travel_eir(travel_eir, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$travel_eir <- travel_eir
  xds_obj$XH_obj[[i]]$travel_eir_obj <- make_static_obj()
  xds_obj$XH_obj[[i]]$travel_eir_obj$travel_eir <- travel_eir
  return(xds_obj)
}

#' @title Set up travel eir 
#'
#' @inherit travel_eir description
#' @note Travel EIR is stored in `xds_obj$XH_obj[[i]]$travel_eir`.
#'
#' @param name a setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** model object
#' @seealso [travel_eir], [get_travel_eir], [change_travel_eir],
#'   [travel_malaria], [malaria_importation], [update_travel_eir]
#'
#' @section Setup methods:
#' + `as_is` uses a supplied travel EIR vector.
#' + `random` generates a random travel EIR value for each population stratum.
#'
#' @export
setup_travel_eir = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_travel_eir", options)
}

#' @title Set up a travel eir 
#'
#' @details If an options list is passed
#' as the first argument, then set 
#' + `Teirname = name$name` 
#' + `options = name`
#' and call `setup_travel_eir(Teirname, xds_obj, options, i)`
#'
#' @inherit setup_travel_eir description params seealso
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_travel_eir.list = function(name, xds_obj, options=list(), i=1){
  options = name
  Teirname = name$name
  if(is.null(Teirname)) Teirname = "no_setup"
  xds_obj <- setup_travel_eir(Teirname, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up no travel eir  
#' @details Don't change anything
#' @inherit setup_travel_eir description params seealso
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_travel_eir.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up no travel EIR
#' @details Set travel EIR to zero for each stratum of the selected host
#' species using [change_travel_eir].
#' 
#' @inherit setup_travel_eir description params seealso
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_travel_eir.no_travel = function(name, xds_obj, options=list(), i=1){
  teir = rep(0, xds_obj$nStrata[i])
  xds_obj <- change_travel_eir(teir, xds_obj, i)
  return(xds_obj)
}


#' @title Set up a travel eir 
#' @details
#' Pass a pre-configured travel eir vector. If it passes
#' the checks, it replaces the current travel_eir.
#' 
#' If called with `name = "as_is"`, the travel eir
#' must be at `options$travel_eir`.
#' 
#' @inherit setup_travel_eir description params seealso
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_travel_eir.as_is = function(name, xds_obj, options=list(), i=1){
  
  if(is.list(options)) 
    Teir = options$travel_eir
  if(is.numeric(name))
    Teir = name
  
  xds_obj <- change_travel_eir(Teir, xds_obj, i)
  
  return(xds_obj)
}

#' @title Set up random travel EIR
#' 
#' @details Set up random values for travel EIR.
#' 
#' @inherit setup_travel_eir description params seealso
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_travel_eir.random = function(name, xds_obj, options = list(), i=1){
  travel_eir = make_travel_eir_random(xds_obj$nStrata[i], options)
  xds_obj <- change_travel_eir(travel_eir, xds_obj, i)
  return(xds_obj)
}

#' @title Make random travel EIR
#' 
#' @description Set up a random vector 
#' 
#' @param nStrata is the number of population strata
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_travel_eir_random = function(nStrata, options=list(), f_rand = stats::rlnorm, p1=log(1/3650), p2=.2) {with(options,{
  travel_eir <- f_rand(nStrata, p1, p2)
  return(travel_eir)
})}

#' @title Update the travel eir 
#' @description Port function for travel EIR.
#' Dispatches on `class(xds_obj$XH_obj[[i]]$travel_eir_obj)`.
#' @seealso [travel_eir], [importation_dynamics]
#' 
#' @param t time
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_travel_eir <- function(t, xds_obj, i) {
  UseMethod("update_travel_eir", xds_obj$XH_obj[[i]]$travel_eir_obj)
}

#' @title Update travel_eir 
#' 
#' @description Return `xds_obj` unchanged for a static travel EIR port.
#' 
#' @inheritParams update_travel_eir
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_travel_eir.static <- function(t, xds_obj, i) {return(xds_obj)}


#' @title Get the travel eir 
#'
#' @seealso [travel_eir], [setup_travel_eir], [change_travel_eir],
#'   [travel_malaria], [malaria_importation]
#'
#' @note Travel EIR is stored in `xds_obj$XH_obj[[i]]$travel_eir`.
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return the travel eir, a vector of length nStrata
#' @export
get_travel_eir = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$travel_eir)
}
