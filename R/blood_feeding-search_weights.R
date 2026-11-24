#' @title Check the blood search weights
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(blood_search_weights) == nStrata
#' 
#' @param blood_search_weights search weights for hosts
#' @param nStrata the number of population strata
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_blood_search_weights = function(blood_search_weights, nStrata){
  stopifnot(is.numeric(blood_search_weights))
  stopifnot(blood_search_weights >= 0)
  stopifnot(length(blood_search_weights) == nStrata) 
}

#' @title Change the blood search weights
#' 
#' @description Change the static blood search weights for host species `i`
#' and recompute derived blood-feeding and transmission terms.
#' 
#' @param blood_search_weights a numeric vector with one weight per population stratum
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** model object
#' 
#' @export
change_blood_search_weights = function(blood_search_weights, xds_obj, i=1){
  check_blood_search_weights(blood_search_weights, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$search_weights <- blood_search_weights
  xds_obj$XH_obj[[i]]$search_weights_obj <- make_static_obj()
  xds_obj <- update_XY_interface(xds_obj)
  return(xds_obj)
}

#' @title Set up blood search weights
#' 
#' @description Advanced setup for heterogeneous biting. Useful methods include:
#' + `"as_is"` sets the supplied vector of weights
#' + `"random"` generates random search weights (see [make_blood_search_weights_random])
#' @param name a setup method name or a numeric vector of weights
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** model object
#' 
#' @seealso [xds_setup], [xds_info_search_weights_blood]
#' @export
setup_blood_search_weights = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_blood_search_weights", options)
}

#' @title Set up blood search weights
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `BSWname = name$name` 
#' + `options = name`
#' and call `setup_blood_search_weights(BSWname, xds_obj, options, i)` 
#'
#' @inheritParams setup_blood_search_weights
#'
#' @return an **`xds`** model object
#' 
#' @keywords internal
#' @export
setup_blood_search_weights.list = function(name, xds_obj, options=list(), i=1){
  options = name
  BSWname = name$name
  if(is.null(BSWname)) BSWname = "no_setup"
  xds_obj <- setup_blood_search_weights(BSWname, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up default blood search weights
#' @description Initialize unit blood search weights for host species `i`.
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_blood_search_weights.setup = function(name, xds_obj, options = list(), i=1){
  blood_search_weights = rep(1, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$search_weights <- blood_search_weights
  xds_obj$XH_obj[[i]]$search_weights_obj <- make_static_obj()
  xds_obj$XH_obj[[i]]$search_weights_obj$search_weights <- blood_search_weights
  return(xds_obj)
}

#' @title Leave blood search weights unchanged
#' @description Return `xds_obj` without changing the configured search weights.
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_blood_search_weights.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up random blood search weights
#' 
#' @description Generate random blood search weights using
#' [make_blood_search_weights_random].
#' 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_blood_search_weights.random = function(name, xds_obj, options = list(), i=1){
  blood_search_weights = make_blood_search_weights_random(xds_obj$nStrata[i], options)
  xds_obj <- change_blood_search_weights(blood_search_weights, xds_obj, i)
  return(xds_obj)
}

#' @title Make blood search weights 
#' 
#' @description Set up a random vector of blood search weights
#' 
#' @param nStrata is the number of population strata
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' 
#' @return a numeric vector
#' 
#' @seealso [setup_blood_search_weights]
#' 
#' @export
make_blood_search_weights_random = function(nStrata, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  blood_search_weights <- f_rand(nStrata, p1, p2)
  return(blood_search_weights)
})}

#' @title Set up blood search weights from supplied values
#' @description 
#' Pass a pre-configured blood_search_weights. If it passes 
#' the checks, it replaces the current blood_search_weights.
#' 
#' If the first argument to `setup_blood_search_weights` is a
#' numeric vector, the `"as_is"` method is used. 
#' 
#' When `name = "as_is"`, pass the new vector in `options$weights`.
#' 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_blood_search_weights.as_is = function(name, xds_obj, options=list(), i=1){
  
  if(is.list(options)) 
    blood_search_weights_as_is = options$weights
  if(is.numeric(name))
    blood_search_weights_as_is = as.vector(name)
  
  xds_obj <- change_blood_search_weights(blood_search_weights_as_is, xds_obj, i)
  
  return(xds_obj)
}

#' @title Update the blood search weights
#' 
#' @description Dispatch the update for blood search weights of host species
#' `i`.
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_blood_search_weights <- function(t, y, xds_obj, i){
  UseMethod("update_blood_search_weights", xds_obj$XH_obj[[i]]$search_weights_obj)
}

#' @title Update the blood_search_weights
#' @description The static method returns `xds_obj` unmodified.
#' @inheritParams update_blood_search_weights
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_blood_search_weights.static <- function(t, y, xds_obj, i) { return(xds_obj) }

#' @title Get blood search weights
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a numeric vector with one weight per population stratum
#' @export
get_blood_search_weights = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$search_weights)
}
