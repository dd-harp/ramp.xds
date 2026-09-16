#' @title Check blood search weights
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(blood_search_weights) == nPatches
#' 
#' 
#' @param blood_search_weights search weights for hosts
#' @param nS the number of population strata
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_blood_search_weights = function(blood_search_weights, nS){
  stopifnot(is.numeric(blood_search_weights))
  stopifnot(blood_search_weights >= 0)
  stopifnot(length(blood_search_weights) == nS) 
}

#' @title Change the blood_search_weights
#' 
#' @description
#' Update the blood_search_weights for the \eqn{i^{th}} host species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param blood_search_weights a blood_search_weights
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_blood_search_weights = function(blood_search_weights, xds_obj, i=1){
  check_blood_search_weights(blood_search_weights, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$search_weights <- blood_search_weights
  xds_obj$XH_obj[[i]]$search_weights_obj <- make_static_obj()
  return(xds_obj)
}

#' @title Set up a blood_search_weights
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_blood_search_weights = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_blood_search_weights", options)
}

#' @title Set up a blood_search_weights
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `BSWname = name$name` 
#' + `options = name`
#' and call `setup_blood_search_weights(BSWname, xds_obj, options, s)` 
#'
#' @inheritParams setup_blood_search_weights
#'
#' @return a numeric vector
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

#' @title Set up no blood_search_weights 
#' @description Don't change anything 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.default = function(name, xds_obj, options = list(), i=1){
  blood_search_weights = rep(1, xds_obj$nStrata[i])
  xds_obj <- change_blood_search_weights(blood_search_weights, xds_obj, i)
  return(xds_obj)
}

#' @title Set up no blood_search_weights 
#' @description Don't change anything 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up a blood_search_weights
#' 
#' @description Implements the "method1" case for [setup_blood_search_weights]. See [make_blood_search_weights_random]
#' 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.random = function(name, xds_obj, options = list(), i=1){
  blood_search_weights = make_blood_search_weights_random(xds_obj$nPatches, options)
  xds_obj <- change_blood_search_weights(blood_search_weights, xds_obj, i)
  return(xds_obj)
}

#' @title Make blood search weights 
#' 
#' @description Set up a random vector 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_blood_search_weights_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  blood_search_weights <- f_rand(nPatches, p1, p2)
  return(blood_search_weights)
})}

#' @title Set up a blood_search_weights
#' @description 
#' Pass a pre-configured blood_search_weights. If it passes 
#' the checks, it replaces the current blood_search_weights.
#' 
#' If called with `name = "as_is"`, the blood_search_weights
#' must be at `options$blood_search_weights` 
#' 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
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

#' @title Update the blood_search_weights Matrix
#' @description Port function for the blood_search_weights, \eqn{w}.
#' Dispatches on `xds_obj$XH_obj[[i]]$blood_search_weights_obj`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights <- function(t, y, xds_obj, i){
  UseMethod("update_blood_search_weights", xds_obj$XH_obj[[i]]$search_weights_obj)
}

#' @title Update the blood_search_weights
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_blood_search_weights
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights.static <- function(t, y, xds_obj, i) { return(xds_obj) }

#' @title blood_search_weights
#' 
#' @description The `S3` definition for the
#' function that computes blood_search_weights
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param s the vector species index
#'  
#' @return an **`xds`** object
#' @keywords internal
#' @export
blood_search_weights = function(t, y, xds_obj, i, s){
  UseMethod("blood_search_weights", xds_obj$XH_obj[[i]]$search_weights_obj)
}

#' @title Compute blood search weights 
#'
#' @description This sets up... 
#' 
#' @inheritParams blood_search_weights
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
blood_search_weights.static = function(t, y, xds_obj, i, s){
  return(return(xds_obj))
}

#' @title Get availability of blood search weights
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param s the vector species index
#'
#' @return a vector 
#' @export
get_blood_search_weights = function(xds_obj, i=1, s=1){
  return(xds_obj$XH_obj[[i]]$search_weights[[s]])
}


