#' @title Check vis_kappa
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(vis_kappa) = nPatches
#' 
#' @param vis_kappa net infectiousness of the visitor population
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_vis_kappa = function(vis_kappa, nP){
  stopifnot(is.numeric(vis_kappa))
  stopifnot(vis_kappa >= 0)
  stopifnot(length(vis_kappa) == nP) 
}

#' @title Change the vis_kappa

#' @description
#' Update the vis_kappa for the \eqn{s^{th}} vector species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param vis_kappa a vis_kappa
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_vis_kappa = function(vis_kappa, xds_obj, s=1){
  check_vis_kappa(vis_kappa, xds_obj$nPatches)
  xds_obj$patches$vis_kappa[[s]] <- vis_kappa
  return(xds_obj)
}

#' @title Set up a vis_kappa
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the host species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_vis_kappa = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_vis_kappa", options)
}

#' @title Set up a vis_kappa
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Vname = name$name` 
#' + `options = name`
#' and call `setup_vis_kappa(Vname, xds_obj, options, s)` 
#'
#' @inheritParams setup_vis_kappa
#'
#' @return a [matrix]
#' @keywords internal
#' @export
setup_vis_kappa.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Vname = name$name
  if(is.null(Vname)) Vname = "no_setup"
  xds_obj <- setup_vis_kappa(Vname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no vis_kappa 
#' @description Don't change anything 
#' @inheritParams setup_vis_kappa
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_vis_kappa.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up a vis_kappa
#' 
#' @description Implements the "random" case for [setup_vis_kappa]. See [make_vis_kappa_random]
#' 
#' @inheritParams setup_vis_kappa
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_vis_kappa.random = function(name, xds_obj, options = list(), s=1){
  vis_kappa = make_vis_kappa_random(xds_obj$nPatches, options)
  xds_obj <- change_vis_kappa(vis_kappa, xds_obj, s)
  return(xds_obj)
}

#' @title Make random values for vis_kappa 
#' 
#' @description Set up a random vector 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a [matrix]
#' @export
make_vis_kappa_random = function(nPatches, options=list(), f_rand = stats::rbeta, p1=20, p2=1000) {with(options,{
  vis_kappa <- f_rand(nPatches, p1, p2)
  return(vis_kappa)
})}

#' @title Set up a vis_kappa
#' @description 
#' Pass a pre-configured vis_kappa. If it passes 
#' the checks, it replaces the current vis_kappa.
#' 
#' If called with `name = "as_is"`, the vis_kappa
#' must be at `options$vis_kappa` 
#' 
#' @inheritParams setup_vis_kappa
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_vis_kappa.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    vis_kappa_as_is = options$vis_kappa_asis
  if(is.numeric(name))
    vis_kappa_as_is = as.vector(name)
  
  xds_obj <- change_vis_kappa(vis_kappa_as_is, xds_obj, s)
  
  return(xds_obj)
}

#' @title Update NI of visitors
#' 
#' @description Port function to compute the availability of vis_kappa.
#' Dispatches on `class(xds_obj$patches$vis_kappa_obj[[s]])`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_vis_kappa <- function(t, y, xds_obj, s){
  UseMethod("update_vis_kappa",  xds_obj$patches$vis_kappa_obj[[s]])
}

#' @title Update NI of visitors
#' 
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_vis_kappa
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_vis_kappa.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Compute NI of visitors
#' 
#' @description The `S3` definition for the
#' function that computes vis_kappa
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
vis_kappa = function(t, y, xds_obj, s){
  UseMethod("vis_kappa", xds_obj$patches$vis_kappa_obj[[s]])
}

#' @title Compute NI of visitors
#'
#' @description This sets up... 
#' 
#' @inheritParams vis_kappa
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
vis_kappa.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Get availability of vis_kappa
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix]
#' @export
get_vis_kappa = function(xds_obj, s=1){
  return(xds_obj$patches$vis_kappa[[s]])
}


