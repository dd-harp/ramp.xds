#' @title Check habitat search weights
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(habitat_search_weights) == nHabitats
#' 
#' 
#' @param search_weights habitat search weights
#' @param nQ the number of habitats
#' 
#' @seealso [xds_info_egg_laying]
#' @export
check_habitat_search_weights = function(search_weights, nQ){
  stopifnot(is.numeric(search_weights))
  stopifnot(search_weights >= 0)
  stopifnot(length(search_weights) == nQ) 
}

#' @title Change the habitat_search_weights
#' 
#' @description
#' Update the habitat_search_weights for the \eqn{i^{th}} host species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param search_weights habitat search weights
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_habitat_search_weights = function(search_weights, xds_obj, s=1){
  check_habitat_search_weights(search_weights, xds_obj$nHabitats)
  xds_obj$L_obj[[s]]$search_weights <- search_weights
  xds_obj$L_obj[[s]]$search_obj <- make_static_obj()
  xds_obj <- compute_Qall(xds_obj)
  xds_obj <- compute_O_matrix(xds_obj)
  return(xds_obj)
}

#' @title Set up a habitat_search_weights
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param s the mosquito species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_habitat_search_weights = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_habitat_search_weights", options)
}

#' @title Set up a habitat_search_weights
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `HSWname = name$name` 
#' + `options = name`
#' and call `setup_habitat_search_weights(HSWname, xds_obj, options, s)` 
#'
#' @inheritParams setup_habitat_search_weights
#'
#' @return a numeric vector
#' 
#' @keywords internal
#' @export
setup_habitat_search_weights.list = function(name, xds_obj, options=list(), s=1){
  options = name
  HSWname = name$name
  if(is.null(HSWname)) HSWname = "no_setup"
  xds_obj <- setup_habitat_search_weights(HSWname, xds_obj, options, s)
  return(xds_obj)
}


#' @title Set habitat search weights
#' @description The default version called by setup 
#' @inheritParams setup_habitat_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_habitat_search_weights.setup = function(name, xds_obj, options = list(), s=1){
  stopifnot(with(options, exists("membership")))
  membership = options$membership
  xds_obj$L_obj[[s]]$membership = membership
  wts <- 0*membership+1
  xds_obj <- change_habitat_search_weights(wts, xds_obj, s)
  return(xds_obj)
}

#' @title Set up no habitat_search_weights 
#' @description Don't change anything 
#' @inheritParams setup_habitat_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_habitat_search_weights.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up a habitat_search_weights
#' 
#' @description Implements the "method1" case for [setup_habitat_search_weights]. See [make_habitat_search_weights_random]
#' 
#' @inheritParams setup_habitat_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_habitat_search_weights.random = function(name, xds_obj, options = list(), s=1){
  habitat_search_weights = make_habitat_search_weights_random(xds_obj$nPatches, options)
  xds_obj <- change_habitat_search_weights(habitat_search_weights, xds_obj, s)
  return(xds_obj)
}

#' @title Make habitat search weights 
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
make_habitat_search_weights_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  habitat_search_weights <- f_rand(nPatches, p1, p2)
  return(habitat_search_weights)
})}

#' @title Set up a habitat_search_weights
#' @description 
#' Pass a pre-configured habitat_search_weights. If it passes 
#' the checks, it replaces the current habitat_search_weights.
#' 
#' If called with `name = "as_is"`, the habitat_search_weights
#' must be at `options$habitat_search_weights` 
#' 
#' @inheritParams setup_habitat_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_habitat_search_weights.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    habitat_search_weights_as_is = options$weights
  if(is.numeric(name))
    habitat_search_weights_as_is = as.vector(name)
  
  xds_obj <- change_habitat_search_weights(habitat_search_weights_as_is, xds_obj, s)
  
  return(xds_obj)
}

#' @title Update the habitat_search_weights Matrix
#' @description Port function for the habitat_search_weights, \eqn{w}.
#' Dispatches on `xds_obj$L_obj[[s]]$habitat_search_obj`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_habitat_search_weights <- function(t, y, xds_obj, s){
  UseMethod("update_habitat_search_weights", xds_obj$L_obj[[s]]$search_obj)
}

#' @title Update the habitat_search_weights
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_habitat_search_weights
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_habitat_search_weights.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title habitat_search_weights
#' 
#' @description The `S3` definition for the
#' function that computes habitat_search_weights
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'  
#' @return an **`xds`** object
#' @keywords internal
#' @export
habitat_search_weights = function(t, y, xds_obj, s){
  UseMethod("habitat_search_weights", xds_obj$L_obj[[s]]$search_obj)
}

#' @title Compute availability of habitat search weights 
#'
#' @description This sets up... 
#' 
#' @inheritParams habitat_search_weights
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
habitat_search_weights.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Get availability of habitat search weights
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a vector 
#' @export
get_habitat_search_weights = function(xds_obj, s=1){
  return(xds_obj$L_obj[[s]]$search_weights)
}


