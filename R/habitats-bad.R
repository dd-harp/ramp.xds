#' @title Check other blood hosts
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(bad_habitats) == nPatches
#' 
#' 
#' @param bad_habitats bad habitat availability
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_bad_habitats = function(bad_habitats, nP){
  stopifnot(is.numeric(bad_habitats))
  stopifnot(bad_habitats >= 0)
  stopifnot(length(bad_habitats) == nP) 
}

#' @title Change the bad_habitats

#' @description
#' Update the bad habitats for the \eqn{s^{th}} vector species, and 
#' trigger updates for the `ML` interface. 
#' 
#' @param bad_habitats bad habitat availability
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_bad_habitats = function(bad_habitats, xds_obj, s=1){
  check_bad_habitats(bad_habitats, xds_obj$nPatches)
  xds_obj$patches$bad_habitats[[s]] <- bad_habitats
  xds_obj$patches$bad_habitats_obj[[s]] <- make_static_obj()
  xds_obj <- compute_Qall(xds_obj)
  xds_obj <- compute_O_matrix(xds_obj)
  return(xds_obj)
}

#' @title Set up bad habitats
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_bad_habitats = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_bad_habitats", options)
}

#' @title Set up bad habitats
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `BHname = name$name` 
#' + `options = name`
#' and call `setup_bad_habitats(BHname, xds_obj, options, s)` 
#'
#' @inheritParams setup_bad_habitats
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_bad_habitats.list = function(name, xds_obj, options=list(), s=1){
  options = name
  BHname = name$name
  if(is.null(BHname)) BHname = "no_setup"
  xds_obj <- setup_bad_habitats(BHname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no bad_habitats 
#' @description Don't change anything 
#' @inheritParams setup_bad_habitats
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_bad_habitats.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up bad habitats
#' 
#' @description Implements the "method1" case for [setup_bad_habitats]. See [make_bad_habitats_random]
#' 
#' @inheritParams setup_bad_habitats
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_bad_habitats.random = function(name, xds_obj, options = list(), s=1){
  bad_habitats = make_bad_habitats_random(xds_obj$nPatches, options)
  xds_obj <- change_bad_habitats(bad_habitats, xds_obj, s)
  return(xds_obj)
}

#' @title Make other blood hosts 
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
make_bad_habitats_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  bad_habitats <- f_rand(nPatches, p1, p2)
  return(bad_habitats)
})}

#' @title Set up bad habitats
#' @description 
#' Pass a pre-configured bad_habitats. If it passes 
#' the checks, it replaces the current bad_habitats.
#' 
#' If called with `name = "as_is"`, the bad_habitats
#' must be at `options$bad_habitats` 
#' 
#' @inheritParams setup_bad_habitats
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_bad_habitats.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    bad_habitats_as_is = options$bad_habitats_asis
  if(is.numeric(name))
    bad_habitats_as_is = as.vector(name)
  
  xds_obj <- change_bad_habitats(bad_habitats_as_is, xds_obj, s)
  
  return(xds_obj)
}

#' @title Update other blood hosts 
#' 
#' @description Port function for the bad_habitats, \eqn{\Theta}.
#' Dispatches on `class(xds_obj$patches$bad_habitats_obj[[s]])`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_bad_habitats <- function(t, y, xds_obj, s){
  UseMethod("update_bad_habitats", xds_obj$patches$bad_habitats_obj[[s]])
}

#' @title Update other blood hosts 
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_bad_habitats
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_bad_habitats.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Other blood hosts
#' 
#' @description The `S3` definition for the
#' function that computes bad_habitats
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
bad_habitats = function(t, y, xds_obj, s){
  UseMethod("bad_habitats", xds_obj$patches$bad_habitats_obj[[s]])
}

#' @title Compute availability of other blood hosts 
#'
#' @description This sets up... 
#' 
#' @inheritParams bad_habitats
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
bad_habitats.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Get availability of other blood hosts
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix]
#' @export
get_bad_habitats = function(xds_obj, s=1){
  return(xds_obj$patches$bad_habitats[[s]])
}


