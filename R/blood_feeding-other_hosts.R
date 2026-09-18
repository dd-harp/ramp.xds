#' @title Check other blood hosts
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(other_blood_hosts) == nPatc
#' 
#' 
#' @param other_blood_hosts availbility of other blood hosts
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_other_blood_hosts = function(other_blood_hosts, nP){
  stopifnot(is.numeric(other_blood_hosts))
  stopifnot(other_blood_hosts >= 0)
  stopifnot(length(other_blood_hosts) == nP) 
}

#' @title Change the other_blood_hosts

#' @description
#' Update the other_blood_hosts for the \eqn{i^{th}} host species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param other_blood_hosts a other_blood_hosts
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_other_blood_hosts = function(other_blood_hosts, xds_obj, s=1){
  check_other_blood_hosts(other_blood_hosts, xds_obj$nPatches)
  xds_obj$patches$other_blood_hosts[[s]] <- other_blood_hosts
  xds_obj$patches$other_blood_hosts_obj[[s]] <- make_static_obj()
  return(xds_obj)
}

#' @title Set up a other_blood_hosts
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_other_blood_hosts = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_other_blood_hosts", options)
}

#' @title Set up a other_blood_hosts
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `OBHname = name$name` 
#' + `options = name`
#' and call `setup_other_blood_hosts(OBHname, xds_obj, options, s)` 
#'
#' @inheritParams setup_other_blood_hosts
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_other_blood_hosts.list = function(name, xds_obj, options=list(), s=1){
  options = name
  OBHname = name$name
  if(is.null(OBHname)) OBHname = "no_setup"
  xds_obj <- setup_other_blood_hosts(OBHname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no other_blood_hosts 
#' @description Don't change anything 
#' @inheritParams setup_other_blood_hosts
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_other_blood_hosts.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up a other_blood_hosts
#' 
#' @description Implements the "method1" case for [setup_other_blood_hosts]. See [make_other_blood_hosts_random]
#' 
#' @inheritParams setup_other_blood_hosts
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_other_blood_hosts.random = function(name, xds_obj, options = list(), s=1){
  other_blood_hosts = make_other_blood_hosts_random(xds_obj$nPatches, options)
  xds_obj <- change_other_blood_hosts(other_blood_hosts, xds_obj, s)
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
make_other_blood_hosts_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  other_blood_hosts <- f_rand(nPatches, p1, p2)
  return(other_blood_hosts)
})}

#' @title Set up a other_blood_hosts
#' @description 
#' Pass a pre-configured other_blood_hosts. If it passes 
#' the checks, it replaces the current other_blood_hosts.
#' 
#' If called with `name = "as_is"`, the other_blood_hosts
#' must be at `options$other_blood_hosts` 
#' 
#' @inheritParams setup_other_blood_hosts
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_other_blood_hosts.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    other_blood_hosts_as_is = options$other_blood_hosts_asis
  if(is.numeric(name))
    other_blood_hosts_as_is = as.vector(name)
  
  xds_obj <- change_other_blood_hosts(other_blood_hosts_as_is, xds_obj, s)
  
  return(xds_obj)
}

#' @title Update other blood hosts 
#' 
#' @description Port function for the other_blood_hosts, \eqn{\Theta}.
#' Dispatches on `class(xds_obj$XY_interface$other_blood_hosts_obj[[s]])`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_other_blood_hosts <- function(t, y, xds_obj, s){
  UseMethod("update_other_blood_hosts", xds_obj$patches$other_blood_hosts_obj[[s]])
}

#' @title Update other blood hosts 
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_other_blood_hosts
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_other_blood_hosts.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Get availability of other blood hosts
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix]
#' @export
get_other_blood_hosts = function(xds_obj, s=1){
  return(xds_obj$XY_interface$other_blood_hosts[[s]])
}


