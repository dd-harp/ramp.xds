#' @title Check other blood host availability
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(other_blood_hosts) == nP
#' 
#' 
#' @param other_blood_hosts availability of other blood hosts
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_other_blood_hosts = function(other_blood_hosts, nP){
  stopifnot(is.numeric(other_blood_hosts))
  stopifnot(other_blood_hosts >= 0)
  stopifnot(length(other_blood_hosts) == nP) 
}

#' @title Change other blood host availability

#' @description Set other blood host availability for vector species `s`.
#' 
#' @param other_blood_hosts a numeric vector of patch-level availability
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @export
change_other_blood_hosts = function(other_blood_hosts, xds_obj, s=1){
  check_other_blood_hosts(other_blood_hosts, xds_obj$nPatches)
  xds_obj$patches$other_blood_hosts[[s]] <- other_blood_hosts
  xds_obj$patches$other_blood_hosts_obj[[s]] <- make_static_obj()
  xds_obj$patches$other_blood_hosts_obj[[s]]$other_blood_hosts <- other_blood_hosts
  return(xds_obj)
}

#' @title Set up other blood hosts
#'
#' @param name a setup method name or a numeric vector of availability values
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_other_blood_hosts = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_other_blood_hosts", options)
}

#' @title Set up other blood hosts
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `OBHname = name$name` 
#' + `options = name`
#' and call `setup_other_blood_hosts(OBHname, xds_obj, options, s)` 
#'
#' @inheritParams setup_other_blood_hosts
#'
#' @return an **`xds`** model object
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

#' @title Leave other blood host availability unchanged
#' @description Return `xds_obj` without changing the configured availability.
#' @inheritParams setup_other_blood_hosts
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_other_blood_hosts.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up other blood hosts
#' 
#' @description Generate random other blood host availability using
#' [make_other_blood_hosts_random].
#' 
#' @inheritParams setup_other_blood_hosts
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_other_blood_hosts.random = function(name, xds_obj, options = list(), s=1){
  other_blood_hosts = make_other_blood_hosts_random(xds_obj$nPatches, options)
  xds_obj <- change_other_blood_hosts(other_blood_hosts, xds_obj, s)
  return(xds_obj)
}

#' @title Make random other blood host availability
#' 
#' @description Generate a random vector of other blood host availability.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_other_blood_hosts_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  other_blood_hosts <- f_rand(nPatches, p1, p2)
  return(other_blood_hosts)
})}

#' @title Set up other blood hosts
#' @description 
#' Pass a pre-configured other_blood_hosts. If it passes 
#' the checks, it replaces the current other_blood_hosts.
#' 
#' If called with `name = "as_is"`, provide the vector in
#' `options$other_blood_hosts_asis`.
#' 
#' @inheritParams setup_other_blood_hosts
#' @return an **`xds`** model object
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

#' @title Update other blood host availability
#' 
#' @description Dispatch the update for other blood host availability of
#' vector species `s`.
#' Dispatches on `class(xds_obj$patches$other_blood_hosts_obj[[s]])`.
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_other_blood_hosts <- function(t, y, xds_obj, s){
  UseMethod("update_other_blood_hosts", xds_obj$patches$other_blood_hosts_obj[[s]])
}

#' @title Update other blood host availability
#' @description The static method returns `xds_obj` unmodified.
#' @inheritParams update_other_blood_hosts
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_other_blood_hosts.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Get other blood host availability
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a numeric vector of patch-level availability
#' @export
get_other_blood_hosts = function(xds_obj, s=1){
  return(xds_obj$patches$other_blood_hosts[[s]])
}
