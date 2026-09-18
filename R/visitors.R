#' @title Check visitors
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(visitors) = nPatches
#' 
#' 
#' @param visitors the availability of visitors
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_visitors = function(visitors, nP){
  stopifnot(is.numeric(visitors))
  stopifnot(visitors >= 0)
  stopifnot(length(visitors) == nP) 
}

#' @title Change the visitors

#' @description
#' Update the visitors for the \eqn{s^{th}} vector species. 
#' 
#' @param visitors a visitors
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_visitors = function(visitors, xds_obj, s=1){
  check_visitors(visitors, xds_obj$nPatches)
  xds_obj$patches$visitors[[s]] <- visitors
  xds_obj$patches$visitors_obj[[s]] <- make_static_obj()
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- compute_local_frac(xds_obj)
  xds_obj <- compute_beta(0, y, xds_obj)
  xds_obj <- compute_EIR(0, y, xds_obj)
  xds_obj <- compute_kappa(0, y, xds_obj)
  return(xds_obj)
}

#' @title Set up a visitors
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the host species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_visitors = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_visitors", options)
}

#' @title Set up a visitors
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Vname = name$name` 
#' + `options = name`
#' and call `setup_visitors(Vname, xds_obj, options, s)` 
#'
#' @inheritParams setup_visitors
#'
#' @return a [matrix]
#' @keywords internal
#' @export
setup_visitors.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Vname = name$name
  if(is.null(Vname)) Vname = "no_setup"
  xds_obj <- setup_visitors(Vname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no visitors 
#' @description Don't change anything 
#' @inheritParams setup_visitors
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_visitors.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up a visitors
#' 
#' @description Implements the "random" case for [setup_visitors]. See [make_visitors_random]
#' 
#' @inheritParams setup_visitors
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_visitors.random = function(name, xds_obj, options = list(), s=1){
  visitors = make_visitors_random(xds_obj$nPatches, options)
  xds_obj <- change_visitors(visitors, xds_obj, s)
  return(xds_obj)
}

#' @title Make visitors 
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
make_visitors_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  visitors <- f_rand(nPatches, p1, p2)
  return(visitors)
})}

#' @title Set up a visitors
#' @description 
#' Pass a pre-configured visitors. If it passes 
#' the checks, it replaces the current visitors.
#' 
#' If called with `name = "as_is"`, the visitors
#' must be at `options$visitors` 
#' 
#' @inheritParams setup_visitors
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_visitors.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    visitors_as_is = options$visitors_asis
  if(is.numeric(name))
    visitors_as_is = as.vector(name)
  
  xds_obj <- change_visitors(visitors_as_is, xds_obj, s)
  
  return(xds_obj)
}

#' @title Update visitors
#' 
#' @description Port function to compute the availability of visitors.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_visitors <- function(t, y, xds_obj, s){
  UseMethod("update_visitors",  xds_obj$patches$visitors_obj[[s]])
}

#' @title Update visitors
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_visitors
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_visitors.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Get availability of visitors
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix]
#' @export
get_visitors = function(xds_obj, s=1){
  return(xds_obj$patches$visitors[[s]])
}


