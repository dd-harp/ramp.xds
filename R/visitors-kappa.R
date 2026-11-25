# X

#' @title Check vis_kappa
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + all elements are less than or equal to one
#' + `length(vis_kappa) == nP`
#' 
#' @param vis_kappa net infectiousness of the visitor population
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_vis_kappa = function(vis_kappa, nP){
  stopifnot(is.numeric(vis_kappa))
  stopifnot(vis_kappa >= 0)
  stopifnot(vis_kappa <= 1)
  stopifnot(length(vis_kappa) == nP) 
}

#' @title Change the vis_kappa

#' @description
#' Update the vis_kappa for the \eqn{s^{th}} vector species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param vis_kappa the net infectiousness of the visitor population
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @seealso [visitor_infectiousness], [setup_vis_kappa], [get_vis_kappa],
#'   [malaria_importation]
#'
#' @return an **`xds`** model object
#' 
#' @export
change_vis_kappa = function(vis_kappa, xds_obj, s=1){
  check_vis_kappa(vis_kappa, xds_obj$nPatches)
  xds_obj$patches$vis_kappa[[s]] <- vis_kappa
  xds_obj$patches$vis_kappa_obj[[s]] <- make_static_obj()
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- compute_kappa(0, y, xds_obj)
  return(xds_obj)
}

#' @title Set up visitor net infectiousness
#'
#' @inherit visitor_infectiousness description
#' @note Visitor net infectiousness is stored in `xds_obj$patches$vis_kappa[[s]]`.
#' @param name a numeric vector, a setup function name, or an options list
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @seealso [visitor_infectiousness], [get_vis_kappa], [change_vis_kappa],
#'   [malaria_importation], [update_vis_kappa]
#'
#' @section Setup methods:
#' + `as_is` uses a supplied visitor net infectiousness vector.
#' + `random` generates a random visitor net infectiousness vector for each patch.
#'
#' @export
setup_vis_kappa = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_vis_kappa", options)
}

#' @title Set up visitor net infectiousness
#'
#' @details Set up the net infectiousness of visitors using an options list.
#'
#' @note If an options list is passed
#' as the first argument, then set 
#' + `Vname = name$name` 
#' + `options = name`
#' and call `setup_vis_kappa(Vname, xds_obj, options, s)`.
#'
#' @inherit setup_vis_kappa description params seealso
#'
#' @return an **`xds`** model object
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
#' @details Don't change anything.
#' @inherit setup_vis_kappa description params seealso
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_vis_kappa.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up visitor net infectiousness
#' 
#' @details Implements the "random" case for [setup_vis_kappa]. See [make_vis_kappa_random].
#' 
#' @inherit setup_vis_kappa description params seealso
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_vis_kappa.random = function(name, xds_obj, options = list(), s=1){
  vis_kappa = make_vis_kappa_random(xds_obj$nPatches, options)
  xds_obj <- change_vis_kappa(vis_kappa, xds_obj, s)
  return(xds_obj)
}

#' @title Make random values for vis_kappa 
#' 
#' @description Set up a random vector.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a [vector]
#' @export
make_vis_kappa_random = function(nPatches, options=list(), f_rand = stats::rbeta, p1=20, p2=1000) {with(options,{
  vis_kappa <- f_rand(nPatches, p1, p2)
  return(vis_kappa)
})}

#' @title Set up visitor net infectiousness
#' @details
#' Pass a pre-configured vis_kappa. If it passes 
#' the checks, it replaces the current vis_kappa.
#' 
#' If called with `name = "as_is"`, the vis_kappa
#' must be at `options$vis_kappa_asis`.
#' 
#' @inherit setup_vis_kappa description params seealso
#' @return an **`xds`** model object
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
#' @description Port function to compute the net infectiousness of visitors.
#' @seealso [visitor_infectiousness], [importation_dynamics]
#' @note Dispatches on `class(xds_obj$patches$vis_kappa_obj[[s]])`.
#' 
#' @param t time
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_vis_kappa <- function(t, xds_obj, s){
  UseMethod("update_vis_kappa",  xds_obj$patches$vis_kappa_obj[[s]])
}

#' @title Update NI of visitors
#' 
#' @description The `static` method returns `xds_obj` unmodified.
#' @inheritParams update_vis_kappa
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_vis_kappa.static <- function(t, xds_obj, s) { return(xds_obj) }

#' @title Get net infectiousness of visitors
#'
#' @seealso [visitor_infectiousness], [setup_vis_kappa],
#'   [change_vis_kappa], [malaria_importation]
#' @note Visitor net infectiousness is stored in `xds_obj$patches$vis_kappa[[s]]`.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [vector]
#' @export
get_vis_kappa = function(xds_obj, s=1){
  return(xds_obj$patches$vis_kappa[[s]])
}
