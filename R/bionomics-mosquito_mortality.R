
#' @title Set up a mosquito mortality bionomic object
#'
#' @description Set up an object to return a
#' constant baseline mosquito mortality rate, \eqn{g}
#'
#' @param g the mosquito mortality rate
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_g_obj = function(g, MY_obj){
  MY_obj$g = g
  MY_obj$g_t = g
  MY_obj$es_g = 1
  MY_obj$g_obj <- list()
  class(MY_obj$g_obj) <- "static"
  MY_obj$g_obj$g <- g
  return(MY_obj)
}

#' @title Compute mosquito mortality
#'
#' @description This method dispatches on the type of `g_obj`. It should
#' compute the baseline mosquito mortality rate, \eqn{g}
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_g = function(t, xds_obj, s) {
  UseMethod("F_g", xds_obj$MY_obj[[s]]$g_obj)
}

#' @title Static model mosquito mortality rate
#' @description Implements [F_g] for a static model
#' @inheritParams F_g
#' @return \eqn{g}, the baseline mosquito mortality rate
#' @keywords internal
#' @export
F_g.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$g_obj$g)
}

#' @title Check mosquito mortality rate
#' 
#' @description
#' Check that the mosquito mortality rates are non-negative and have length
#' `nPatches`.
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the validated mosquito mortality rate
#' @seealso [xds_info_mosquito_demography]
#' @export
check_g = function(xds_obj, s=1){
  g <- F_g(0, xds_obj, s)
  stopifnot(is.numeric(g))
  stopifnot(g >= 0)
  stopifnot(length(g) == xds_obj$nPatches)
  return(g)
}

#' @title Change mosquito mortality rate
#' 
#' @description Update the mosquito mortality rate for the \eqn{s^{th}} vector
#' species and refresh the demographic matrix.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_g = function(xds_obj, s=1){
  g <- check_g(xds_obj, s)
  xds_obj$MY_obj[[s]]$g_obj$g <- g
  xds_obj$MY_obj[[s]]$g <- g
  xds_obj$MY_obj[[s]]$g_t <- g
  xds_obj <- change_Omega(xds_obj, s)
  class(xds_obj$MY_obj[[s]]$g_obj) <- "static"
  return(xds_obj)
}

#' @title Set up mosquito mortality rate
#'
#' @description Set up the mosquito mortality strategy and validate its rates.
#'
#' @param name a mosquito mortality rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_g = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_g(name, xds_obj, options, s)
  xds_obj <- change_g(xds_obj, s)
  return(xds_obj)
}

#' @title Set up F_g
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_F_g = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_g", options)
}

#' @title Set up F_g
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Fg_name = name$name` 
#' + `options = name`
#' and call `setup_F_g(Fg_name, xds_obj, options, s)` 
#'
#' @inheritParams setup_F_g
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_F_g.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Fg_name = name$name
  if(is.null(Fg_name)) Fg_name = "no_setup"
  xds_obj <- setup_F_g(Fg_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no F_g 
#' @description Don't change anything 
#' @inheritParams setup_F_g
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_g.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up static mosquito mortality rate
#'
#' @description Set up supplied static mosquito mortality rates.
#'
#' @inheritParams setup_F_g
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_g.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$g)) {
    g <- options$g
  } else if (is.numeric(name)) {
    g <- name
  } else {
    stop("Supply mortality rates in `options$g` or as numeric `name`.")
  }
  xds_obj$MY_obj[[s]]$g_obj$g <- g
  class(xds_obj$MY_obj[[s]]$g_obj) <- "static"
  return(xds_obj)
}

#' @title Set up random mosquito mortality rate
#' 
#' @description Implements the "random" case for [setup_F_g]. See [make_F_g_random].
#' 
#' @inheritParams setup_F_g
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_g.random = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$g_obj <- make_F_g_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Compute random mosquito mortality rates
#'
#' @description Generate patch-level mosquito mortality rates using the
#' configured random number generator.
#'
#' @inheritParams F_g
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_g.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$g_obj, {
    g <- f_rand(xds_obj$nPatches, p1, p2)
    return(g)
  })
}

#' @title Make random mosquito mortality rates
#' 
#' @description Build a configuration object for random mosquito mortality.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a configuration object for [F_g.random]
#' @export
make_F_g_random = function(nPatches, options=list(), f_rand = stats::rbeta,
                           p1=80, p2=1000) {
  with(options, {
    g_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    class(g_obj) <- "random"
    return(g_obj)
  })
}

