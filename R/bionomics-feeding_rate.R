#' @title Set up blood feeding rates
#'
#' @description Set up an object
#' to compute dynamic blood feeding rates
#'
#' @param f the blood feeding rate
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_f_obj = function(f, MY_obj){
  MY_obj$f = f
  MY_obj$f_t = f
  MY_obj$es_f = 1
  MY_obj$f_obj <- list()
  class(MY_obj$f_obj) <- "static"
  MY_obj$f_obj$f <- f
  return(MY_obj)
}

#' @title Compute the blood feeding rate, f
#'
#' @description 
#' Set the baseline value of the feeding rate, \eqn{f}. 
#'
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_f = function(t, xds_obj, s) {
  UseMethod("F_f", xds_obj$MY_obj[[s]]$f_obj)
}

#' @title Constant baseline blood feeding rate
#'
#' @description 
#' Set or reset the baseline value of the feeding rate, \eqn{f}, to a static
#' value
#'
#' @inheritParams F_f
#'
#' @return \eqn{f}, the baseline blood feeding rate
#' @keywords internal
#' @export
F_f.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$f_obj$f)
}

#' @title Check blood feeding rate
#'
#' @description Check that blood feeding rates are numeric, between zero and
#' one, and have length `nPatches`.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the validated blood feeding rate
#' @seealso [xds_info_blood_feeding]
#' @export
check_f = function(xds_obj, s=1){
  f <- F_f(0, xds_obj, s)
  stopifnot(is.numeric(f))
  stopifnot(f >= 0)
  stopifnot(f <= 1)
  stopifnot(length(f) == xds_obj$nPatches)
  return(f)
}

#' @title Change the blood feeding rate
#'
#' @description 
#' For models where the baseline value is constant, 
#' update the blood feeding rate.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_f = function(xds_obj, s=1){
  f <- check_f(xds_obj, s)
  xds_obj$MY_obj[[s]]$f_obj$f <- f
  xds_obj$MY_obj[[s]]$f <- f
  xds_obj$MY_obj[[s]]$f_t <- f
  class(xds_obj$MY_obj[[s]]$f_obj) <- "static"
  return(xds_obj)
}

#' @title Set up constant blood feeding rate
#'
#' @description Configure and validate models with 
#' constant blood feeding rates. This calls [setup_F_f]
#' to configure a function \eqn{F_f}, and call
#' [change_f]. 
#' 
#' @note Use [setup_F_f] and not [setup_f] if
#' the baseline values of \eqn{f} will vary over time.
#'
#' @param name a blood feeding rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @seealso [setup_F_f]
#' 
#' @export
setup_f = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_f(name, xds_obj, options, s)
  xds_obj <- change_f(xds_obj, s)
  return(xds_obj)
}

#' @title Set up blood feeding rates
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_F_f = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_f", options)
}

#' @title Set up blood feeding rates
#'
#' @description If an options list is passed as the first argument, use its
#' `name` field to select the setup method.
#'
#' + `Ff_name = name$name`
#' + `options = name`
#' and call `setup_F_f(Ff_name, xds_obj, options, s)`.
#'
#' @inheritParams setup_F_f
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_F_f.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Ff_name = name$name
  if(is.null(Ff_name)) Ff_name = "no_setup"
  xds_obj <- setup_F_f(Ff_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up blood feeding rates
#' 
#' @description Leave the existing blood feeding configuration unchanged.
#' 
#' @inheritParams setup_F_f
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_f.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up blood feeding rates
#'
#' @description Change the blood feeding rates to a new
#' set, either passed as the first argument or `name = "as_is"` and
#' the new values as `options$f`
#'
#' @inheritParams setup_F_f
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_f.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$f)) {
    f <- options$f
  } else if (is.numeric(name)) {
    f <- name
  } else {
    stop("Supply blood feeding rates in `options$f` or as numeric `name`.")
  }
  xds_obj$MY_obj[[s]]$f_obj$f <- f
  class(xds_obj$MY_obj[[s]]$f_obj) <- "static"
  return(xds_obj)
}

#' @title Set up blood feeding rates
#'
#' @description Implements the "random" case for [setup_F_f].
#'
#' @inheritParams setup_F_f
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_f.random = function(name, xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$f_obj <- make_F_f_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Compute random blood feeding rates
#'
#' @description Generate patch-level blood feeding rates using the configured
#' random number generator.
#'
#' @inheritParams F_f
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_f.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$f_obj, {
    f <- f_rand(xds_obj$nPatches, p1, p2)
    return(f)
  })
}

#' @title Make random blood feeding rate object
#'
#' @description Build a configuration object for random blood feeding rates.
#'
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for `f_rand`
#' @param p2 argument for f_rand
#' @return a configuration object for [F_f.random]
#' @export
make_F_f_random = function(nPatches, options=list(), f_rand=stats::rbeta,
                           p1=280, p2=1000){
  with(options, {
    f_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    class(f_obj) <- "random"
    return(f_obj)
  })
}


#' @title Compute the blood feeding rate, f
#'
#' @description Implements a type2 functional response to compute 
#' blood feeding rates as a functional response to resource availability:
#' \deqn{F_f(B)= f_x \frac{s_f B}{1+s_f B}}. 
#'
#' @inheritParams F_f
#'
#' @return \eqn{f}, the baseline blood feeding rate
#' @keywords internal
#' @export
F_f.type2 = function(t, xds_obj, s){
  B <- xds_obj$terms$B[[s]]
  with(xds_obj$MY_obj[[s]]$f_obj, {
    f <- fx*sf*B/(1+sf*B)
    return(f)
  })
}

#' @title Set up type 2 blood feeding rates
#'
#' @description Configure a type 2 functional response for blood feeding.
#'
#' @inheritParams setup_F_f
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_f.type2 = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$f_obj <- make_F_f_type2(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Make type 2 blood feeding rate configuration
#'
#' @description Build a configuration object for the type 2 blood feeding
#' response in response to the availability of blood hosts:
#' \deqn{F_f(B)= f_x \frac{s_f B}{1+s_f B}}. 
#'
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param fx the maximum blood feeding rate, \eqn{f_x}
#' @param sf the shape parameter, \eqn{s_f}
#' @return a configuration object for [F_f.type2]
#' @export
make_F_f_type2 = function(nPatches, options=list(), fx=.3, sf=.1){
  with(options, {
    f_obj <- list(fx=fx, sf=sf)
    class(f_obj) <- "type2"
    return(f_obj)
  })
}