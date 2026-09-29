
#' @title Set up a dispersal loss bionomic object
#'
#' @description Set up an object
#' to compute the dispersal loss fraction, \eqn{\mu}
#'
#' @param mu the emigration loss fraction
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_mu_obj = function(mu, MY_obj){
  MY_obj$mu = mu
  MY_obj$mu_t = mu
  MY_obj$es_mu = 1
  MY_obj$mu_obj <- list()
  class(MY_obj$mu_obj) <- "static"
  MY_obj$mu_obj$mu <- mu
  return(MY_obj)
}

#' @title Compute the emigration loss fraction
#'
#' @description This method dispatches on the type of `mu_obj`. It should
#' compute the baseline emigration-loss fraction, \eqn{\mu}
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_mu <- function(t, xds_obj, s){
  UseMethod("F_mu", xds_obj$MY_obj[[s]]$mu_obj)
}

#' @title Static model emigration-loss fraction
#' @description Implements [F_mu] for a static model
#' @inheritParams F_mu
#' @return \eqn{mu}, the baseline emigration-loss fraction
#' @keywords internal
#' @export
F_mu.static <- function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$mu_obj$mu)
}

#' @title Check emigration-loss fraction
#' 
#' @description
#' Check that the emigration-loss fractions are between zero and one and have
#' length `nPatches`.
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the validated emigration-loss fraction
#' 
#' @seealso [xds_info_mosquito_demography]
#' @export
check_mu = function(xds_obj, s=1){
  mu <- F_mu(0, xds_obj, s)
  stopifnot(is.numeric(mu))
  stopifnot(mu >= 0)
  stopifnot(mu <= 1)
  stopifnot(length(mu) == xds_obj$nPatches)
  return(mu)
}

#' @title Change emigration-loss fraction
#' 
#' @description Update the emigration-loss fraction for the \eqn{s^{th}} vector
#' species and refresh the demographic matrix.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_mu = function(xds_obj, s=1){
  mu <- check_mu(xds_obj, s)
  xds_obj$MY_obj[[s]]$mu_obj$mu <- mu
  xds_obj$MY_obj[[s]]$mu <- mu
  xds_obj$MY_obj[[s]]$mu_t <- mu
  xds_obj <- change_Omega(xds_obj, s)
  class(xds_obj$MY_obj[[s]]$mu_obj) <- "static"
  return(xds_obj)
}

#' @title Set up emigration-loss fraction
#'
#' @description Configure and validate the emigration-loss fraction.
#'
#' @param name an emigration-loss fraction or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_mu = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_mu(name, xds_obj, options, s)
  xds_obj <- change_mu(xds_obj, s)
  return(xds_obj)
}

#' @title Set up F_mu
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_F_mu = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_mu", options)
}

#' @title Set up F_mu
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Fmu_name = name$name` 
#' + `options = name`
#' and call `setup_F_mu(Fmu_name, xds_obj, options, s)` 
#'
#' @inheritParams setup_F_mu
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_F_mu.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Fmu_name = name$name
  if(is.null(Fmu_name)) Fmu_name = "no_setup"
  xds_obj <- setup_F_mu(Fmu_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no F_mu 
#' @description Don't change anything 
#' @inheritParams setup_F_mu
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_mu.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up static emigration-loss fraction
#'
#' @description Set up supplied static emigration-loss fractions.
#'
#' @inheritParams setup_F_mu
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_mu.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$mu)) {
    mu <- options$mu
  } else if (is.numeric(name)) {
    mu <- name
  } else {
    stop("Supply emigration-loss fractions in `options$mu` or as numeric `name`.")
  }
  xds_obj$MY_obj[[s]]$mu_obj$mu <- mu
  class(xds_obj$MY_obj[[s]]$mu_obj) <- "static"
  return(xds_obj)
}

#' @title Set up random emigration-loss fraction
#' 
#' @description Implements the "random" case for [setup_F_mu]. See [make_F_mu_random].
#' 
#' @inheritParams setup_F_mu
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_mu.random = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$mu_obj <- make_F_mu_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Compute random emigration-loss fractions
#'
#' @description Generate patch-level emigration-loss fractions using the
#' configured random number generator.
#'
#' @inheritParams F_mu
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_mu.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$mu_obj, {
    mu <- f_rand(xds_obj$nPatches, p1, p2)
    return(mu)
  })
}

#' @title Make random emigration-loss fractions
#' 
#' @description Build a configuration object for random emigration-loss fractions.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a configuration object for [F_mu.random]
#' @export
make_F_mu_random = function(nPatches, options=list(), f_rand = stats::rbeta,
                            p1=8, p2=1000) {
  with(options, {
    mu_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    class(mu_obj) <- "random"
    return(mu_obj)
  })
}
