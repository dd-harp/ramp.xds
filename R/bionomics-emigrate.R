
#' @title Set up a patch emigration bionomic object
#'
#' @description Set up an object
#' to compute the patch emigration rate, \eqn{\sigma}
#'
#' @param sigma the mosquito patch emigration rate
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_sigma_obj = function(sigma, MY_obj){
  MY_obj$sigma = sigma
  MY_obj$sigma_t = sigma
  MY_obj$es_sigma = 1
  MY_obj$sigma_obj <- list()
  class(MY_obj$sigma_obj) <- "static"
  MY_obj$sigma_obj$sigma <- sigma
  return(MY_obj)
}

#' @title Compute the mosquito patch emigration rate
#'
#' @description This method dispatches on the type of `sigma_obj` and computes
#' the patch emigration rate, \eqn{\sigma}.
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_sigma = function(t, xds_obj, s){
  UseMethod("F_sigma", xds_obj$MY_obj[[s]]$sigma_obj)
}

#' @title Static model patch emigration rate
#'
#' @description Implements [F_sigma] for a static model
#'
#' @inheritParams F_sigma
#'
#' @return \eqn{sigma}, the patch emigration rate
#' @keywords internal
#' @export
F_sigma.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$sigma_obj$sigma)
}


#' @title Check patch emigration rate
#' 
#' @description
#' Check that the patch emigration rates are numeric, between zero and one,
#' and have length `nPatches`.
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the validated patch emigration rates
#' @seealso [xds_info_mosquito_demography]
#' @export
check_sigma = function(xds_obj, s=1){
  sigma = F_sigma(0, xds_obj, s)
  stopifnot(is.numeric(sigma))
  stopifnot(sigma >= 0)
  stopifnot(sigma <= 1)
  stopifnot(length(sigma) == xds_obj$nPatches)
  return(sigma)
}

#' @title Change the patch emigration rate
#' 
#' @description Update and validate the patch emigration rate for the
#' \eqn{s^{th}} vector species, then update the demographic matrix.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_sigma = function(xds_obj, s=1){
  sigma <- check_sigma(xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_obj$sigma <- sigma
  xds_obj$MY_obj[[s]]$sigma <- sigma
  xds_obj$MY_obj[[s]]$sigma_t <- sigma
  xds_obj <- change_Omega(xds_obj, s)
  class(xds_obj$MY_obj[[s]]$sigma_obj) <- "static"
  return(xds_obj)
}

#' @title Set up F_sigma
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_F_sigma = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_sigma", options)
}

#' @title Set up F_sigma
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Fsigma_name = name$name` 
#' + `options = name`
#' and call `setup_F_sigma(Fsigma_name, xds_obj, options, s)` 
#'
#' @inheritParams setup_F_sigma
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_F_sigma.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Fsigma_name = name$name
  if(is.null(Fsigma_name)) Fsigma_name = "no_setup"
  xds_obj <- setup_F_sigma(Fsigma_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no F_sigma 
#' @description Don't change anything 
#' @inheritParams setup_F_sigma
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_sigma.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up sigma
#'
#' @description Change the static values. 
#'
#' @inheritParams setup_F_sigma
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_sigma.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    sigma = options$sigma
  if(is.matrix(name))
    sigma <- name

  xds_obj$MY_obj[[s]]$sigma_obj$sigma = sigma
  xds_obj <- change_sigma(xds_obj, s)
  return(xds_obj)
}

#' @title Set up F_sigma
#' 
#' @description Implements the "random" case for [setup_F_sigma]. See [make_F_sigma_random]
#' 
#' @inheritParams setup_F_sigma
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_sigma.random = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$sigma_obj <- make_F_sigma_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Random patch emigration rate
#'
#' @description Generate patch emigration rates using the configured random
#' number generator.
#'
#' @inheritParams F_sigma
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_sigma.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$sigma_obj, {
    sigma <- f_rand(xds_obj$nPatches, p1, p2)
    return(sigma)
  })
}

#' @title Make random patch emigration rates
#' 
#' @description Set up a random vector 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a configuration object for [F_sigma.random]
#' @export
make_F_sigma_random = function(nPatches, options=list(), f_rand = stats::rbeta, p1=100, p2=1000) {
  with(options, {
    sigma_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    class(sigma_obj) <- "random"
    return(sigma_obj)
  })
}


#' @title Patch emigration rate
#'
#' @description Implements a type 2 functional response to the availability of three resources: 
#' blood hosts (\eqn{B}), aquatic habitats (\eqn{Q}), and sugar (\eqn{S}). 
#' 
#' \deqn{\sigma = \sigma_X \left( \frac{\sigma_Q Q}{1+\sigma_Q Q} 
#' + \frac{\sigma_B B}{1+\sigma_B B}
#' + \frac{\sigma_S S}{1+\sigma_S S}
#' \right)}
#'
#' @inheritParams F_sigma
#'
#' @return \eqn{\sigma}, the patch emigration rate
#' @keywords internal
#' @export
F_sigma.type2 = function(t, xds_obj, s){
  S = xds_obj$patches$sugar[[s]]
  B = xds_obj$terms$B[[s]]
  Q = xds_obj$terms$Qall[[s]]
  
  with(xds_obj$MY_obj[[s]]$sigma_obj, {
    sigma = sigX*(sigQ*Q/(1+sigQ*Q)+
                  sigB*B/(1+sigB*B)+ 
                  sigS*S/(1+sigB*S))
    return(sigma)
})}

#' @title Set up F_sigma
#' 
#' @description Set up a type 2 functional response
#' to model the patch emigration rate
#' 
#' @inheritParams setup_F_sigma
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_sigma.type2 = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$sigma_obj <- make_F_sigma_type2(xds_obj$nPatches, options)
  with(xds_obj$patches, stopifnot(exists("sugar"))) 
  return(xds_obj)
}

#' @title Make a patch emigration configuration
#' 
#' @description Make an object to model patch emigration as a 
#' type 2 functional response 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' 
#' @param sigX the emigration rate from a patch with no resources
#' @param sigB the shape parameter for blood hosts
#' @param sigQ the shape parameter for aquatic habitats
#' @param sigS the shape parameter for sugar
#'  
#' @return a configuration object for [F_sigma.type2]
#' 
#' @seealso [F_sigma.type2]
#' 
#' @export
make_F_sigma_type2 = function(nPatches, options=list(), sigX=.3, sigB=.1,
                              sigQ=.1, sigS=.1) {
  with(options, {
    sigma_obj <- list(sigX=sigX, sigB=sigB, sigQ=sigQ, sigS=sigS)
    class(sigma_obj) <- "type2"
    return(sigma_obj)
  })
}

#' @title Set up patch emigration rate
#'
#' @description Configure and validate the patch emigration rate.
#'
#' @param name a patch emigration rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_sigma = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_sigma(name, xds_obj, options, s)
  xds_obj <- change_sigma(xds_obj, s)
  return(xds_obj)
}