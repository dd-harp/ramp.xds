
#' @title Get mean forcing
#'
#' @description
#' Get the parameter(s) that set mean forcing
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return a vector, the mean parameter for forcing
#'
#' @export
get_mean_forcing = function(xds_obj){
  UseMethod("get_mean_forcing", xds_obj$forced_by)
}

#' @title Get mean forcing
#'
#' @description
#' Return null for models without forcing
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an empty vector 
#'
#' @keywords internal
#' @export
get_mean_forcing.none = function(xds_obj){
  return(c())
}

#' @title Get mean forcing
#'
#' @description
#' Get `Lambda`, the mean emergence
#' rate(s) \eqn{\Lambda}, for a model
#' forced by adult mosquito emergence
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return a vector, the value of `Lambda`
#'
#' @keywords internal
#' @export
get_mean_forcing.Lambda = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    return(xds_obj$L_obj[[1]]$Lambda)
  } else {
    Lambda = list()
    for(s in 1:length(xds_obj$nVectorSpecies))
      Lambda[[s]] <- xds_obj$L_obj[[s]]$Lambda
    return(Lambda)
  }
}

#' @title Get mean forcing
#'
#' @description
#' Get the mean daily EIR for an `eir` model
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return a vector, the value of `eir`
#'
#' @keywords internal
#' @export
get_mean_forcing.eir = function(xds_obj){
  return(xds_obj$EIR_obj$eir)
}

#' @title Get mean forcing
#'
#' @description
#' Get mean egg deposition rates
#' for a trivial **MY** model
#'
#' @param xds_obj an **`xds`** model object
#'
#' @keywords internal
#' @export
get_mean_forcing.eggs = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    return(xds_obj$MY_obj[[1]]$eggs)
  } else {
    eggs = list()
    for(s in 1:length(xds_obj$nVectorSpecies))
      eggs[[s]] <- xds_obj$MY_obj[[s]]$eggs
    return(eggs)
  }
}


#' @title Get mean forcing
#'
#' @description
#' Get the mean density of infectious mosquitoes,
#' for a trivial **MY**  model
#'
#' @param xds_obj an **`xds`** model object
#'
#' @keywords internal
#' @export
get_mean_forcing.fqZ = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    f = get_f(xds_obj)
    q = get_q(xds_obj)
    Z = xds_obj$MY_obj[[1]]$Z
    return(f*q*Z)
  } else {
    fqZ = rep(0, xds_obj$nVectorSpecies)
    for(s in 1:length(xds_obj$nVectorSpecies)){
      f = get_f(xds_obj, s)
      q = get_q(xds_obj, s)
      fqZ[s] <- f*q*xds_obj$MY_obj[[s]]$Z  
    }
    return(Z)
  }
}

#' @title Get mean forcing
#'
#' @description
#' Get net infectiousness
#'
#' @param xds_obj an **`xds`** model object
#'
#' @keywords internal
#' @export
get_mean_forcing.kappa = function(xds_obj){
  if(xds_obj$nHostSpecies == 1){
    return(xds_obj$XH_obj[[1]]$kappa)
  } else {
    kappa = list()
    for(i in 1:length(xds_obj$nHostSpecies))
      kappa[[i]] <- xds_obj$XH_obj[[i]]$kappa
    return(kappa)
  }
}