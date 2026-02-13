# generalized spatial differential equations

#' @title dts_update_ States for Discrete-Time Systems
#' @description dts_update_s the state variables
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return a [list] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update <- function(t, y, xds_obj) {
  UseMethod("dts_update", xds_obj$frame)
}


#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update_Lt = function(t, y, xds_obj, s){
  tt = with(xds_obj$runtime,runt(t,Dday,Lday))
  if(tt) return(Update_Lt(t, y, xds_obj, s))
  else return(get_L_vars(y, xds_obj,s))
}

#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update_MYt = function(t, y, xds_obj, s){
  tt = with(xds_obj$runtime,runt(t,Dday,MYday))
  if(tt) return(Update_MYt(t, y, xds_obj, s))
  else return(get_MY_vars(y, xds_obj, s))
}

#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update_XHt = function(t, y, xds_obj, i){
  tt = with(xds_obj$runtime,runt(t,Dday,XHday))
  if(tt) return(Update_XHt(t, y, xds_obj, i))
  else return(get_XH_vars(y, xds_obj, i))
}

#' @title Generalized spatial differential equation model
#' @description dts_update_ the state variables
#' @inheritParams dts_update
#' @return a [list] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update.full <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # dts_update_ Variables
  Lt <- dts_update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectors > 1)
    for(s in 2:xds_obj$nVectors)
      Lt <- c(Lt, dts_update_Lt(t, y, xds_obj, s))

  MYt <- dts_update_MYt(t, y, xds_obj, 1)
  if(xds_obj$nVectors > 1)
    for(s in 2:xds_obj$nVectors)
      MYt <- c(MYt, dts_update_MYt(t, y, xds_obj, s))

  XHt <- dts_update_XHt(t, y, xds_obj, 1)
  if(xds_obj$nHosts > 1)
    for(i in 2:xds_obj$nHosts)
      XHt <- c(XHt, dts_update_XHt(t, y, xds_obj, i))

  return(unlist(c(Lt, MYt, XHt)))
}


#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update.human <- function(t, y, xds_obj) {
  
  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # state derivatives
  XHt <- dts_update_XHt(t, y, xds_obj, 1)
  if(xds_obj$nHosts > 1)
    for(i in 2:xds_obj$nHosts)
      XHt <- c(XHt, dts_update_XHt(t, y, xds_obj, i))

  return(c(XHt))
}

#' @title Generalized spatial differential equation model (mosquito only)
#' @description Compute and update the state variables for
#' a model with mosquito ecology (no transmission)
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update.mosy <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  # compute derivatives
  Lt <- dts_update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectors > 1)
    for(s in 2:xds_obj$nVectors)
      Lt <- c(Lt, dts_update_Lt(t, y, xds_obj, s))

  Mt <- dts_update_MYt(t, y, xds_obj, 1)
  if(xds_obj$nVectors > 1)
    for(s in 2:xds_obj$nVectors)
      Mt <- c(Mt, dts_update_MYt(t, y, xds_obj, s))

  return(c(Lt, Mt))
}


#' @title Difference equation models for aquatic mosquito populations
#' @description Compute and update the state variables for
#' a model with only aquatic mosquitoes
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update.aquatic <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  # compute derivatives
  Lt <- dts_update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectors > 1)
    for(s in 2:xds_obj$nVectors)
      Lt <- c(Lt, dts_update_Lt(t, y, xds_obj, s))

  return(c(Lt))
}

#' @title Difference equation models for aquatic mosquito populations
#' @description Compute and update the state variables for
#' a model with only aquatic mosquitoes
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @noRd
#' @export
dts_update.eir <- function(t, y, xds_obj) {
  
  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  # state derivatives
  XHt <- dts_update_XHt(t, y, xds_obj, 1)
  
  return(list(c(XHt)))
}

