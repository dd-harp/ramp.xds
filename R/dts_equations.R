# generalized spatial differential equations

#' @title dts_update_ States for Discrete-Time Systems
#' @description dts_update_s the state variables
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return a [list] containing the vector of all state derivatives
#' @keywords internal
#' @export
dts_update <- function(t, y, xds_obj) {
  UseMethod("dts_update", xds_obj$frame)
}



#' @title Generalized spatial differential equation model
#' @description dts_update_ the state variables
#' @inheritParams dts_update
#' @return a [list] containing the vector of all state derivatives
#' @keywords internal
#' @export
dts_update.full <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # dts_update_ Variables
  Lt <- Update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies)
      Lt <- c(Lt, Update_Lt(t, y, xds_obj, s))

  MYt <- Update_MYt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies)
      MYt <- c(MYt, Update_MYt(t, y, xds_obj, s))

  XHt <- Update_XHt(t, y, xds_obj, 1)
  if(xds_obj$nHostSpecies > 1)
    for(i in 2:xds_obj$nHostSpecies)
      XHt <- c(XHt, Update_XHt(t, y, xds_obj, i))

  return(unlist(c(Lt, MYt, XHt)))
}


#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @keywords internal
#' @export
dts_update.human <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # state derivatives
  XHt <- Update_XHt(t, y, xds_obj, 1)
  if(xds_obj$nHostSpecies > 1)
    for(i in 2:xds_obj$nHostSpecies)
      XHt <- c(XHt, Update_XHt(t, y, xds_obj, i))

  return(c(XHt))
}

#' @title Generalized spatial differential equation model (mosquito only)
#' @description Compute and update the state variables for
#' a model with mosquito ecology (no transmission)
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @keywords internal
#' @export
dts_update.mosy <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # compute derivatives
  Lt <- Update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies)
      Lt <- c(Lt, Update_Lt(t, y, xds_obj, s))

  Mt <- Update_MYt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies)
      Mt <- c(Mt, Update_MYt(t, y, xds_obj, s))

  return(c(Lt, Mt))
}


#' @title Difference equation models for aquatic mosquito populations
#' @description Compute and update the state variables for
#' a model with only aquatic mosquitoes
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @keywords internal
#' @export
dts_update.aquatic <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # compute derivatives
  Lt <- Update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies)
      Lt <- c(Lt, Update_Lt(t, y, xds_obj, s))

  return(c(Lt))
}

#' @title Difference equation models for aquatic mosquito populations
#' @description Compute and update the state variables for
#' a model with only aquatic mosquitoes
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @keywords internal
#' @export
dts_update.eir <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # state derivatives
  XHt <- Update_XHt(t, y, xds_obj, 1)

  return(list(c(XHt)))
}
