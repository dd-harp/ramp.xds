# X 

#' @title Update states for discrete-time systems
#' @description Update the state variables.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
dts_update <- function(t, y, xds_obj) {
  UseMethod("dts_update", xds_obj$frame)
}



#' @title Update states for discrete-time systems
#' @description Update the state variables for a *full* model.
#' @inheritParams dts_update
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
dts_update.full <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

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


#' @title Update states for discrete-time systems
#' @description Update the state variables for a *human* model.
#' @inheritParams dts_update
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
dts_update.human <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  XHt <- Update_XHt(t, y, xds_obj, 1)
  if(xds_obj$nHostSpecies > 1)
    for(i in 2:xds_obj$nHostSpecies)
      XHt <- c(XHt, Update_XHt(t, y, xds_obj, i))

  return(c(XHt))
}

#' @title Update states for discrete-time systems
#' @description Update the state variables for a *mosy* model.
#' @inheritParams dts_update
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
dts_update.mosy <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

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


#' @title Update states for discrete-time systems
#' @description Update the state variables for an *aquatic* model.
#' @inheritParams dts_update
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
dts_update.aquatic <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  Lt <- Update_Lt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies)
      Lt <- c(Lt, Update_Lt(t, y, xds_obj, s))

  return(c(Lt))
}

#' @title Update states for discrete-time systems
#' @description Update the state variables for an *eir* model.
#' @inheritParams dts_update
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
dts_update.eir <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  XHt <- Update_XHt(t, y, xds_obj, 1)

  return(c(XHt))
}
