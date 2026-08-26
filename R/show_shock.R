
#' @title Plot the shock
#'
#' @description For a model with temporal forcing,
#' show the perturbation function
#'
#' @param xds_obj an **`xds`** model object
#' @param tm the time points
#' @param add add to existing plot
#' @param clr the line color
#' @param rng if not NULL, range limits for plotting
#'
#' @return the temporal shock, invisibly
#'
#' @export
show_shock = function(xds_obj, tm=10*c(0:365), add=FALSE, clr="black", rng=NULL){
  shock <- F_shock(tm, xds_obj)
  
  if(is.null("rng")) rng = range(shock)
  if(length(shock)>0){
    if(add==FALSE) plot(tm, shock, ylab="Shock", xlab = "Time",
                        col=clr, type="n", ylim=rng)
    lines(tm, shock, col = clr)
  }
  return(invisible(shock))
  
}

#' @title Compute the temporal shock
#'
#' @description
#' Evaluate the function `F_shock` for a forced model.
#'
#' @param tm the time points
#' @param xds_obj an **`xds`** model object
#'
#' @return the temporal shock, invisibly
#'
#' @keywords internal
#' @export
F_shock = function(tm, xds_obj){
  UseMethod("F_shock", xds_obj$forced_by)
}

#' @title Compute the temporal shock
#'
#' @description
#' Evaluate the function `F_shock` for a forced model when `forced_by` is "none"
#'
#' @inheritParams F_shock
#'
#' @return an empty vector
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_shock.none = function(tm, xds_obj){
  return(c())
}

#' @title Compute the temporal shock
#'
#' @description
#' Evaluate the function `F_shock` for a forced model when `forced_by` is "Lambda"
#'
#' @inheritParams F_shock
#'
#' @return the temporal shock
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_shock.Lambda = function(tm, xds_obj){
  return(with(xds_obj$L_obj[[1]], F_shock(tm)))
}

#' @title Compute the temporal shock
#'
#' @description
#' Evaluate the function `F_shock` for a forced model when `forced_by` is "eir"
#'
#' @inheritParams F_shock
#'
#' @return the temporal shock, invisibly
#'
#' @keywords internal
#' @export
F_shock.eir= function(tm, xds_obj){
  return(with(xds_obj$EIR_obj,F_shock(tm)))
}

#' @title Compute the temporal shock
#'
#' @description
#' Evaluate the function `F_shock` for a trivial **MY** model
#'
#' @inheritParams F_shock
#'
#' @return the temporal shock
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_shock.MY = function(tm, xds_obj){
  return(with(xds_obj$MY_obj[[1]], F_shock(tm)))
}

#' @title Compute the temporal shock
#'
#' @description
#' Evaluate the function `F_shock` for a trivial **XH** model
#'
#' @inheritParams F_shock
#'
#' @return the temporal shock
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_shock.kappa = function(tm, xds_obj){
  return(with(xds_obj$XH_obj[[1]], F_shock(tm)))
}
