# generic methods for developments

#' @title Set up developments
#' @description This method dispatches on the type of `pars$DEVELOPMENT`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
Development <- function(t, pars) {
  UseMethod("Development", pars$DEVELOPMENT)
}

#' @title Set up developments
#' @description Implements [Development] for the no_dev model (do nothing)
#' @inheritParams Development
#' @return [list]
#' @export
Development.no_dev <- function(t, pars) {
  return(pars)
}

#' @title Set up the no_dev model for developments (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_development <- function(pars) {
  DEVELOPMENT <- list()
  class(DEVELOPMENT) <- 'no_dev'
  pars$DEVELOPMENT <- DEVELOPMENT
  return(pars)
}
