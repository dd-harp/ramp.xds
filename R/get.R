
#' @title Get the last state
#' @param pars an `xds` object
#' @return a [numeric] vector
#' @export
get_last <- function(pars){
  pars$outputs$last_y
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param i the host species index
#' @export
get_EIR = function(pars, i){
  pars$outputs$terms$EIR
}

