#' @title Compute emerging adults
#' @description
#' Sums up adults emerging from all aquatic habitats using the habitat membership matrix
#' @param t the time
#' @param y the state variables
#' @param pars a `xds` object
#' @return a [list] -- the modified `xds` object
#' @export
Emergence = function(t, y, pars){
  for(s in 1:pars$nVectors)
    pars$Lambda[[s]] = pars$habitat_matrix %*% F_emerge(t, y, pars, s)
  return(pars)
}
