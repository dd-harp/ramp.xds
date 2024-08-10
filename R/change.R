
#' @title Change human population density
#' @param H human population density
#' @param pars a [list]
#' @param i the host species index
#' @return a [list]
#' @export
change_H = function(H, pars, i=1){
  stopifnot(length(H) == pars$nStrata[i])
  vars = as.list(get_Xinits(pars,i))
  make_Xinits(pars, H, i, vars)
  class(pars$BFpar) <- trigger_setup(pars$BFpar)
  class(pars$beta) <- trigger_setup(pars$beta)
  return(pars)
}
