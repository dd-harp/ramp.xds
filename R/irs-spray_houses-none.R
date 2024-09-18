
#' @title Set no spray_houses
#' @description The null model for spray_houses
#' @inheritParams SprayHouses
#' @return [list]
#' @export
SprayHouses.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no spray_houses"
#' @inheritParams setup_spray_houses
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_spray_houses.none <- function(name, pars, opts=list()) {
  spray_houses <- 'none'
  class(spray_houses) <- 'none'
  pars$irs$spray_houses <- spray_houses
  return(pars)
}
