
#' @title Set no spray_area
#' @description The null model for spray_area
#' @inheritParams SprayArea
#' @return [list]
#' @export
SprayArea.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no spray_area"
#' @inheritParams setup_spray_area
#' @return an **`xds`** object
#' @export
setup_spray_area.none <- function(name, pars, opts=list()) {
  spray_area <- 'none'
  class(spray_area) <- 'none'
  pars$area_spray$spray <- spray_area
  return(pars)
}
