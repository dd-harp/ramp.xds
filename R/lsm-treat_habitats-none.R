
#' @title Set no treat_habitats
#' @description The null model for treat_habitats
#' @inheritParams TreatHabitats
#' @return [list]
#' @export
TreatHabitats.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no treat_habitats"
#' @inheritParams setup_treat_habitats
#' @return an **`xds`** object
#' @export
setup_treat_habitats.none <- function(name, pars, opts=list()) {
  treat_habitats <- 'none'
  class(treat_habitats) <- 'none'
  pars$lsm$treat_habitats <- treat_habitats
  return(pars)
}
