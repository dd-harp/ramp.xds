
#' @title Make Outputs
#' @param pars an `xds` object
#' @param de_vars the output of [deSolve]
#' @param tm the time
#' @return an **`xds`** object
make_outputs = function(pars, de_vars, tm){
  UseMethod("make_outputs", pars$frame)
}

#' @title Make Outputs
#' @inheritParams make_outputs
#' @return an **`xds`** object
make_outputs.full = function(pars, de_vars, tm){
  pars$outputs$time <- tm
  pars$outputs$last_y <- tail(de_vars, 1)
  #  pars$outputs$bionomics <- get_bionomics(tm, de_vars, pars)
  pars$outputs$orbits <- parse_orbits(de_vars, pars)
  pars$outputs$terms <- get_terms(tm, de_vars, pars)
  return(pars)
}

#' @title Make Outputs
#' @inheritParams make_outputs
#' @return an **`xds`** object
make_outputs.mosy = function(pars, de_vars, tm){
  pars$outputs$time <- tm
  pars$outputs$last_y <- tail(de_vars, 1)
  pars$outputs$bionomics <- get_bionomics(tm, de_vars, pars)
  pars$outputs$orbits <- parse_orbits(de_vars, pars)
  return(pars)
}

#' @title Make Outputs
#' @inheritParams make_outputs
#' @return an **`xds`** object
make_outputs.aquatic = function(pars, de_vars, tm){
  pars$outputs$time <- tm
  pars$outputs$last_y <- tail(de_vars, 1)
  pars$outputs$orbits <- parse_orbits(de_vars, pars)
  return(pars)
}

#' @title Make Outputs
#' @inheritParams make_outputs
#' @return an **`xds`** object
make_outputs.human = function(pars, de_vars, tm){
  pars$outputs$time <- tm
  pars$outputs$last_y <- tail(de_vars, 1)
  pars$outputs$orbits <- parse_orbits(de_vars, pars)
  pars$outputs$terms <- get_terms(tm, de_vars, pars)
  return(pars)
}

#' @title Make Outputs
#' @inheritParams make_outputs
#' @return an **`xds`** object
make_outputs.eir = function(pars, de_vars, tm){
  pars$outputs$time <- tm
  pars$outputs$last_y <- tail(de_vars, 1)
  pars$outputs$orbits <- parse_orbits(de_vars, pars)
  pars$outputs$terms <- list()
  pars$outputs$terms$EIR <- list()
  pars$outputs$terms$EIR[[1]] <- with(pars, F_eir(tm))
  return(pars)
}