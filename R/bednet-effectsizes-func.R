
#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNetEffectSizes
#' @return [list]
#' @export
BedNetEffectSizes.func <- function(t, pars, s) {
  ## set es_f
  ## set es_q
  ## set es_g
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_effectsizes
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes.func = function(name, pars, opts=list()){
  class(name) <- name
  pars <- setup_bednet_effectsizes_func(pars, opts)
}

#' @title Set no bednet
#' @description The null model for bednet
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes_func <- function(pars, opts) {
  pars = dynamic_vector_control(pars)
  coverage <- list()
  class(coverage) <- 'func'
  ## setup function to compute es_f
  ## setup function to compute es_q
  ## setup function to compute es_g
  pars$bednets$coverage <- coverage
  return(pars)
}
