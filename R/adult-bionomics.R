
#' @title Set bloodfeeding and mortality rates to baseline
#' @description This method dispatches on the type of `pars$MYZpar`. It should
#' set the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [list]
#' @export
MBionomics <- function(t, y, pars, s) {
  UseMethod("MBionomics", pars$MYZpar[[s]]$baseline)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.setup <- function(t, y, pars, s) {
  pars$MYZpar[[s]]$now <- pars$MYZpar[[s]]$baseline
  pars$MYZpar[[s]]$now <- pars$MYZpar[[s]]$baseline
  return(pars)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.static <- function(t, y, pars, s) {
  pars$MYZpar[[s]]$now <- pars$MYZpar[[s]]$baseline
  return(pars)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.dynamic <- function(t, y, pars, s) {
  for(s in 1:pars$nVectors){
     pars$MYZpar[[s]]$baseline <- dynamic_MYZpar(t, pars$vars, pars$MYZpar[[s]])
     pars <- make_MYZprobs(pars, s)
     pars <- make_Omega(pars, s)
     pars$MYZpar[[s]]$now <- pars$MYZpar[[s]]$baseline
  }
  return(pars)
}

#' @title Call a function to set baseline bionomic parameters
#' @description This method dispatches on the type of `baseline`.
#' @param t current simulation time
#' @param vars current simulation time
#' @param MYZpar the mosquito model
#' @return baseline bionomics as a named [list]
#' @export
dynamic_MYZpar <- function(t, vars, MYZpar) {
  UseMethod("dynamic_MYZpar", MYZpar$baseline)
}

#' @title Update adult bionomic parameters
#' @description This method dispatches on the type of `baseline`.
#' @inheritParams dynamic_MYZpar
#' @return baseline bionomics as a named [list]
#' @export
dynamic_MYZpar.RMfunc <- function(t, vars, MYZpar){with(MYZpar,{
  baseline$f      <- func$F_f(t, vars, f_par)
  baseline$q      <- func$F_q(t, vars, q_par)
  baseline$g      <- func$F_g(t, vars, g_par)
  baseline$sigma  <- func$F_sigma(t, vars, sigma_par)
  baseline$mu     <- func$F_mu(t, vars, mu_par)
  baseline$nu     <- func$F_nu(t, vars, nu_par)
  baseline$eip    <- func$F_eip(t, vars, eip_par)
  baseline$calK   <- func$F_calK(t, vars, calK_par)
  return(baseline)
})}

#' @title Adult bionomics: Compute probabilities from rates
#' @description Where homologous `xde` and `dts` models exist,
#' the rates can be translated into probabilities with runtime
#' support.
#' @param pars an `xds` object
#' @param s the species index
#' @return a [list]
#' @seealso [dynamic_MYZpar]
#' @export
make_MYZprobs <- function(pars, s) {
  UseMethod("MBionomics", pars$xds)
}

#' @title Adult bionomics: Compute probabilities from rates
#' @description The `xde` case returns the unmodified `xds` object.
#' @inheritParams make_MYZprobs
#' @return unmodified `xds` object
#' @export
make_MYZprobs.xde <- function(pars, s) {
  return(pars)
}

#' @title Compute probabilities from rates for `dts` models with runtime support
#' @description The `dts` case calls `MYZ_rates2probs` to compute
#' @inheritParams make_MYZprobs
#' @return a [list]
#' @export
make_MYZprobs.dts <- function(pars, s) {
  pars$MYZpar[[s]]$baseline <- MYZ_rates2probs(pars$MYZpar[[s]], pars$runtime)
  return(pars)
}



