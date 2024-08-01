
#' @title Compute the blood feeding rate, f
#' @description This method dispatches on the type of `q_par$f_par`. It should
#' set the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param vars exogenous variables
#' @param f_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f <- function(t, vars, f_par) {
  UseMethod("F_f", f_par)
}

#' @title Compute the human blood fraction
#' @description This method dispatches on the type of `q_par$q_par`. It should
#' set the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param vars exogenous variables
#' @param q_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q <- function(t, vars, q_par) {
  UseMethod("F_q", q_par)
}

#' @title Compute mosguito survival
#' @description This method dispatches on the type of `q_par$g_par`. It should
#' set the values of g to (possibly changing) baseline values.
#' @param t current simulation time
#' @param vars exogenous variables
#' @param g_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_g <- function(t, vars, g_par) {
  UseMethod("F_g", g_par)
}

#' @title Compute mosquito emigration rates
#' @description This method dispatches on the type of `q_par$sigma_par`. It should
#' set the values of sigma to (possibly changing) baseline value(s).
#' @param t current simulation time
#' @param vars exogenous variables
#' @param sigma_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma <- function(t, vars, sigma_par) {
  UseMethod("F_sigma", sigma_par)
}

#' @title Compute the emigration loss
#' @description Compute the fraction lost to emigration
#' @param t current simulation time
#' @param vars exogenous variables
#' @param mu_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_mu <- function(t, vars, mu_par) {
  UseMethod("F_mu", mu_par)
}

#' @title Compute the egg laying rate
#' @description This method dispatches on the type of `q_par`. It should
#' set the values of nu to (possibly changing) baseline value(s).
#' @param t current simulation time
#' @param vars exogenous variables
#' @param nu_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu <- function(t, vars, nu_par) {
  UseMethod("F_nu", nu_par)
}

#' @title Compute the egg laying rate
#' @description This method ...
#' @param t current simulation time
#' @param vars exogenous variables
#' @param calK_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_calK <- function(t, vars, calK_par) {
  UseMethod("F_calK", calK_par)
}
