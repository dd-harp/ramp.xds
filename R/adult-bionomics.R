
#' @title Compute the blood feeding rate, f
#' @description This method dispatches on the type of `MYZpar$f_par`. It should
#' set the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f <- function(t, MYZpar) {
  UseMethod("F_f", MYZpar$f_par)
}

#' @title Compute the human blood fraction
#' @description This method dispatches on the type of `MYZpar$q_par`. It should
#' set the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q <- function(t, MYZpar) {
  UseMethod("F_q", MYZpar$q_par)
}

#' @title Compute mosguito survival
#' @description This method dispatches on the type of `MYZpar$g_par`. It should
#' set the values of g to (possibly changing) baseline values.
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_g <- function(t, MYZpar) {
  UseMethod("F_g", MYZpar$g_par)
}

#' @title Compute mosguito survival
#' @description This method dispatches on the type of `MYZpar$p_par`. It should
#' set the values of g to (possibly changing) baseline values.
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_p <- function(t, MYZpar) {
  UseMethod("F_p", MYZpar$p_par)
}


#' @title Compute mosquito emigration rates
#' @description This method dispatches on the type of `MYZpar$sigma_par`. It should
#' set the values of sigma to (possibly changing) baseline value(s).
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma <- function(t, MYZpar) {
  UseMethod("F_sigma", MYZpar$sigma_par)
}

#' @title Compute the egg laying rate
#' @description This method dispatches on the type of `MYZpar$nu_par`. It should
#' set the values of nu to (possibly changing) baseline value(s).
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu <- function(t, MYZpar) {
  UseMethod("F_nu", MYZpar$nu_par)
}

