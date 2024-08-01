
#' @title Static model for the blood feeding rate
#' @description Implements [F_f] for a static model
#' @inheritParams F_f
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f.static <- function(t, vars, f_par){
  f_par$f
}

#' @title Static model for human blood fraction
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q.static <- function(t, vars, q_par){
  q_par$q
}

#' @title Static model for mosquito survival
#' @description Implements [F_mu] for a static model
#' @inheritParams F_mu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_mu.static <- function(t, vars, mu_par){
  mu_par$mu
}

#' @title Static model for mosquito survival
#' @description Implements [F_g] for a static model
#' @inheritParams F_g
#' @return a [numeric] vector of length `nPatches`
#' @export
F_g.static <- function(t, vars, g_par){
  g_par$g
}

#' @title Static model for mosquito emigration
#' @description Implements [F_sigma] for a static model
#' @inheritParams F_sigma
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma.static <- function(t, vars, sigma_par){
  sigma_par$sigma
}


#' @title Static model for the egg laying rate
#' @description Implements [F_nu] for a static model
#' @inheritParams F_nu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu.static <- function(t, vars, nu_par){
  nu_par$nu
}

#' @title Static model for mosquito emigration
#' @description Implements [F_calK] for a static model
#' @inheritParams F_calK
#' @return a [numeric] vector of length `nPatches`
#' @export
F_calK.static <- function(t, vars, calK_par){
  calK_par$calK
}
