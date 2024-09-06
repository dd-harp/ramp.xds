# Functions to compute adult mosquito bionomic parameters
# Compatible with "SEI" and "GeRM"

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

#' @title Static model for the blood feeding rate
#' @description Implements [F_f] for a static model
#' @inheritParams F_f
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f.static <- function(t, vars, f_par){
  f_par$f
}

#' @title Type 2 functional response for the blood feeding rate
#' @description Implements [F_f] for a static model
#' @inheritParams F_f
#' @return a [numeric] vector of length `nPatches`
#' @export
F_f.type2 <- function(t, vars, f_par){
  with(vars, with(f_par,
                  return(fx*sf*B/(1+sf*B))
  ))
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

#' @title Static model for human blood fraction
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q.static <- function(t, vars, q_par){
  q_par$q
}

#' @title Static model for human blood fraction
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return a [numeric] vector of length `nPatches`
#' @export
F_q.dynamic <- function(t, vars, q_par){
  with(vars,{
    return((W+Visitors)/(W + Visitors + O))
  })
}


#' @title Compute the human blood fraction
#' @description This method dispatches on the type of `p_par`. It should
#' set the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param vars exogenous variables
#' @param p_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_p <- function(t, vars, p_par) {
  UseMethod("F_p", p_par)
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

#' @title Static model for mosquito survival
#' @description Implements [F_g] for a static model
#' @inheritParams F_g
#' @return a [numeric] vector of length `nPatches`
#' @export
F_g.static <- function(t, vars, g_par){
  g_par$g
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

#' @title Static model for mosquito emigration
#' @description Implements [F_sigma] for a static model
#' @inheritParams F_sigma
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma.static <- function(t, vars, sigma_par){
  sigma_par$sigma
}


#' @title Model for mosquito emigration based on resource availability
#' @description Implements [F_sigma] for a static model
#' @inheritParams F_sigma
#' @return a [numeric] vector of length `nPatches`
#' @export
F_sigma.BQS <- function(t, vars, sigma_par){
  with(vars, with(sigma_par,
                  return(sigma_x*(sigma_B/(1+sB*B) + sigma_Q/(1+sQ*Q) + sigma_S/(1+sS*S)))
  ))
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

#' @title Static model for mosquito survival
#' @description Implements [F_mu] for a static model
#' @inheritParams F_mu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_mu.static <- function(t, vars, mu_par){
  mu_par$mu
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

#' @title Static model for the egg laying rate
#' @description Implements [F_nu] for a static model
#' @inheritParams F_nu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu.static <- function(t, vars, nu_par){
  nu_par$nu
}

#' @title Type 2 functional response for the blood feeding rate
#' @description Implements [F_nu] for a static model
#' @inheritParams F_nu
#' @return a [numeric] vector of length `nPatches`
#' @export
F_nu.type2 <- function(t, vars, nu_par){
  with(nu_par,{
    return(nux*snu*Q/(1+snu*Q))
  })
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

#' @title Static model for mosquito emigration
#' @description Implements [F_calK] for a static model
#' @inheritParams F_calK
#' @return a [numeric] vector of length `nPatches`
#' @export
F_calK.static <- function(t, vars, calK_par){
  calK_par$calK
}




