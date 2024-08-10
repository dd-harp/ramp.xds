#' @title Get the initial values as a vector
#' @param pars an **`xds`** object
#' @param i the human species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_H = function(pars, i=1){
  y=get_inits(pars)
  F_H(as.vector(unlist(y)), pars, i)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f = function(pars, s=1){
  UseMethod("get_f", pars$MYZpar[[s]])
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.trace = function(pars, s=1){
  numeric(0)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], f*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.si = function(pars, s=1){
  with(pars$MYZpar[[s]], f*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.sei = function(pars, s=1){
  with(pars$MYZpar[[s]], f*es_f)
}


#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.basicM = function(pars, s=1){
  with(pars$MYZpar[[s]], f*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q = function(pars, s=1){
  UseMethod("get_q", pars$MYZpar[[s]])
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.trace = function(pars, s=1){
  numeric(0)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], q*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.sei = function(pars, s=1){
  with(pars$MYZpar[[s]], q*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.si = function(pars, s=1){
  with(pars$MYZpar[[s]], q*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.basicM = function(pars, s=1){
  with(pars$MYZpar[[s]], q*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g = function(pars, s=1){
  UseMethod("get_g", pars$MYZpar[[s]])
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.trace = function(pars, s=1){
  numeric(0)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], g*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.sei = function(pars, s=1){
  with(pars$MYZpar[[s]], g*es_g)
}


#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.si = function(pars, s=1){
  with(pars$MYZpar[[s]], g*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.basicM = function(pars, s=1){
  with(pars$MYZpar[[s]], g*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma = function(pars, s=1){
  UseMethod("get_sigma", pars$MYZpar[[s]])
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.trace = function(pars, s=1){
  numeric(0)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma*es_sigma)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.si = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma*es_sigma)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.sei = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma*es_sigma)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.basicM = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma*es_sigma)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @seealso [get_bionomics]
#' @export
get_ft = function(pars, s=1){
  return(pars$outputs$bionomics[[s]]$f_t)
}

#' @title Get the human fraction
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_qt = function(pars, s=1){
  return(pars$outputs$bionomics[[s]]$q_t)
}

#' @title Get the last state
#' @param pars an `xds` object
#' @return a [numeric] vector
#' @export
get_last <- function(pars){
  pars$outputs$last_y
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm the time
#' @param de_vars the output of [deSolve]
#' @param pars an `xds` object
#' @return [list]
#' @export
get_bionomics <- function(tm, de_vars, pars) {
  MYZ <- list()
  MYZ[[1]] <- get_bionomics_s(tm, de_vars, pars, 1)
  s = length(pars$MYZpar)
  if(s >1)
    for(ix in 2:s)
      MYZ[[ix]] <- get_bionomics_s(tm, de_vars, pars, ix)
  return(MYZ)
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm the time
#' @param de_vars the output of [deSolve]
#' @param pars an `xds` object
#' @param s a vector species index
#' @return [list]
#' @export
get_bionomics_s <- function(tm, de_vars, pars, s) {

  pars <- reset_state_i(1, tm, de_vars, pars)
  f_t     <- get_f(pars, s)
  q_t     <- get_q(pars, s)
  g_t     <- get_g(pars, s)
  sigma_t <- get_sigma(pars, s)

  for(i in 2:length(tm)){
    pars <- reset_state_i(i, tm, de_vars, pars)
    f_t     <- rbind(f_t, get_f(pars, s))
    q_t     <- rbind(q_t, get_q(pars, s))
    g_t     <- rbind(g_t, get_g(pars, s))
    sigma_t <- rbind(sigma_t, get_sigma(pars, s))
  }
  return(list(f_t=f_t, q_t=q_t, g_t=g_t, sigma_t=sigma_t))
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm a vector of times
#' @param de_vars a matrix with the values of the variables
#' @param pars an **`xds`** object
#' @return [list]
#' @export
get_terms <- function(tm, de_vars, pars) {
  pars <- reset_state_i(1, tm, de_vars, pars)
  terms = list()
  terms$EIR = as.vector(pars$EIR[[1]])
  terms$kappa = as.vector(pars$kappa[[1]])
  for(i in 2:length(tm)){
    pars <- reset_state_i(i, tm, de_vars, pars)
    terms$EIR <- rbind(terms$EIR, as.vector(pars$EIR[[1]]))
    terms$kappa <- rbind(terms$kappa, as.vector(pars$kappa[[1]]))
  }
  return(terms)
}
