# specialized methods for the adult mosquito macdonald model

#' @title The **macdonald** Module Skill Set 
#' 
#' @description The **MY** skill set is a list of 
#' an module's capabilities: 
#' 
#' + `demography` is 
#'
#' @inheritParams skill_set_MY 
#' 
#' @return *MY* module skill set, as a list 
#' 
#' @export
skill_set_MY.macdonald = function(MYname){
  return(list())
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_MY.macdonald = function(xds_obj, s){
  Omega <- with(xds_obj$MY_obj[[s]], make_Omega_xde(g, sigma, mu, K_matrix))
  xds_obj$MY_obj[[s]]$Omega <- Omega
  xds_obj$MY_obj[[s]]$Upsilon <- expm::expm(-Omega*xds_obj$MY_obj[[s]]$eip) 
  return(xds_obj)
}

#' @title Compute derivatives for the **MY** module `macdonald`
#' @description 
#' This implements a delay differential equation model for adult mosquito ecology and
#' infection dynamics that is consistent with the model published by George
#' Macdonald in 1952. A generalized version of this model, the **MY** module `GeRM`, 
#' was developed to handle exogenous forcing by weather and vector control. This model
#' should be used only for educational purposes. 
#'
#' **Variables:**
#'
#' - \eqn{M}: the density of adult mosquitoes 
#' - \eqn{Y}: the density of infected adult mosquitoes 
#' - \eqn{Z}: the density of infectious adult mosquitoes 
#'
#' **Parameters and Terms:**
#'
#' - \eqn{\Lambda} or `Lambda`: the emergence rate of adult mosquitoes (from `F_emerge`) 
#' - \eqn{f} or `f`: the blood feeding rate 
#' - \eqn{q} or `q`: maturation rate
#' - \eqn{\tau} or `eip`: the extrinsic incubation period 
#' - \eqn{\Omega} or `Omega`: an adult mosquito demographic matrix, including mortality and migration 
#' - \eqn{\Upsilon} or `Upsilon`: survival and dispersal through the eip, \eqn{\Upsilon= e^{-\Omega \tau}}
#'
#'
#' **Dynamics:**
#' In the delay equation, we use the subscript to denote the lagged value of a 
#' variable or term: *e.g.*, \eqn{M_\tau = M(t-\tau)}. 
#' 
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda - \Omega \cdot M \\
#' dY/dt &= fq\kappa(M-Y) - \Omega \cdot Y \\
#' dZ/dt &= \Upsilon \cdot (fq\kappa)_\tau(M_\tau-Y_\tau) - \Omega \cdot Y \\
#' \end{array}}
#'
#' This model was included mainly for the historical interest. It has been updated to handle
#' exogenous forcing by weather and vector control in the module `GeRM` 
#' 
#' @note This model is not capable of being extended to 
#' handle exogenous forcing by weather or vector control. Please 
#' use the `GeRM` model. 
#' @inheritParams dMYdt
#' @return a [numeric] vector
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
#' @export
dMYdt.macdonald <- function(t, y, xds_obj, s){

  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      if (t <= eip) {
        M_eip <- inits$M
        Y_eip <- inits$Y
        kappa_eip <- kappa
      } else {
        M_eip <- lagvalue(t=t-eip, nr = ix$M_ix)
        Y_eip <- lagvalue(t=t-eip, nr = ix$Y_ix)
        kappa_eip <- lagderiv(t=t-eip, nr = ix$kappa_ix)
      }

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      dYdt <- f*q*kappa*(M - Y) - (Omega %*% Y)
      dZdt <- Upsilon %*% (f*q*kappa_eip * (M_eip - Y_eip)) - (Omega %*% Z)
      return(c(dMdt, dPdt, dYdt, dZdt, kappa))
    })
  })
}


#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the macdonald model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.macdonald <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the macdonald model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.macdonald <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$MY_obj[[s]]$ix$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the macdonald model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.macdonald <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Setup MY_obj for the macdonald model
#' @description Implements [setup_MY_obj] for the macdonald model
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @export
setup_MY_obj.macdonald = function(MYname, xds_obj, s, options=list()){
  xds_obj = ode_to_dde(xds_obj)
  MY_obj <- make_MY_obj_macdonald(xds_obj$nPatches, options)
  class(MY_obj) <- 'macdonald'
  xds_obj$MY_obj[[s]] = MY_obj
  return(xds_obj)
}

#' @title Make parameters for macdonald ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param options a [list] of values that overwrites the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @importFrom expm expm
#' @export
make_MY_obj_macdonald = function(nPatches, options=list(), eip=12,
                                   g=1/12, sigma=1/8, mu=0, f=0.3, q=0.95,
                                   nu=1, eggsPerBatch=60){

  with(options,{
    MY_obj <- list()
    MY_obj$nPatches <- nPatches

    MY_obj$eip    <- checkIt(eip, 1)
    MY_obj$f      <- checkIt(f, nPatches)
    MY_obj$q      <- checkIt(q, nPatches)
    MY_obj$g      <- checkIt(g, nPatches)
    MY_obj$sigma  <- checkIt(sigma, nPatches)
    MY_obj$mu     <- checkIt(mu, nPatches)
    MY_obj$nu     <- checkIt(nu, nPatches)

    MY_obj$eggsPerBatch <- eggsPerBatch
    MY_obj$K_matrix <- diag(nPatches)

    Omega <- diag(g, nPatches)
    MY_obj$Omega <- Omega
    MY_obj$Upsilon <- expm::expm(-Omega*eip)

    MY_obj$baseline <- 'macdonald'
    class(MY_obj$baseline) <- 'macdonald'

    return(MY_obj)
  })}

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the macdonald model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @importFrom utils tail
#' @export
setup_MY_ix.macdonald <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  kappa_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(kappa_ix, 1)


  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix, Y_ix=Y_ix, Z_ix=Z_ix,
                          kappa_ix=kappa_ix)
  return(xds_obj)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @export
get_MY_vars.macdonald <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         Y = y[Y_ix],
         Z = y[Z_ix]
)))}

#' @title parse the output of deSolve and return variables for the macdonald model
#' @description Implements [parse_MY_orbits] for the macdonald model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @export
parse_MY_orbits.macdonald <- function(outputs, xds_obj, s) {with(xds_obj$MY_obj[[s]]$ix,{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  Y = outputs[,Y_ix]
  Z = outputs[,Z_ix]
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parous=parous))
})}



#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MY_pars.macdonald <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f, q=q, g=g, sigma=sigma, eip=eip, mu=mu,
    nu=nu, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @export
change_MY_pars.macdonald <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f = f
    xds_obj$MY_obj[[s]]$q = q
    xds_obj$MY_obj[[s]]$g = g
    xds_obj$MY_obj[[s]]$sigma = sigma
    xds_obj$MY_obj[[s]]$eip = eip
    xds_obj$MY_obj[[s]]$mu = mu
    xds_obj$MY_obj[[s]]$nu = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    return(xds_obj)
  }))}


#' @title Setup initial values for the macdonald model
#' @description Implements [setup_MY_inits] for the macdonald model
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @export
setup_MY_inits.macdonald = function(xds_obj, s, options=list()){with(xds_obj$MY_obj[[s]], {
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_macdonald(nPatches, options)
  return(xds_obj)
})}


#' @title Make inits for macdonald adult mosquito model
#' @param nPatches the number of patches in the model
#' @param options a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
make_MY_inits_macdonald = function(nPatches, options = list(),
                                   M=5, P=1, Y=1, Z=1){
  with(options,{
    M = unname(checkIt(M, nPatches))
    P = unname(checkIt(P, nPatches))
    Y = unname(checkIt(Y, nPatches))
    Z = unname(checkIt(Z, nPatches))
    dk_ix = rep(0, nPatches)
    return(list(M=M, P=P, Y=Y, Z=Z, dk_ix=dk_ix))
})}

#' @title change initial values for the macdonald model
#' @description Implements [change_MY_inits] for the macdonald model
#' 
#' @inheritParams change_MY_inits
#' 
#' @return a [list]
#' @export
change_MY_inits.macdonald = function(xds_obj, s, options=list()){
  inits = with(get_MY_inits(xds_obj, s), 
            with(options,{ 
              list(M=M, P=P, Y=Y, Z=Z, dk_ix=dk_ix)
           }))
  xds_obj$MY_obj[[s]]$inits=inits 
  return(xds_obj)
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritParams steady_state_MY
#' @return none
#' @importFrom MASS ginv
#' @importFrom expm expm
#' @export
steady_state_MY.macdonald = function(Lambda, kappa, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  Upsilon = expm(-Omega*eip)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}



#' @title macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models
#' @description
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return an **`xds`** object
#' @export
MBaseline.macdonald <- function(t, y, xds_obj, s) {
  return(xds_obj)
}

#' @title macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models
#' @description
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return an **`xds`** object
#' @export
MBionomics.macdonald <- function(t, y, xds_obj, s) {
  return(xds_obj)
}


#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.macdonald = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}
