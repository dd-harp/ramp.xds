# specialized methods for the adult mosquito GeRM model

#' @title The **GeRM** Module Skill Set 
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
skill_set_MY.GeRM = function(MYname){
  return(list())
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_MY.GeRM = function(xds_obj, s){
  
  return(xds_obj)
}


#' @title Compute Derivatives for **MY** module `GeRM` 
#' 
#' @description Compute the derivatives for the generalized, non-autonomous Ross-Macdonald model
#' for mosquito ecology and infection dynamics.
#' @inheritParams dMYdt
#' @return a [numeric] vector
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
#' @export
dMYdt.GeRM <- function(t, y, xds_obj, s){
  
  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{
      
      deip_dt = dF_eip(t, xds_obj, s)

      if (t <= eip) {
        M_eip <- xds_obj$MYinits[[s]]$M
        Y_eip <- xds_obj$MYinits[[s]]$Y
        fqkappa_eip <- kappa*f*q
        g_eip <- g
        sigma_eip <- sigma
      } else {
        M_eip <- lagvalue(t=t-eip, nr = xds_obj$ix$MY[[s]]$M_ix)
        Y_eip <- lagvalue(t=t-eip, nr = xds_obj$ix$MY[[s]]$Y_ix)
        fqkappa_eip <- lagderiv(t=t-eip, nr = xds_obj$ix$MY[[s]]$fqkappa_ix)
        g_eip <- lagderiv(t=t-eip, nr = xds_obj$ix$MY[[s]]$g_ix)
        sigma_eip <- lagderiv(t=t-eip, nr = xds_obj$ix$MY[[s]]$sigma_ix)
      }

      Omega_eip <- compute_Omega_xde(g_eip, sigma_eip, mu, calK)

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      dYdt <- f*q*kappa*(M - Y) - (Omega %*% Y)
      dZdt <- U %*% (fqkappa_eip * (M_eip - Y_eip)) - (Omega %*% Z)
      dUdt <- ((1-deip_dt)*Omega_eip - Omega) %*% U

      return(c(dMdt, dPdt, dYdt, dZdt, as.vector(dUdt), f*q*kappa, g, sigma))
    })
  })
}


#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the GeRM model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.GeRM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$ix$MY[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the GeRM model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.GeRM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  f*q*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the GeRM model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.GeRM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}



#' @title Setup MY_obj for the GeRM model
#' @description Implements [setup_MY_obj] for the GeRM model
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @export
setup_MY_obj.GeRM = function(MYname, xds_obj, s, options=list()){
  xds_obj = ode_to_dde(xds_obj)
  MY_obj <- make_MY_obj_GeRM(xds_obj$nPatches, options)
  class(MY_obj) <- 'GeRM'
  xds_obj$MY_obj[[s]] = MY_obj
  return(xds_obj)
}

#' @title Make parameters for GeRM ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param options a [list] of values that overwrites the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @export
make_MY_obj_GeRM = function(nPatches, options=list(), eip =12,
                              g=1/12,  sigma=1/8,  mu=0,
                              f=0.3,  q=0.95,
                              nu=1,  eggsPerBatch=60){

  with(options,{

    MY_obj <- list()

    MY_obj$nPatches <- nPatches

    MY_obj <- setup_eip_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_f_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_q_obj(checkIt(q, nPatches), MY_obj) 
    MY_obj <- setup_g_obj(checkIt(g, nPatches), MY_obj) 
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj) 
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj) 
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj) 

    MY_obj <- setup_K_obj(nPatches, MY_obj) 

    Omega <- diag(g, nPatches)
    MY_obj$Omega <- Omega
    MY_obj$Upsilon <- expm::expm(-Omega*eip)
    MY_obj$nPatches <- nPatches

    Omega <- diag(g, nPatches)
    MY_obj$Omega <- Omega
    MY_obj$Upsilon <- expm::expm(-Omega*eip)
    MY_obj$nPatches <- nPatches

    MY_obj$eggsPerBatch <- eggsPerBatch

    MY_obj$baseline <- MY_obj
    class(MY_obj$baseline) <- 'GeRM'

    return(MY_obj)
  })}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the GeRM model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @importFrom utils tail
#' @export
setup_MY_ix.GeRM <- function(xds_obj, s) {with(xds_obj,{
  
  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)
  
  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)
  
  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)
  
  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)
  
  U_ix <- seq(from = max_ix+1, length.out = nPatches^2)
  max_ix <- tail(U_ix, 1)
  
  fqkappa_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(fqkappa_ix, 1)
  
  g_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(g_ix, 1)
  
  sigma_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(sigma_ix, 1)
  
  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, P_ix=P_ix, Y_ix=Y_ix, Z_ix=Z_ix,
                         U_ix = U_ix, fqkappa_ix=fqkappa_ix,
                         g_ix=g_ix, sigma_ix=sigma_ix)
  return(xds_obj)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @export
get_MY_vars.GeRM <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix, return(list(
    M = y[M_ix],
    P = y[P_ix],
    Y = y[Y_ix],
    Z = y[Z_ix],
    U = matrix(y[U_ix], xds_obj$nPatches, xds_obj$nPatches)
)))}

#' @title parse the output of deSolve and return variables for the GeRM model
#' @description Implements [parse_MY_orbits] for the GeRM model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @export
parse_MY_orbits.GeRM <- function(outputs, xds_obj, s) {with(xds_obj$MY_obj[[s]]$ix,{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  Y = outputs[,Y_ix]
  Z = outputs[,Z_ix]
  f = get_ft(xds_obj, s)
  q = get_qt(xds_obj, s)
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parous=parous, fqZ=f*q*Z, fqM=f*q*M))
})}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MY_pars.GeRM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @export
change_MY_pars.GeRM <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_par <- f_par
    xds_obj$MY_obj[[s]]$q_par <- q_par
    xds_obj$MY_obj[[s]]$g_par <- g_par
    xds_obj$MY_obj[[s]]$sigma_par <- sigma_par
    xds_obj$MY_obj[[s]]$mu_par <- mu_par
    xds_obj$MY_obj[[s]]$nu_par <- nu_par
    xds_obj$MY_obj[[s]]$eip_par <- eip_par
    xds_obj$MY_obj[[s]]$calK_par <- calK_par
    xds_obj$MY_obj[[s]]$eggsPerBatch <- eggsPerBatch
    return(xds_obj)
  }))}


#' @title Set mosquito bionomics to baseline
#' @description Implements [MBaseline] for models with no forcing on the baseline
#' @inheritParams MBaseline
#' @return the model as a [list]
#' @export
MBaseline.GeRM <- function(t, y, xds_obj, s){with(xds_obj$MY_obj[[s]],{
  vars = xds_obj$vars
  # Baseline parameters
  xds_obj$MY_obj[[s]]$f_t      <- F_feeding_rate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$q_t      <- F_human_frac(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$g_t      <- F_mozy_mort(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$sigma_t  <- F_emigrate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$mu       <- F_dispersal_loss(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$nu       <- F_batch_rate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$eip      <- F_eip(t, xds_obj, s) 
  xds_obj                      <- F_K_matrix(t, xds_obj, s) 
  
  # Reset Effect Sizes
  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  return(xds_obj)
})}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.GeRM <- function(t, y, xds_obj, s) {with(xds_obj$MY_obj[[s]],{
  xds_obj$MY_obj[[s]]$f <- es_f*f_t
  xds_obj$MY_obj[[s]]$q <- es_q*q_t
  xds_obj$MY_obj[[s]]$g <- es_g*g_t
  xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
  xds_obj <- make_Omega(xds_obj, s)
  return(xds_obj)
})}

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the GeRM_dts model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @export
Update_MYt.GeRM <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$Lambda[[s]]*xds_obj$MYday
  kappa = xds_obj$kappa[[s]]
  xds_obj <- update_Omega(xds_obj, s)

  with(get_MY_vars(y, xds_obj, s),{
    with(MY_obj,{

      eip_day_ix = (t %% max_eip) + 1
      eip_yday_ix = ((t-1) %% max_eip) + 1
      Gix = c(t-1:max_eip) %% max_eip + 1
      Gt <- G[Gix]

      Mt <- Lambda + Omega %*% M
      Pt <- f*(M-P) + Omega %*% P
      Ut <- Lambda + Omega %*% (exp(-f*q*kappa)*U)
      Yt <- Omega %*% (Y %*% diag(1-Gt))
      Zt <- Omega %*% (Y%*%Gt)  + (Omega %*% Z)

      Yt[,eip_yday_ix]  <- Yt[,eip_yday_ix] + Yt[,eip_day_ix]
      Yt[,eip_day_ix] <- Omega %*% ((1-exp(-f*q*kappa))*U)

      return(list(M=unname(Mt), P=unname(Pt), U=unname(Ut), Y=unname(as.vector(Yt)), Z=unname(Zt)))
    })
  })
}


#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the GeRM_dts model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @export
Update_MYt.GeRM <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$Lambda[[s]]*xds_obj$MYday
  kappa = xds_obj$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      eip_day_ix = (t %% max_eip) + 1
      eip_yday_ix = ((t-1) %% max_eip) + 1
      Gix = c(t-1:max_eip) %% max_eip + 1
      Gt <- G[Gix]

      Mt <- Lambda + Omega %*% M
      Pt <- ff*(M-P) + Omega %*% P
      Ut <- Lambda + Omega %*% (exp(-ff*q*kappa)*U)
      Yt <- Omega %*% (Y %*% diag(1-Gt))
      Zt <- Omega %*% (Y%*%Gt)  + (Omega %*% Z)

      Yt[,eip_yday_ix]  <- Yt[,eip_yday_ix] + Yt[,eip_day_ix]
      Yt[,eip_day_ix] <- Omega %*% ((1-exp(-f*q*kappa))*U)

      return(list(M=unname(Mt), P=unname(Pt), U=unname(Ut), Y=unname(as.vector(Yt)), Z=unname(Zt)))
    })
  })
}



#' @title Setup initial values for the GeRM model
#' @description Implements [setup_MY_inits] for the GeRM model
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @export
setup_MY_inits.GeRM = function(xds_obj, s, options=list()){with(xds_obj$MY_obj[[s]], {
  Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)
  Upsilon = expm(-Omega*eip)
  xds_obj$MYinits[[s]] = make_MY_inits_GeRM(nPatches, Upsilon, options)
  return(xds_obj)
})}


#' @title Make inits for GeRM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param Upsilon a matrix describing survival and dispersal through the EIP
#' @param options a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
make_MY_inits_GeRM = function(nPatches, Upsilon, options = list(),
                             M=5, P=1, Y=1, Z=1){
  stopifnot(dim(Upsilon) == c(nPatches,nPatches))
  with(options,{
    M = unname(checkIt(M, nPatches))
    P = unname(checkIt(P, nPatches))
    Y = unname(checkIt(Y, nPatches))
    Z = unname(checkIt(Z, nPatches))
    U = unname(Upsilon)
    dd = rep(0, nPatches)
    return(list(M=M, P=P, Y=Y, Z=Z, U=U, d1=dd, d2=dd, d3=dd))
  })
}

#' @title Set new MY parameter values
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_inits
#' @return an `xds` object
#' @export
change_MY_inits.GeRM <- function(xds_obj, s=1, options=list()) {
  with(xds_obj$MY_obj[[s]]$inits, with(options,{
    xds_obj$MYinits[[s]]$M = M
    xds_obj$MYinits[[s]]$Y = Y
    xds_obj$MYinits[[s]]$P = P
    xds_obj$MYinits[[s]]$Z = Z
    return(xds_obj)
  }))}




#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritParams steady_state_MY
#' @return none
#' @importFrom MASS ginv
#' @export
steady_state_MY.GeRM = function(Lambda, kappa, xds_obj, s=1){with(xds_obj$MY_obj[[s]],{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}



#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.GeRM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.GeRM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.GeRM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.GeRM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}
