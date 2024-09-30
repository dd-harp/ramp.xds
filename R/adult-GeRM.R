# specialized methods for the adult mosquito GeRM model

#' @title **MYZ** Component Derivatives for the `GeRM` model
#' @description Compute the derivatives for the generalized, non-autonomous Ross-Macdonald model
#' for mosquito ecology and infection dynamics.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
#' @export
dMYZdt.GeRM <- function(t, y, pars, s){
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]
  deip_dt = d_F_eip_dt(t, pars$vars, pars$MYZpar[[s]]$eip_par)

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

      if (t <= eip) {
        M_eip <- pars$MYZinits[[s]]$M
        Y_eip <- pars$MYZinits[[s]]$Y
        fqkappa_eip <- kappa*f*q
        g_eip <- g
        sigma_eip <- sigma
      } else {
        M_eip <- lagvalue(t=t-eip, nr = pars$ix$MYZ[[s]]$M_ix)
        Y_eip <- lagvalue(t=t-eip, nr = pars$ix$MYZ[[s]]$Y_ix)
        fqkappa_eip <- lagderiv(t=t-eip, nr = pars$ix$MYZ[[s]]$fqkappa_ix)
        g_eip <- lagderiv(t=t-eip, nr = pars$ix$MYZ[[s]]$g_ix)
        sigma_eip <- lagderiv(t=t-eip, nr = pars$ix$MYZ[[s]]$sigma_ix)
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


#' @title Setup MYZpar for the GeRM model
#' @description Implements [make_MYZpar] for the GeRM model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.GeRM = function(MYZname, pars, s, MYZopts=list()){
  pars = xds_dde(pars)
  MYZpar <- create_MYZpar_GeRM(pars$nPatches, MYZopts)
  class(MYZpar) <- 'GeRM'
  pars$MYZpar[[s]] = MYZpar
  return(pars)
}

#' @title Make parameters for GeRM ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZopts a [list] of values that overwrites the defaults
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
create_MYZpar_GeRM = function(nPatches, MYZopts=list(), eip =12,
                              g=1/12,  sigma=1/8,  mu=0,
                              f=0.3,  q=0.95,
                              nu=1,  eggsPerBatch=60){

  with(MYZopts,{

    MYZpar <- list()

    MYZpar$nPatches <- nPatches

    eip_par <- list()
    class(eip_par) <- 'static'
    eip_par$eip = eip
    MYZpar$eip_par <- eip_par
    MYZpar$eip=eip

    f=checkIt(f, nPatches)
    MYZpar$f_par <- list()
    class(MYZpar$f_par) <- "static"
    MYZpar$f_par$f = f
    MYZpar$f_t = f
    MYZpar$es_f = 1

    q=checkIt(q, nPatches)
    MYZpar$q_par <- list()
    class(MYZpar$q_par) <- "static"
    MYZpar$q_par$q = q
    MYZpar$q_t = q
    MYZpar$es_q = 1

    g=checkIt(g, nPatches)
    MYZpar$g_par <- list()
    class(MYZpar$g_par) <- "static"
    MYZpar$g_par$g = g
    MYZpar$g_t = g
    MYZpar$es_g = 1

    mu=checkIt(mu, nPatches)
    MYZpar$mu_par <- list()
    class(MYZpar$mu_par) <- "static"
    MYZpar$mu_par$mu = mu
    MYZpar$mu = mu

    sigma=checkIt(sigma, nPatches)
    MYZpar$sigma_par <- list()
    class(MYZpar$sigma_par) <- "static"
    MYZpar$sigma_par$sigma = sigma
    MYZpar$sigma_t = sigma
    MYZpar$es_sigma = 1

    nu=checkIt(nu, nPatches)
    MYZpar$nu_par <- list()
    class(MYZpar$nu_par) <- "static"
    MYZpar$nu_par$nu = nu
    MYZpar$nu=nu

    calK = diag(nPatches)
    MYZpar$calK_par <- list()
    class(MYZpar$calK_par) <- "static"
    MYZpar$calK_par$calK = calK
    MYZpar$calK=calK

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsilon <- expm::expm(-Omega*eip)
    MYZpar$nPatches <- nPatches

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsilon <- expm::expm(-Omega*eip)
    MYZpar$nPatches <- nPatches

    MYZpar$eggsPerBatch <- eggsPerBatch

    MYZpar$baseline <- MYZpar
    class(MYZpar$baseline) <- 'GeRM'

    return(MYZpar)
  })}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MYZpars.GeRM <- function(pars, s=1) {
  with(pars$MYZpar[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZpars
#' @return an **`xds`** object
#' @export
set_MYZpars.GeRM <- function(pars, s=1, MYZopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZpar[[s]]$f_par <- f_par
    pars$MYZpar[[s]]$q_par <- q_par
    pars$MYZpar[[s]]$g_par <- g_par
    pars$MYZpar[[s]]$sigma_par <- sigma_par
    pars$MYZpar[[s]]$mu_par <- mu_par
    pars$MYZpar[[s]]$nu_par <- nu_par
    pars$MYZpar[[s]]$eip_par <- eip_par
    pars$MYZpar[[s]]$calK_par <- calK_par
    pars$MYZpar[[s]]$eggsPerBatch <- eggsPerBatch
    return(pars)
  }))}


#' @title Set mosquito bionomics to baseline
#' @description Implements [MBaseline] for models with no forcing on the baseline
#' @inheritParams MBaseline
#' @return the model as a [list]
#' @export
MBaseline.GeRM <- function(t, y, pars, s){with(pars$MYZpar[[s]],{
  vars = pars$vars
  # Baseline parameters
  pars$MYZpar[[s]]$f_t      <- F_f(t, vars, f_par)
  pars$MYZpar[[s]]$q_t      <- F_q(t, vars, q_par)
  pars$MYZpar[[s]]$g_t      <- F_g(t, vars, g_par)
  pars$MYZpar[[s]]$sigma_t  <- F_sigma(t, vars, sigma_par)
  pars$MYZpar[[s]]$mu       <- F_mu(t, vars, mu_par)
  pars$MYZpar[[s]]$nu       <- F_nu(t, vars, nu_par)
  pars$MYZpar[[s]]$eip      <- F_eip(t, vars, eip_par)
  pars$MYZpar[[s]]$calK     <- F_calK(t, vars, calK_par)
  pars$MYZpar[[s]]$eggsPerBatch <- eggsPerBatch
  # Reset Effect Sizes
  pars$MYZpar[[s]]$es_f     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_q     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_g     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_sigma <- rep(1, pars$nPatches)
  return(pars)
})}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.GeRM <- function(t, y, pars, s) {with(pars$MYZpar[[s]],{
  pars$MYZpar[[s]]$f <- es_f*f_t
  pars$MYZpar[[s]]$q <- es_q*q_t
  pars$MYZpar[[s]]$g <- es_g*g_t
  pars$MYZpar[[s]]$sigma <- es_sigma*sigma_t
  pars <- make_Omega(pars, s)
  return(pars)
})}

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the GeRM_dts model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.GeRM <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]*pars$MYZday
  kappa = pars$kappa[[s]]
  pars <- update_Omega(pars, s)

  with(list_MYZvars(y, pars, s),{
    with(MYZpar,{

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

#' @title Make inits for GeRM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param Upsilon a matrix describing survival and dispersal through the EIP
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
create_MYZinits_GeRM = function(nPatches, Upsilon, MYZopts = list(),
                              M=5, P=1, Y=1, Z=1){
  stopifnot(dim(Upsilon) == c(nPatches,nPatches))
  with(MYZopts,{
    M = unname(checkIt(M, nPatches))
    P = unname(checkIt(P, nPatches))
    Y = unname(checkIt(Y, nPatches))
    Z = unname(checkIt(Z, nPatches))
    U = unname(Upsilon)
    dd = rep(0, nPatches)
    return(list(M=M, P=P, Y=Y, Z=Z, U=U, d1=dd, d2=dd, d3=dd))
  })
}

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the GeRM_dts model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.GeRM <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]*pars$MYZday
  kappa = pars$kappa[[s]]

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

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

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the GeRM model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.GeRM <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  f*q*y[pars$ix$MYZ[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the GeRM model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.GeRM <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  f*q*y[pars$ix$MYZ[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the GeRM model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.GeRM <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]],{
    return(M*nu*eggsPerBatch)
  })
}




#' @title Setup initial values for the GeRM model
#' @description Implements [make_MYZinits] for the GeRM model
#' @inheritParams make_MYZinits
#' @return a [list]
#' @export
make_MYZinits.GeRM = function(pars, s, MYZopts=list()){with(pars$MYZpar[[s]], {
  Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)
  Upsilon = expm(-Omega*eip)
  pars$MYZinits[[s]] = create_MYZinits_GeRM(nPatches, Upsilon, MYZopts)
  return(pars)
})}

#' @title Set new MYZ parameter values
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZinits
#' @return an `xds` object
#' @export
set_MYZinits.GeRM <- function(pars, s=1, MYZopts=list()) {
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZinits[[s]]$M = M
    pars$MYZinits[[s]]$Y = Y
    pars$MYZinits[[s]]$P = P
    pars$MYZinits[[s]]$Z = Z
    return(pars)
  }))}

#' @title Return initial values as a list
#' @description Implements [get_MYZinits] for the GeRM model.
#' @inheritParams get_MYZinits
#' @return [list]
#' @export
get_MYZinits.GeRM <- function(pars, s=1){
  pars$MYZinits[[s]]
}

#' @title Update inits for GeRM adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.GeRM <- function(pars, y0, s) {
  with(pars$ix$MYZ[[s]],{
    M = y0[M_ix]
    P = y0[P_ix]
    Y = y0[Y_ix]
    Z = y0[Z_ix]
    pars = make_MYZinits(pars, s, list(M=M, P=P, Y=Y, Z=Z))
    return(pars)
  })}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams list_MYZvars
#' @return a [list]
#' @export
list_MYZvars.GeRM <- function(y, pars, s){
  with(pars$ix$MYZ[[s]], return(list(
    M = y[M_ix],
    P = y[P_ix],
    Y = y[Y_ix],
    Z = y[Z_ix],
    U = matrix(y[U_ix], pars$nPatches, pars$nPatches)
)))}


#' @title Parse the output of deSolve and return variables for the GeRM model
#' @description Implements [parse_MYZorbits] for the GeRM model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.GeRM <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  Y = outputs[,Y_ix]
  Z = outputs[,Z_ix]
  f = get_ft(pars, s)
  q = get_qt(pars, s)
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, Y=Y, Z=Z, y=y, z=z, parous=parous, fqZ=f*q*Z, fqM=f*q*M))
})}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @importFrom MASS ginv
#' @export
xde_steady_state_MYZ.GeRM = function(Lambda, kappa, MYZpar){with(MYZpar,{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for the GeRM model.
#' @inheritParams make_indices_MYZ
#' @return a [list]
#' @importFrom utils tail
#' @export
make_indices_MYZ.GeRM <- function(pars, s) {with(pars,{

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

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, P_ix=P_ix, Y_ix=Y_ix, Z_ix=Z_ix,
                          U_ix = U_ix, fqkappa_ix=fqkappa_ix,
                          g_ix=g_ix, sigma_ix=sigma_ix)
  return(pars)
})}


#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], f_t*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], q_t*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], g_t*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma_t*es_sigma)
}
