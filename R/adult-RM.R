# specialized methods for the adult mosquito RM model

#' @title Derivatives for adult mosquitoes
#' @description Implements [dMYZdt] for the RM DDE model.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @importFrom deSolve lagvalue
#' @importFrom deSolve lagderiv
#' @export
dMYZdt.RM <- function(t, y, pars, s){
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]
  deip_dt = d_F_eip_dt(t, pars$vars, pars$MYZpar[[s]]$eip_par)
  U = matrix(y[pars$ix$MYZ[[s]]$U_ix], pars$nPatches, pars$nPatches)

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{
      f = f_t*es_f
      q = q_t*es_q
      g = g_t*es_g
      sigma = sigma_t*es_sigma
      Omega = compute_Omega_xde(g, sigma, mu, calK)

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




#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the RM_dts model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.RM <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]*pars$MYZday
  kappa = pars$kappa[[s]]
  MYZpar <- MYZ_rates2probs(pars$MYZpar[[s]], pars$runtime)
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

#' @title Make inits for RM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param Upsilon a matrix describing survival and dispersal through the EIP
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
create_MYZinits_RM = function(nPatches, Upsilon, MYZopts = list(),
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
#' @description Implements [Update_MYZt] for the RM_dts model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.RM <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]*pars$MYZday
  kappa = pars$kappa[[s]]
  MYZ_rates2probs_RM(pars$MYZpar[[s]], pars$runtime)

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
#' @description Implements [F_fqZ] for the RM model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.RM <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  f*q*y[pars$ix$MYZ[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the RM model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.RM <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  f*q*y[pars$ix$MYZ[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the RM model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.RM <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Setup MYZpar for the RM model
#' @description Implements [make_MYZpar] for the RM model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.RM = function(MYZname, pars, s, MYZopts=list()){
  pars$dlay <- 'dde'
  class(pars$dlay) <- 'dde'
  setup_as = with(MYZopts, ifelse(exists("setup_as"), setup_as, "RM"))
  if(setup_as == "GeRM"){
    MYZpar <- create_MYZpar_GeRM(pars$nPatches, MYZopts)
  } else {
    MYZpar <- create_MYZpar_RM(pars$nPatches, MYZopts)
  }
  class(MYZpar) <- 'RM'
  pars$MYZpar[[s]] = MYZpar
  return(pars)
}

#' @title Setup initial values for the RM model
#' @description Implements [make_MYZinits] for the RM model
#' @inheritParams make_MYZinits
#' @return a [list]
#' @export
make_MYZinits.RM = function(pars, s, MYZopts=list()){with(pars$MYZpar[[s]], {
  Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)
  Upsilon = expm(-Omega*eip)
  pars$MYZinits[[s]] = create_MYZinits_RM(nPatches, Upsilon, MYZopts)
  return(pars)
})}

#' @title Return initial values as a list
#' @description Implements [get_MYZinits] for the RM model.
#' @inheritParams get_MYZinits
#' @return [list]
#' @export
get_MYZinits.RM <- function(pars, s=1){
  pars$MYZinits[[s]]
}

#' @title Update inits for RM adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.RM <- function(pars, y0, s) {
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
list_MYZvars.RM <- function(y, pars, s){
  with(pars$ix$MYZ[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         Y = y[Y_ix],
         Z = y[Z_ix]
       )))
}


#' @title Parse the output of deSolve and return variables for the RM model
#' @description Implements [parse_MYZorbits] for the RM model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.RM <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
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
xde_steady_state_MYZ.RM = function(Lambda, kappa, MYZpar){with(MYZpar,{
  kappa = as.vector(kappa); Lambda = as.vector(Lambda)
  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))
  return(list(M=M_eq, P=P_eq, Y=Y_eq, Z=Z_eq))
})}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for the RM model.
#' @inheritParams make_indices_MYZ
#' @return a [list]
#' @importFrom utils tail
#' @export
make_indices_MYZ.RM <- function(pars, s) {with(pars,{

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
