# specialized methods for the adult mosquito RM_dts model


#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the RM_dts model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.RM_dts <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]*pars$MYZday
  kappa = pars$kappa[[s]]


  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

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

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the RM_dts model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.RM_dts <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], f*q)*y[pars$ix$MYZ[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the RM_dts model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.RM_dts <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], f*q)*y[pars$ix$MYZ[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the RM_dts model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.RM_dts <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Setup initial values for the RM_dts model
#' @description Implements [make_MYZinits] for the RM_dts model
#' @inheritParams make_MYZinits
#' @return a [list]
#' @export
make_MYZinits.RM_dts = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = with(pars$MYZpar[[s]], create_MYZinits_RM_dts(nPatches, max_eip, MYZopts))
  return(pars)
}

#' @title Make inits for RM_dts adult mosquito model
#' @param nPatches the number of patches in the model
#' @param max_eip the maximum number of EIP cohorts, an [integer]
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param U total uninfected mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
create_MYZinits_RM_dts = function(nPatches, max_eip, MYZopts = list(),
                                M=5, P=1, U=0, Y=1, Z=1){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    U = checkIt(U, nPatches)
    Y = checkIt(Y, nPatches*max_eip)
    Z = checkIt(Z, nPatches)
    return(list(M=M, P=P, U=U, Y=Y, Z=Z))
  })
}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for the RM_dts model.
#' @inheritParams make_indices_MYZ
#' @return a [list]
#' @importFrom utils tail
#' @export
make_indices_MYZ.RM_dts <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  U_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(U_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches*MYZpar[[s]]$max_eip)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, P_ix=P_ix, U_ix=U_ix, Y_ix=Y_ix, Z_ix=Z_ix)
  return(pars)
})}


#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams list_MYZvars
#' @return a [list]
#' @export
list_MYZvars.RM_dts <- function(y, pars, s){
  with(pars$ix$MYZ[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         U = y[U_ix],
         Y = matrix(y[Y_ix], pars$nPatches, pars$MYZpar[[s]]$max_eip),
         Z = y[Z_ix]
  )))
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams put_MYZvars
#' @return a [list]
#' @export
put_MYZvars.RM_dts <- function(MYZvars, y, pars, s){
  with(pars$ix$MYZ[[s]],
    with(MYZvars,{
      y[M_ix] = M
      y[P_ix] = P
      y[U_ix] = U
      y[Y_ix] = Y
      y[Z_ix] = Z
      return(y)
  }))
}


#' @title Parse the output of deSolve and return variables for the RM_dts model
#' @description Implements [parse_outputs_MYZ] for the RM_dts model
#' @inheritParams parse_outputs_MYZ
#' @return a [list]
#' @export
parse_outputs_MYZ.RM_dts <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  time = outputs[,1]
  M = outputs[,M_ix+1]
  P = outputs[,P_ix+1]
  U = outputs[,U_ix+1]
  Y = rowSums(outputs[,Y_ix+1])
  Z = outputs[,Z_ix+1]
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(time=time, M=M, P=P, U=U, Y=Y, Z=Z, y=y, z=z, parous=parous))
})}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the RM_dts model.
#' @inheritParams get_MYZinits
#' @return [list]
#' @export
get_MYZinits.RM_dts <- function(pars, s) {with(pars$MYZinits[[s]],{
  list(M=M, P=P, U=U, Y=Y, Z=Z)
})}

#' @title Make inits for RM_dts adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.RM_dts <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  P = y0[P_ix]
  U = y0[U_ix]
  Y = y0[Y_ix]
  Z = y0[Z_ix]
  pars$MYZinits[[s]] = create_MYZinits_RM_dts(pars$nPatches, max_eip, list(), M=M, P=P, U=U, Y=Y, Z=Z)
  return(pars)
})}

