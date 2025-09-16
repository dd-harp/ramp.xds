# specialized methods for the adult mosquito RMdts model

#' @title The **RMdts** Module Skill Set 
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
skill_set_MY.RMdts = function(MYname){
  return(list())
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_MY.RMdts = function(xds_obj, s){
  return(xds_obj)
}


#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the RMdts model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @export
Update_MYt.RMdts <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$ML_interface$Lambda[[s]]*xds_obj$MYday
  kappa = xds_obj$kappa[[s]]


  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

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
#' @description Implements [F_fqZ] for the RMdts model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.RMdts <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], f*q)*y[xds_obj$ix$MY[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the RMdts model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.RMdts <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]], f*q)*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the RMdts model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.RMdts <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Setup initial values for the RMdts model
#' @description Implements [setup_MY_inits] for the RMdts model
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @export
setup_MY_inits.RMdts = function(xds_obj, s, options=list()){
  xds_obj$MY_obj[[s]]$inits = with(xds_obj$MY_obj[[s]], make_MY_inits_RMdts(nPatches, max_eip, options))
  return(xds_obj)
}

#' @title Make inits for RMdts adult mosquito model
#' @param nPatches the number of patches in the model
#' @param max_eip the maximum number of EIP cohorts, an [integer]
#' @param options a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @param U total uninfected mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
make_MY_inits_RMdts = function(nPatches, max_eip, options = list(),
                                  M=5, P=1, U=0, Y=1, Z=1){
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    U = checkIt(U, nPatches)
    Y = checkIt(Y, nPatches*max_eip)
    Z = checkIt(Z, nPatches)
    return(list(M=M, P=P, U=U, Y=Y, Z=Z))
  })
}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the RMdts model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @importFrom utils tail
#' @export
setup_MY_ix.RMdts <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(P_ix, 1)

  U_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(U_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches*MY_obj[[s]]$max_eip)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  xds_obj$max_ix = max_ix
  xds_obj$ix$MY[[s]] = list(M_ix=M_ix, P_ix=P_ix, U_ix=U_ix, Y_ix=Y_ix, Z_ix=Z_ix)
  return(xds_obj)
})}


#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @export
get_MY_vars.RMdts <- function(y, xds_obj, s){
  with(xds_obj$ix$MY[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix],
         U = y[U_ix],
         Y = matrix(y[Y_ix], xds_obj$nPatches, xds_obj$MY_obj[[s]]$max_eip),
         Z = y[Z_ix]
       )))
}


#' @title parse the output of deSolve and return variables for the RMdts model
#' @description Implements [parse_MY_orbits] for the RMdts model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @export
parse_MY_orbits.RMdts <- function(outputs, xds_obj, s) {with(xds_obj$ix$MY[[s]],{
  M = outputs[,M_ix]
  P = outputs[,P_ix]
  U = outputs[,U_ix]
  Y = rowSums(outputs[,Y_ix])
  Z = outputs[,Z_ix]
  y = Y/M
  z = Z/M
  parous = P/M
  return(list(M=M, P=P, U=U, Y=Y, Z=Z, y=y, z=z, parous=parous))
})}

#' @title Make inits for RMdts adult mosquito model
#' @inheritParams change_MY_inits
#' @return a [list]
#' @export
change_MY_inits.RMdts <- function(xds_obj, s, options) {
  with(xds_obj$MY_obj[[s]]$ix,{
    with(options,{
    M = y0[M_ix]
    P = y0[P_ix]
    U = y0[U_ix]
    Y = y0[Y_ix]
    Z = y0[Z_ix]
    xds_obj$MY_obj[[s]]$inits = make_MY_inits_RMdts(xds_obj$nPatches, max_eip, list(), M=M, P=P, U=U, Y=Y, Z=Z)
    return(xds_obj)
})})}

