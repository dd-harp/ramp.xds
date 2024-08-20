# specialized methods for the adult mosquito sei model

#' @title Derivatives for a oviposition model for adult mosquito infection dynamics
#' @description Implements [dMYZdt] for the sei ODE model.
#' @details
#' The dynamics of adult mosquitoes:
#' \deqn{\frac{dM}{dt} = \Lambda - \Omega \cdot M}
#' For infected mosquitoes:
#' \deqn{\frac{dY}{dt} = fq\kappa(M-Y-Z) - \Omega \cdot Y - Y/\tau}
#' For infectious mosquitoes:
#' \deqn{\frac{dZ}{dt} = Y/\tau - \Omega \cdot Z}
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.sei <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

      f = f_t*es_f; q = q_t*es_q
      Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)

      dM <- Lambda - (Omega %*% M)
      dY <- f*q*kappa*(M-Y-Z) - Omega %*% Y - Y/eip
      dZ <- Y/eip - (Omega %*% Z)

      return(c(dM, dY, dZ))
    })
  })
}

#' @title Steady States: MYZ-sei
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
xde_steady_state_MYZ.sei = function(Lambda, kappa, MYZpar){with(MYZpar,{
  Omega_inv <- solve(Omega)
  Z_eq <- as.vector(solve(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  return(c(M=M_eq, Z=Z_eq))
})}

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the sei model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.sei <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]
  D = pars$MYZday

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{
      Mt <- ccc*Lambda*D + Omega %*% M
      Zt <- Omega%*%((1-exp(-f*q*kappa*pars$MYZday))*(M-Z)) + (Omega %*% Z)
      Zt <- Zt + (1-exp(-ccc))*(1-exp(-f*q*kappa*pars$MYZday))*Lambda*ccc*D
      return(list(M=unname(Mt), Z=unname(Zt)))
    })
  })
}

#' @title Setup MYZpar for the sei model
#' @description Implements [make_MYZpar] for the sei model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.sei = function(MYZname, pars, s, MYZopts=list()){
  setup_as = with(MYZopts, ifelse(exists("setup_as"), setup_as, "RM"))
  if(setup_as == "GeRM"){
    MYZpar <- create_MYZpar_GeRM(pars$nPatches, MYZopts)
  } else {
    MYZpar <- create_MYZpar_RM(pars$nPatches, MYZopts)
  }
  class(MYZpar) <- 'sei'
  pars$MYZpar[[s]] <- MYZpar
  return(pars)
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the sei model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.sei <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  Z = y[pars$ix$MYZ[[s]]$Z_ix]
  return(f*q*Z)
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the sei model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.sei <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  M = y[pars$ix$MYZ[[s]]$M_ix]
  return(f*q*M)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the sei model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.sei <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]],{
    return(M*nu*eggsPerBatch)
  })
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams list_MYZvars
#' @return a [list]
#' @export
list_MYZvars.sei <- function(y, pars, s){
  with(pars$ix$MYZ[[s]],
       return(list(
         M = y[M_ix],
         Y = y[Y_ix],
         Z = y[Z_ix]
       )))
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams put_MYZvars
#' @return a [list]
#' @export
put_MYZvars.sei <- function(MYZvars, y, pars, s){
  with(pars$ix$MYZ[[s]],
       with(MYZvars,{
         y[M_ix] = M
         y[Y_ix] = Y
         y[Z_ix] = Z
         return(y)
       }))
}





#' @title Setup initial values for the sei model
#' @description Implements [make_MYZinits] for the sei model
#' @inheritParams make_MYZinits
#' @return a [list]
#' @export
make_MYZinits.sei = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = with(pars$MYZpar[[s]], create_MYZinits_sei(nPatches, MYZopts))
  return(pars)
}


#' @title Make inits for sei adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param Y total infected mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
create_MYZinits_sei = function(nPatches, MYZopts = list(),
                              M=5, Y=1, Z=0){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    Y = checkIt(Y, nPatches)
    Z = checkIt(Z, nPatches)
    return(list(M=M, Y=Y, Z=Z))
  })
}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for the sei model.
#' @inheritParams make_indices_MYZ
#' @return a [list]
#' @importFrom utils tail
#' @export
make_indices_MYZ.sei <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, Y_ix=Y_ix, Z_ix=Z_ix)
  return(pars)
})}


#' @title Parse the output of deSolve and return variables for the sei model
#' @description Implements [parse_MYZorbits] for the sei model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.sei <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  M = outputs[,M_ix]
  Y = outputs[,Y_ix]
  Z = outputs[,Z_ix]
  f = get_ft(pars,s)
  q = get_ft(pars,s)
  y = Y/M
  z = Z/M
  return(list(M=M, Z=Z, Y=Y, y=y, z=z, fqZ=f*q*Z, fqM=f*q*M))
})}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the sei model.
#' @inheritParams get_MYZinits
#' @return [numeric]
#' @export
get_MYZinits.sei <- function(pars, s) {
  pars$MYZinits[[s]]
}

#' @title Make inits for sei adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.sei <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  Y = y0[Y_ix]
  Z = y0[Z_ix]
  pars$MYZinits[[s]] = create_MYZinits_sei(pars$nPatches, list(), M=M, Y=Y, Z=Z)
  return(pars)
})}
