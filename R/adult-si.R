# specialized methods for the adult mosquito si model

#' @title Derivatives for a simple model for adult mosquito infection dynamics
#' @description Implements [dMYZdt] for the si ODE model.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.si <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]
  MYZpar = update_MYZpar_RM(pars$MYZpar[[s]])

  with(pars$ix$MYZ[[s]],{
    M <- y[M_ix]
    Z <- y[Z_ix]

    with(MYZpar,{
      dM <- Lambda - (Omega %*% M)
      dZ <- f*q*kappa*(M-Z) - (Omega %*% Z)

      return(c(dM, dZ))
    })
  })
}

#' @title Steady States: MYZ-si
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
xde_steady_state_MYZ.si = function(Lambda, kappa, MYZpar){with(MYZpar,{
  Omega_inv <- solve(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  Z_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  return(list(M=M_eq, Z=Z_eq))
})}

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the si model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.si <- function(t, y, pars, s) {
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

#' @title Setup MYZpar for the si model
#' @description Implements [make_MYZpar] for the si model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.si = function(MYZname, pars, s, MYZopts=list()){
  MYZpar = create_MYZpar_RM_static(pars$nPatches, MYZopts)
  class(MYZpar) <- "si"
  pars$MYZpar[[s]] <- MYZpar
  return(pars)
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the si model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.si <- function(t, y, pars, s) {
  Z = y[pars$ix$MYZ[[s]]$Z_ix]
  with(pars$MYZpar[[s]], f*q*(Upsilon %*% Z))
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the si model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.si <- function(t, y, pars, s) {
  M = y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]], f*q*M)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the si model.
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.si <- function(t, y, pars, s) {
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
list_MYZvars.si <- function(y, pars, s){
  with(pars$ix$MYZ[[s]],
       return(list(
         M = y[M_ix],
         Z = y[Z_ix]
       )))
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams put_MYZvars
#' @return a [list]
#' @export
put_MYZvars.si <- function(MYZvars, y, pars, s){
  with(pars$ix$MYZ[[s]],
       with(MYZvars,{
         y[M_ix] = M
         y[Z_ix] = Z
         return(y)
       }))
}





#' @title Setup initial values for the si model
#' @description Implements [make_MYZinits] for the si model
#' @inheritParams make_MYZinits
#' @return a [list]
#' @export
make_MYZinits.si = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = with(pars$MYZpar[[s]], create_MYZinits_si(nPatches, MYZopts))
  return(pars)
}


#' @title Make inits for si adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param Z infectious mosquito density at each patch
#' @return a [list]
#' @export
create_MYZinits_si = function(nPatches, MYZopts = list(),
                                M=5, Z=1){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    Z = checkIt(Z, nPatches)
    return(list(M=M,  Z=Z))
  })
}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for the si model.
#' @inheritParams make_indices_MYZ
#' @return a [list]
#' @importFrom utils tail
#' @export
make_indices_MYZ.si <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  Z_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Z_ix, 1)

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, Z_ix=Z_ix)
  return(pars)
})}



#' @title Parse the output of deSolve and return variables for the si model
#' @description Implements [parse_MYZorbits] for the si model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.si <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  M = outputs[,M_ix]
  Y = outputs[,Z_ix]
  Z = t(with(pars$MYZpar[[1]], Upsilon %*% t(Y)))
  ff = get_ft(pars,s)
  qq = get_qt(pars,s)
  y = Y/M
  z = Z/M
  return(list(M=M, Z=Z, Y=Y, y=y, z=z, fqZ=ff*qq*Z, fqM=ff*qq*M))
})}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the si model.
#' @inheritParams get_MYZinits
#' @return [numeric]
#' @export
get_MYZinits.si <- function(pars, s) {pars$MYZinits[[s]]}

#' @title Make inits for si adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.si <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  Z = y0[Z_ix]
  pars$MYZinits[[s]] = create_MYZinits_si(pars$nPatches, list(), M=M, Z=Z)
  return(pars)
})}
