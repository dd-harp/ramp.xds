# specialized methods for the adult mosquito si model

#' @title \eqn{\cal MYZ} Component Derivatives for the `si` Mosquito Model
#' @description Derivatives for a simple SI model for mosquito infection
#' dynamics, where \eqn{M} is the density of mosquitoes, and \eqn{Y} the
#' density of infected mosquitoes:
#' \deqn{
#' \begin{array}{rr}
#' dM/dt =& \Lambda &- \Omega \cdot M \\
#' dY/dt =& f q \kappa (M-Y) &- \Omega \cdot Y
#' \end{array}.
#' }
#' The model assumes \eqn{Z = \Upsilon Y}
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.si <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

      f = f_t*es_f; q = q_t*es_q
      Omega = compute_Omega_xde(g_t*es_g, sigma_t*es_sigma, mu, calK)

      dM <- Lambda - (Omega %*% M)
      dY <- f*q*kappa*(M-Y) - (Omega %*% Y)

      return(c(dM, dY))
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
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  return(list(M=M_eq, Y=Y_eq))
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
      Yt <- Omega%*%((1-exp(-f*q*kappa*pars$MYYday))*(M-Y)) + (Omega %*% Y)
      Yt <- Yt + (1-exp(-ccc))*(1-exp(-f*q*kappa*pars$MYYday))*Lambda*ccc*D
      return(list(M=unname(Mt), Y=unname(Yt)))
    })
  })
}

#' @title Setup MYZpar for the si model
#' @description Implements [make_MYZpar] for the si model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.si = function(MYZname, pars, s, MYZopts=list()){
  setup_as = with(MYZopts, ifelse(exists("setup_as"), setup_as, "RM"))
  if(setup_as == "GeRM"){
    MYZpar <- create_MYZpar_GeRM(pars$nPatches, MYZopts)
  } else {
    MYZpar <- create_MYZpar_RM(pars$nPatches, MYZopts)
  }
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
  f = get_f(pars, s)
  q = get_q(pars, s)
  Y = y[pars$ix$MYZ[[s]]$Y_ix]
  Upsilon = compute_Upsilon(pars, s)
  return(f*q*(Upsilon %*% Y))
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the si model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.si <- function(t, y, pars, s) {
  f = get_f(pars, s)
  q = get_q(pars, s)
  M = y[pars$ix$MYZ[[s]]$M_ix]
  return(f*q*M)
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
         Y = y[Y_ix]
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
         y[Y_ix] = Y
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
#' @param Y infectious mosquito density at each patch
#' @return a [list]
#' @export
create_MYZinits_si = function(nPatches, MYZopts = list(),
                                M=5, Y=1){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    Y = checkIt(Y, nPatches)
    return(list(M=M,  Y=Y))
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

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, Y_ix=Y_ix)
  return(pars)
})}



#' @title Parse the output of deSolve and return variables for the si model
#' @description Implements [parse_MYZorbits] for the si model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.si <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  M = outputs[,M_ix]
  Y = outputs[,Y_ix]
  Z = Y*0
  tm = pars$outputs$time
  for(i in 1:length(tm)){
    Upsilon = get_bionomics_s_t(tm[i], outputs[i,], pars, s)$Upsilon
    if(pars$nPatches == 1) Z[i] = Upsilon%*% Y[i]
    if(pars$nPatches > 1) Z[i,] = Upsilon%*% Y[i,]
  }
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
  Y = y0[Y_ix]
  pars$MYZinits[[s]] = create_MYZinits_si(pars$nPatches, list(), M=M, Y=Y)
  return(pars)
})}
