# specialized methods for a basic adult mosquito model

#' @title Derivatives for adult mosquitoes
#' @description Implements [dMYZdt] for the basicM xde ODE model.
#' @details \deqn{\begin{array}{rl}\frac{dM}{dt} &= \Lambda(t) - \Omega \cdot M \\ \frac{dP}{dt} &= f(M-P) - \Omega\cdot P\end{array}}
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.basicM <- function(t, y, pars, s){

  Lambda = pars$Lambda[[s]]

  with(pars$ix$MYZ[[s]],{
    M <- y[M_ix]
    P <- y[P_ix]

    with(pars$MYZpar[[s]]$now,{

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)

      return(c(dMdt, dPdt))
    })
  })
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MYZpar`
#' @inheritParams xde_steady_state_M
#' @return none
#' @export
xde_steady_state_M.basicM = function(Lambda, MYZpar){with(MYZpar,{
  Omega_inv <- solve(Omega)
  M_eq  <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  return(c(M=M_eq, P=P_eq))
})}

# specialized methods for the adult mosquito basicM model

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the basicM model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.basicM <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]*pars$Dday

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

      Mt <- Lambda + Omega %*% M
      Pt <- f*(M-P) + Omega %*% P

      return(c(Mt, Pt))
    })
  })
}



#' @title Setup MYZpar for the basicM xde model
#' @description Implements [make_MYZpar] for the basicM xde model
#' @inheritParams make_MYZpar
#' @return a [list] vector
#' @export
make_MYZpar.basicM = function(MYZname, pars, s, MYZopts=list()){
  MYZpar <- create_MYZpar_RM_static(pars$nPatches, MYZopts)
  class(MYZpar) <- 'basicM'
  pars$MYZpar[[s]] = MYZpar
  return(pars)
}



#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the basicM xde model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.basicM <- function(t, y, pars, s) {
  0*y[pars$ix$MYZ[[s]]$M_ix]
}

#' @title The net blood feeding rate of the mosquito population in a patch
#' @description Implements [F_fqM] for the basicM xde model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.basicM <- function(t, y, pars, s) {
  M = y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]]$now, f*q*M)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the basic ecology model
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.basicM <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]]$now, {
    return(M*nu*eggsPerBatch)
  })
}

#' @title Setup the basicM model
#' @description Implements [make_MYZinits] for the basicM model
#' @inheritParams make_MYZinits
#' @return a [list] vector
#' @export
make_MYZinits.basicM = function(pars, s, MYZopts){
  pars$MYZinits[[s]] = create_MYZinits_basicM(pars$nPatches, MYZopts)
  return(pars)
}

#' @title Make inits for basicM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @return none
#' @export
create_MYZinits_basicM = function(nPatches, MYZopts, M=5, P=1){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    return(list(M=M, P=P))
  })}


#' @title Make inits for RM adult mosquito model
#' @inheritParams update_MYZinits
#' @return none
#' @export
update_MYZinits.basicM <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  P = y0[P_ix]
  pars$MYZinits[[s]] = create_MYZinits_basicM(pars$nPatches, list(), M=M, P=P)
  return(pars)
})}


#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams list_MYZvars
#' @return a [list]
#' @export
list_MYZvars.basicM <- function(y, pars, s){
  with(pars$ix$MYZ[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix]
       )))
}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the basicM model.
#' @inheritParams get_MYZinits
#' @return none
#' @export
get_MYZinits.basicM <- function(pars, s) {
  pars$MYZinits[[s]]
}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [make_indices_MYZ] for the basic M model.
#' @inheritParams make_indices_MYZ
#' @return none
#' @importFrom utils tail
#' @export
make_indices_MYZ.basicM <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(P_ix, 1)

  pars$max_ix = max_ix

  pars$ix$MYZ[[s]] = list(M_ix=M_ix, P_ix=P_ix)

  return(pars)
})}

#' @title Parse the output of deSolve and return variables for the basicM model
#' @description Implements [parse_outputs_MYZ] for the basicM model.
#' @inheritParams parse_outputs_MYZ
#' @return [list]
#' @export
parse_outputs_MYZ.basicM <- function(outputs, pars, s) {
  time = outputs[,1]
  with(pars$ix$MYZ[[s]],{
    M = outputs[,M_ix+1]
    P = outputs[,P_ix+1]
    parous = P/M
    return(list(time=time, M=M, P=P, parous=parous))
})}
