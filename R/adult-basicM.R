# specialized methods for a basic adult mosquito model

#' @title **MYZ** Component Derivatives for `basicM`
#' @description Implements [dMYZdt] for the basicM xde ODE model.
#' @details \deqn{\begin{array}{rl}dM/dt &= \Lambda(t) - \Omega \cdot M \\ dP/dt &= f(M-P) - \Omega\cdot P\end{array}}
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.basicM <- function(t, y, pars, s){

  Lambda = pars$Lambda[[s]]

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)

      return(c(dMdt, dPdt))
    })
  })
}


#' @title Set mosquito bionomics to baseline
#' @description Implements [MBaseline] for models with no forcing on the baseline
#' @inheritParams MBaseline
#' @return the model as a [list]
#' @export
MBaseline.basicM <- function(t, y, pars, s){with(pars$MYZpar[[s]],{
  # Baseline parameters
  pars$MYZpar[[s]]$f_t      <- F_f(t, vars, f_par)
  pars$MYZpar[[s]]$q_t      <- F_q(t, vars, q_par)
  pars$MYZpar[[s]]$g_t      <- F_g(t, vars, g_par)
  pars$MYZpar[[s]]$sigma_t  <- F_sigma(t, vars, sigma_par)
  pars$MYZpar[[s]]$mu       <- F_mu(t, vars, mu_par)
  pars$MYZpar[[s]]$nu       <- F_nu(t, vars, nu_par)
  pars$MYZpar[[s]]$calK     <- F_calK(t, vars, calK_par)
  pars$MYZpar[[s]]$eggsPerBatch <- eggsPerBatc
  h
  # Reset Effect Sizes
  pars$MYZpar[[s]]$es_f     < rep(1, pars$nPatches)
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
MBionomics.basicM <- function(t, y, pars, s) {with(pars$MYZpar[[s]],{
  pars$MYZpar[[s]]$f <- es_f*f_t
  pars$MYZpar[[s]]$q <- es_q*q_t
  pars$MYZpar[[s]]$g <- es_g*g_t
  pars$MYZpar[[s]]$sigma <- es_sigma*sigma_t
  pars <- make_Omega(pars, s)
  return(pars)
})}


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
#' @description Implements [setup_MYZpar] for the basicM xde model
#' @inheritParams setup_MYZpar
#' @return a [list] vector
#' @export
setup_MYZpar.basicM = function(MYZname, pars, s, MYZopts=list()){
  MYZpar <- make_MYZpar_GeRM(pars$nPatches, MYZopts)
  class(MYZpar) <- 'basicM'
  pars$MYZpar[[s]] = MYZpar
  return(pars)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MYZpars.basicM <- function(pars, s=1) {
  with(pars$MYZpar[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZpars
#' @return an **`xds`** object
#' @export
set_MYZpars.basicM <- function(pars, s=1, MYZopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZpar[[s]]$f_par <- f_par
    pars$MYZpar[[s]]$q_par <- q_par
    pars$MYZpar[[s]]$g_par <- g_par
    pars$MYZpar[[s]]$sigma_par <- sigma_par
    pars$MYZpar[[s]]$mu_par <- mu_par
    pars$MYZpar[[s]]$nu_par <- nu_par
    pars$MYZpar[[s]]$calK_par <- calK_par
    pars$MYZpar[[s]]$eggsPerBatch <- eggsPerBatch
    return(pars)
  }))}

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
  f = get_f(pars, s)
  q = get_q(pars, s)
  M = y[pars$ix$MYZ[[s]]$M_ix]
  return(f*q*M)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the basic ecology model
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.basicM <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]], {
    return(M*nu*eggsPerBatch)
  })
}

#' @title Setup the basicM model
#' @description Implements [setup_MYZinits] for the basicM model
#' @inheritParams setup_MYZinits
#' @return a [list] vector
#' @export
setup_MYZinits.basicM = function(pars, s, MYZopts){
  pars$MYZinits[[s]] = make_MYZinits_basicM(pars$nPatches, MYZopts)
  return(pars)
}


#' @title Set new MYZ parameter values
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZinits
#' @return an `xds` object
#' @export
set_MYZinits.basicM <- function(pars, s=1, MYZopts=list()) {
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZinits[[s]]$M = M
    pars$MYZinits[[s]]$P = P
    return(pars)
  }))}

#' @title Make inits for basicM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @return none
#' @export
make_MYZinits_basicM = function(nPatches, MYZopts, M=5, P=1){
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
  pars$MYZinits[[s]] = make_MYZinits_basicM(pars$nPatches, list(), M=M, P=P)
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

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MYZix] for the basic M model.
#' @inheritParams setup_MYZix
#' @return none
#' @importFrom utils tail
#' @export
setup_MYZix.basicM <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(P_ix, 1)

  pars$max_ix = max_ix

  pars$ix$MYZ[[s]] = list(M_ix=M_ix, P_ix=P_ix)

  return(pars)
})}

#' @title Parse outputs for basicM
#' @description Implements [parse_MYZorbits] for the basicM model.
#' @inheritParams parse_MYZorbits
#' @return [list]
#' @export
parse_MYZorbits.basicM <- function(outputs, pars, s) {
  with(pars$ix$MYZ[[s]],{
    M = outputs[,M_ix]
    P = outputs[,P_ix]
    parous = P/M
    return(list(M=M, P=P, parous=parous))
  })}
