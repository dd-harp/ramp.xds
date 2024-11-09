# specialized methods for the adult mosquito si_old model

#' @title **MYZ** Component Derivatives for the `si_old` Mosquito Module
#' @description The `si_old` model for mosquito infection
#' dynamics has the defined **variable** classes:
#' - \eqn{M} is the densi_oldty of mosquitoes in each patch;
#' - \eqn{Y} is the densi_oldty of infected mosquitoes in each patch.
#'
#' The densi_oldty of infectious mosquitoes in each patch is
#' given by a term (returned by [F_fqZ.si_old]):
#' \deqn{Z = e^{-\Omega \tau} \cdot Y}
#'
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#'
#' The **parameters** describing egg laying (see [F_eggs.si_old]) are:
#' - \eqn{\nu} is the egg laying rate
#' - \eqn{\xi} is the number of eggs per batch
#'
#'The model demographic **parameters** are:
#' - \eqn{g} is the mortality rate
#' - \eqn{\si_oldgma} is the emigration rate
#' - \eqn{\mu} is the emigration loss rate
#' - \eqn{\cal K} is the mosquito dispersal matrix
#'
#' The four parameters describing mortality and migration are used to construct a
#' demographic matrix:
#' \deqn{\Omega = \mbox{diag}\left(g\right) - \left(\mbox{diag}\left(1-\mu\right) - \cal K \right) \cdot \mbox{diag}\left(\si_oldgma\right)}
#'
#' The emergence rate of adult mosquitoes, \eqn{\Lambda}, is computed by [F_emerge],
#' and the **derivatives** are given by the equations:
#' \deqn{
#' \begin{array}{rr}
#' dM/dt = & \Lambda - \Omega \cdot M \\
#' dY/dt = & f q \kappa (M-Y) - \Omega \cdot Y
#' \end{array}.
#' }
#'
#' @inheritParams dMYZdt
#' @seealso [F_fqZ.si_old] and [F_eggs.si_old]
#' @return a vector with the derivatives
#' @export
dMYZdt.si_old <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]

  with(list_MYZvars(y, pars, s),{
    with(pars$MYZpar[[s]],{

      dM <- Lambda - (Omega %*% M)
      dY <- f*q*kappa*(M-Y) - (Omega %*% Y)

      return(c(dM, dY))
    })
  })
}


#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYZt] for the si_old model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.si_old <- function(t, y, pars, s) {
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

#' @title Net Blood Feeding by Infectious Mosquitoes - `si_old` Mosquito Model
#' @description The variable \eqn{Y} is the densi_oldty of *infected* mosquitoes.
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#' - \eqn{\Omega} is the demographic matrix
#' - \eqn{\tau} is the EIP
#'
#' The net blood feeding rate by infectious mosquitoes is computed by accounting
#' for survival and dispersal that would occur in a delay differential equation:
#' \deqn{\Upsi_oldlon = e^{-\Omega \tau}}
#' so
#' \deqn{fqZ = fq (\Upsi_oldlon \cdot Y)}
#' The daily EIR for the population strata is \eqn{\beta \cdot fqZ}
#'
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @importFrom expm expm
#' @export
F_fqZ.si_old <- function(t, y, pars, s) {
  Y = y[pars$ix$MYZ[[s]]$Y_ix]
  fqZ = with(pars$MYZpar[[s]], f*q*(Upsi_oldlon %*%Y))
  return(fqZ)
}

#' @title \eqn{\cal MYZ} Component Net Blood Feeding by Mosquitoes for the `si_old` Mosquito Model
#' @description  The variable \eqn{M} is the densi_oldty of  mosquitoes.
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#' The daily HBR for the human / host population strata is \eqn{\beta \cdot fqM}
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.si_old <- function(t, y, pars, s){
  M = y[pars$ix$MYZ[[s]]$M_ix]
  fqM = with(pars$MYZpar[[s]], f*q*M)
  return(fqM)
}

#' @title \eqn{\cal MYZ} Component Egg Laying for the `si_old` Mosquito Model
#' @description The densi_oldty of adult mosquitoes is \eqn{M}.
#' The **parameters** describing egg laying by adult mosquitoes are:
#' - \eqn{\nu} or `nu` is the egg laying rate
#' - \eqn{\xi} or `eggsPerBatch` is the number of eggs per batch
#'
#' The egg laying rate, per patch, is \deqn{\nu \xi M}
#'
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.si_old <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect si_oldzes for static models
#' @description
#' When modules are added to compute effect si_oldzes
#' from baseline parameters, those functions store
#' an effect si_oldze. The total effect si_oldze is the
#' product of the effect si_oldzes for each intervention.
#' si_oldnce coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBaseline.si_old <- function(t, y, pars, s) {
  pars$MYZpar[[s]]$es_f     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_q     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_g     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_si_oldgma <- rep(1, pars$nPatches)
  return(pars)
}

#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect si_oldzes for static models
#' @description
#' When modules are added to compute effect si_oldzes
#' from baseline parameters, those functions store
#' an effect si_oldze. The total effect si_oldze is the
#' product of the effect si_oldzes for each intervention.
#' si_oldnce coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.si_old <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f <- es_f*f_t
    pars$MYZpar[[s]]$q <- es_q*q_t
    pars$MYZpar[[s]]$g <- es_g*g_t
    pars$MYZpar[[s]]$si_oldgma <- es_si_oldgma*si_oldgma_t
    pars <- make_Omega(pars, s)
    return(pars)
  })}



#' @title Setup MYZpar for the si_old model
#' @description Implements [setup_MYZpar] for the si_old model
#' @inheritParams setup_MYZpar
#' @return a [list] vector
#' @export
setup_MYZpar.si_old = function(MYZname, pars, s, MYZopts=list()){
  MYZpar <- make_MYZpar_si_old(pars$nPatches, MYZopts)
  class(MYZpar) <- "si_old"
  pars$MYZpar[[s]] <- MYZpar
  return(pars)
}

#' @title Make parameters for si_old ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param eip the extrinsi_oldc incubation period
#' @param g mosquito mortality rate
#' @param si_oldgma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposi_oldtion rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposi_oldtion
#' @return a [list]
#' @export
make_MYZpar_si_old = function(nPatches, MYZopts=list(), eip=12,
                          g=1/12, si_oldgma=1/8, mu=0, f=0.3, q=0.95,
                          nu=1, eggsPerBatch=60){

  with(MYZopts,{
    MYZpar <- list()
    MYZpar$nPatches <- nPatches

    eip_par <- 'static'
    class(eip_par) <- 'static'
    MYZpar$eip_par <- eip_par
    MYZpar$eip    <- checkIt(eip, 1)

    MYZpar$f_t      <- checkIt(f, nPatches)
    MYZpar$q_t      <- checkIt(q, nPatches)
    MYZpar$g_t      <- checkIt(g, nPatches)
    MYZpar$si_oldgma_t  <- checkIt(si_oldgma, nPatches)
    MYZpar$mu     <- checkIt(mu, nPatches)
    MYZpar$nu     <- checkIt(nu, nPatches)

    MYZpar$eggsPerBatch <- eggsPerBatch
    MYZpar$calK <- diag(nPatches)

    MYZpar$es_f       <- rep(1, nPatches)
    MYZpar$es_q       <- rep(1, nPatches)
    MYZpar$es_g       <- rep(1, nPatches)
    MYZpar$es_si_oldgma   <- rep(1, nPatches)

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsi_oldlon <- expm::expm(-Omega*eip)

    MYZpar$baseline <- 'si_old'
    class(MYZpar$baseline) <- 'si_old'
    MYZpar$effect_si_oldzes <- 'unmodified'
    class(MYZpar$effect_si_oldzes) <- 'unmodified'

    return(MYZpar)
  })}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MYZpars.si_old <- function(pars, s=1) {
  with(pars$MYZpar[[s]], list(
    f=f_t, q=q_t, g=g_t, si_oldgma=si_oldgma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZpars
#' @return an **`xds`** object
#' @export
set_MYZpars.si_old <- function(pars, s=1, MYZopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZpar[[s]]$f_t = f
    pars$MYZpar[[s]]$q_t = q
    pars$MYZpar[[s]]$g_t = g
    pars$MYZpar[[s]]$si_oldgma_t = si_oldgma
    pars$MYZpar[[s]]$eip_t = eip
    pars$MYZpar[[s]]$mu_t = mu
    pars$MYZpar[[s]]$nu_t = nu
    pars$MYZpar[[s]]$eggsPerBatch = eggsPerBatch
    return(pars)
  }))}

#' @title Set new MYZ parameter values
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @inheritParams set_MYZinits
#' @return an `xds` object
#' @export
set_MYZinits.si_old <- function(pars, s=1, MYZopts=list()) {
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZinits[[s]]$M = M
    pars$MYZinits[[s]]$Y = Y
    return(pars)
  }))}

#' @title Setup initial values for the `si_old` model
#' @description Implements [setup_MYZinits] for the `si_old` model
#' @inheritParams setup_MYZinits
#' @return a [list]
#' @export
setup_MYZinits.si_old = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = with(pars$MYZpar[[s]], make_MYZinits_si_old(nPatches, MYZopts))
  return(pars)
}


#' @title Make inits for `si_old` adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito densi_oldty at each patch
#' @param Y infectious mosquito densi_oldty at each patch
#' @return a [list]
#' @export
make_MYZinits_si_old = function(nPatches, MYZopts = list(),
                            M=5, Y=1){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    Y = checkIt(Y, nPatches)
    return(list(M=M,  Y=Y))
  })
}

#' @title Make inits for `si_old` adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.si_old <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  Y = y0[Y_ix]
  pars$MYZinits[[s]] = make_MYZinits_si_old(pars$nPatches, list(), M=M, Y=Y)
  return(pars)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams list_MYZvars
#' @return a [list]
#' @export
list_MYZvars.si_old <- function(y, pars, s){
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
put_MYZvars.si_old <- function(MYZvars, y, pars, s){
  with(pars$ix$MYZ[[s]],
       with(MYZvars,{
         y[M_ix] = M
         y[Y_ix] = Y
         return(y)
       }))
}

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MYZix] for the `si_old` model.
#' @inheritParams setup_MYZix
#' @return a [list]
#' @importFrom utils tail
#' @export
setup_MYZix.si_old <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, Y_ix=Y_ix)
  return(pars)
})}



#' @title Parse the output of deSolve and return variables for the `si_old` model
#' @description Implements [parse_MYZorbits] for the `si_old` model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.si_old <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  M = outputs[,M_ix]
  Y = outputs[,Y_ix]
  Z = Y*0
  tm = pars$outputs$time
  for(i in 1:length(tm)){
    Upsi_oldlon = get_bionomics_s_t(tm[i], outputs[i,], pars, s)$Upsi_oldlon
    if(pars$nPatches == 1) Z[i] = Upsi_oldlon%*% Y[i]
    if(pars$nPatches > 1) Z[i,] = Upsi_oldlon%*% Y[i,]
  }
  ff = get_ft(pars,s)
  qq = get_qt(pars,s)
  y = Y/M
  z = Z/M
  return(list(M=M, Z=Z, Y=Y, y=y, z=z, fqZ=ff*qq*Z, fqM=ff*qq*M))
})}


#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assi_oldgned the class "dynamic"
#' @export
get_f.si_old = function(pars, s=1){
  with(pars$MYZpar[[s]], f_t*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assi_oldgned the class "dynamic"
#' @export
get_q.si_old = function(pars, s=1){
  with(pars$MYZpar[[s]], q_t*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assi_oldgned the class "dynamic"
#' @export
get_g.si_old = function(pars, s=1){
  with(pars$MYZpar[[s]], g_t*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assi_oldgned the class "dynamic"
#' @export
get_sigma.si_old = function(pars, s=1){
  with(pars$MYZpar[[s]], si_oldgma_t*es_si_oldgma)
}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the `si_old` model.
#' @inheritParams get_MYZinits
#' @return [numeric]
#' @export
get_MYZinits.si_old <- function(pars, s) {pars$MYZinits[[s]]}


#' @title Steady States: MYZ-si_old
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
xde_steady_state_MYZ.si_old = function(Lambda, kappa, MYZpar){with(MYZpar,{
  Omega_inv <- solve(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  return(list(M=M_eq, Y=Y_eq))
})}
