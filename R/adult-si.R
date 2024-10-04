# specialized methods for the adult mosquito SI model

#' @title **MYZ** Component Derivatives for the `SI` Mosquito Module
#' @description The `SI` model for mosquito infection
#' dynamics has the defined **variable** classes:
#' - \eqn{M} is the density of mosquitoes in each patch;
#' - \eqn{Y} is the density of infected mosquitoes in each patch.
#'
#' The density of infectious mosquitoes in each patch is
#' given by a term (returned by [F_fqZ.SI]):
#' \deqn{Z = e^{-\Omega \tau} \cdot Y}
#'
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#'
#' The **parameters** describing egg laying (see [F_eggs.SI]) are:
#' - \eqn{\nu} is the egg laying rate
#' - \eqn{\xi} is the number of eggs per batch
#'
#'The model demographic **parameters** are:
#' - \eqn{g} is the mortality rate
#' - \eqn{\sigma} is the emigration rate
#' - \eqn{\mu} is the emigration loss rate
#' - \eqn{\cal K} is the mosquito dispersal matrix
#'
#' The four parameters describing mortality and migration are used to construct a
#' demographic matrix:
#' \deqn{\Omega = \mbox{diag}\left(g\right) - \left(\mbox{diag}\left(1-\mu\right) - \cal K \right) \cdot \mbox{diag}\left(\sigma\right)}
#'
#' The emergence rate of adult mosquitoes, \eqn{\Lambda}, is computed by [F_emerge],
#' and the **derivatives** returned by [dMYZdt.SI] are given by the equations:
#' \deqn{
#' \begin{array}{rr}
#' dM/dt =& \Lambda &- \Omega \cdot M \\
#' dY/dt =& f q \kappa (M-Y) &- \Omega \cdot Y \\
#' \end{array}.
#' }
#'
#' @seealso [F_fqZ.SI] and [F_eggs.SI]
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.SI <- function(t, y, pars, s) {
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
#' @description Implements [Update_MYZt] for the SI model.
#' @inheritParams Update_MYZt
#' @return a [numeric] vector
#' @export
Update_MYZt.SI <- function(t, y, pars, s) {
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

#' @title Net Blood Feeding by Infectious Mosquitoes - `SI` Mosquito Model
#' @description The variable \eqn{Y} is the density of *infected* mosquitoes.
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#' - \eqn{\Omega} is the demographic matrix
#' - \eqn{\tau} is the EIP
#'
#' The net blood feeding rate by infectious mosquitoes is computed by accounting
#' for survival and dispersal that would occur in a delay differential equation:
#' \deqn{\Upsilon = e^{-\Omega \tau}}
#' so
#' \deqn{fqZ = fq (\Upsilon \cdot Y)}
#' The daily EIR for the population strata is \eqn{\beta \cdot fqZ}
#'
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @importFrom expm expm
#' @export
F_fqZ.SI <- function(t, y, pars, s) {
  Y = y[pars$ix$MYZ[[s]]$Y_ix]
  fqZ = with(pars$MYZpar[[s]], f*q*(Upsilon %*%Y))
  return(fqZ)
}

#' @title \eqn{\cal MYZ} Component Net Blood Feeding by Mosquitoes for the `SI` Mosquito Model
#' @description  The variable \eqn{M} is the density of  mosquitoes.
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#' The daily HBR for the human / host population strata is \eqn{\beta \cdot fqM}
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.SI <- function(t, y, pars, s){
  M = y[pars$ix$MYZ[[s]]$M_ix]
  fqM = with(pars$MYZpar[[s]], f*q*M)
  return(fqM)
}

#' @title \eqn{\cal MYZ} Component Egg Laying for the `SI` Mosquito Model
#' @description The density of adult mosquitoes is \eqn{M}.
#' The **parameters** describing egg laying by adult mosquitoes are:
#' - \eqn{\nu} or `nu` is the egg laying rate
#' - \eqn{\xi} or `eggsPerBatch` is the number of eggs per batch
#'
#' The egg laying rate, per patch, is \deqn{\nu \xi M}
#'
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.SI <- function(t, y, pars, s) {
  M <- y[pars$ix$MYZ[[s]]$M_ix]
  with(pars$MYZpar[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models
#' @description
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBaseline.SI <- function(t, y, pars, s) {
  pars$MYZpar[[s]]$es_f     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_q     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_g     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_sigma <- rep(1, pars$nPatches)
  return(pars)
}

#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect sizes for static models
#' @description
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.SI <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f <- es_f*f_t
    pars$MYZpar[[s]]$q <- es_q*q_t
    pars$MYZpar[[s]]$g <- es_g*g_t
    pars$MYZpar[[s]]$sigma <- es_sigma*sigma_t
    pars <- make_Omega(pars, s)
    return(pars)
  })}



#' @title Setup MYZpar for the SI model
#' @description Implements [setup_MYZpar] for the SI model
#' @inheritParams setup_MYZpar
#' @return a [list] vector
#' @export
setup_MYZpar.SI = function(MYZname, pars, s, MYZopts=list()){
  MYZpar <- make_MYZpar_SI(pars$nPatches, MYZopts)
  class(MYZpar) <- "SI"
  pars$MYZpar[[s]] <- MYZpar
  return(pars)
}

#' @title Make parameters for si ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @export
make_MYZpar_SI = function(nPatches, MYZopts=list(), eip=12,
                            g=1/12, sigma=1/8, mu=0, f=0.3, q=0.95,
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
    MYZpar$sigma_t  <- checkIt(sigma, nPatches)
    MYZpar$mu     <- checkIt(mu, nPatches)
    MYZpar$nu     <- checkIt(nu, nPatches)

    MYZpar$eggsPerBatch <- eggsPerBatch
    MYZpar$calK <- diag(nPatches)

    MYZpar$es_f       <- rep(1, nPatches)
    MYZpar$es_q       <- rep(1, nPatches)
    MYZpar$es_g       <- rep(1, nPatches)
    MYZpar$es_sigma   <- rep(1, nPatches)

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsilon <- expm::expm(-Omega*eip)

    MYZpar$baseline <- 'SI'
    class(MYZpar$baseline) <- 'SI'
    MYZpar$effect_sizes <- 'unmodified'
    class(MYZpar$effect_sizes) <- 'unmodified'

    return(MYZpar)
  })}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MYZpars.SI <- function(pars, s=1) {
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
set_MYZpars.SI <- function(pars, s=1, MYZopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZpar[[s]]$f_t = f
    pars$MYZpar[[s]]$q_t = q
    pars$MYZpar[[s]]$g_t = g
    pars$MYZpar[[s]]$sigma_t = sigma
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
set_MYZinits.SI <- function(pars, s=1, MYZopts=list()) {
  with(pars$MYZpar[[s]], with(MYZopts,{
    pars$MYZinits[[s]]$M = M
    pars$MYZinits[[s]]$Y = Y
    return(pars)
}))}

#' @title Setup initial values for the `SI` model
#' @description Implements [setup_MYZinits] for the `SI` model
#' @inheritParams setup_MYZinits
#' @return a [list]
#' @export
setup_MYZinits.SI = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = with(pars$MYZpar[[s]], make_MYZinits_SI(nPatches, MYZopts))
  return(pars)
}


#' @title Make inits for `SI` adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param Y infectious mosquito density at each patch
#' @return a [list]
#' @export
make_MYZinits_SI = function(nPatches, MYZopts = list(),
                              M=5, Y=1){
  with(MYZopts,{
    M = checkIt(M, nPatches)
    Y = checkIt(Y, nPatches)
    return(list(M=M,  Y=Y))
  })
}

#' @title Make inits for `SI` adult mosquito model
#' @inheritParams update_MYZinits
#' @return a [list]
#' @export
update_MYZinits.SI <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  Y = y0[Y_ix]
  pars$MYZinits[[s]] = make_MYZinits_SI(pars$nPatches, list(), M=M, Y=Y)
  return(pars)
})}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams list_MYZvars
#' @return a [list]
#' @export
list_MYZvars.SI <- function(y, pars, s){
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
put_MYZvars.SI <- function(MYZvars, y, pars, s){
  with(pars$ix$MYZ[[s]],
       with(MYZvars,{
         y[M_ix] = M
         y[Y_ix] = Y
         return(y)
       }))
}

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MYZix] for the `SI` model.
#' @inheritParams setup_MYZix
#' @return a [list]
#' @importFrom utils tail
#' @export
setup_MYZix.SI <- function(pars, s) {with(pars,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  pars$max_ix = max_ix
  pars$ix$MYZ[[s]] = list(M_ix=M_ix, Y_ix=Y_ix)
  return(pars)
})}



#' @title Parse the output of deSolve and return variables for the `SI` model
#' @description Implements [parse_MYZorbits] for the `SI` model
#' @inheritParams parse_MYZorbits
#' @return a [list]
#' @export
parse_MYZorbits.SI <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
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


#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.SI = function(pars, s=1){
  with(pars$MYZpar[[s]], f_t*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.SI = function(pars, s=1){
  with(pars$MYZpar[[s]], q_t*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.SI = function(pars, s=1){
  with(pars$MYZpar[[s]], g_t*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.SI = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma_t*es_sigma)
}

#' @title Return initial values as a vector
#' @description Implements [get_MYZinits] for the `SI` model.
#' @inheritParams get_MYZinits
#' @return [numeric]
#' @export
get_MYZinits.SI <- function(pars, s) {pars$MYZinits[[s]]}


#' @title Steady States: MYZ-SI
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
xde_steady_state_MYZ.SI = function(Lambda, kappa, MYZpar){with(MYZpar,{
  Omega_inv <- solve(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  return(list(M=M_eq, Y=Y_eq))
})}
