# specialized methods for the adult mosquito si model

#' @title Derivatives for a simple model for adult mosquito infection dynamics
#' @description Implements [dMYZdt] for the si ODE model.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.si <- function(t, y, pars, s) {
  Lambda = pars$Lambda[[s]]
  kappa = pars$kappa[[s]]

  with(pars$ix$MYZ[[s]],{
    M <- y[M_ix]
    Z <- y[Z_ix]

    with(pars$MYZpar[[s]],{

      dM <- Lambda - (Omega %*% M)
      dZ <- f*q*kappa*(M-Z) - (Omega %*% Z)

      return(c(dM, dZ))
    })
  })
}

#' @title Derivatives for adult mosquitoes
#' @description Implements [DT_MYZt] for the si model.
#' @inheritParams DT_MYZt
#' @return a [numeric] vector
#' @export
DT_MYZt.si <- function(t, y, pars, s) {
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
#' @description Implements [xde_setup_MYZpar] for the si model
#' @inheritParams xde_setup_MYZpar
#' @return a [list] vector
#' @export
xde_setup_MYZpar.si = function(MYZname, pars, s, EIPopts, MYZopts=list(), calK){
  pars$MYZpar[[s]] = xde_make_MYZpar_si(pars$nPatches, MYZopts, calK)
  return(pars)
}


#' @title Make parameters for si ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param calK a mosquito dispersal matrix of dimensions `nPatches` by `nPatches`
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu loss to emigration emigration rate
#' @param eip the extrinsic incubation periodw
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @export
xde_make_MYZpar_si = function(nPatches, MYZopts=list(), calK,
                          g=1/12, sigma=1/8, mu=1, eip=12, f=0.3, q=0.95,
                          nu=1, eggsPerBatch=60){

  stopifnot(is.matrix(calK))
  stopifnot(dim(calK) == c(nPatches, nPatches))

  with(MYZopts,{
    MYZpar <- list()
    class(MYZpar) <- c('si')

    MYZpar$nPatches <- nPatches

    MYZpar$g      <- checkIt(g, nPatches)
    MYZpar$p      <- exp(-checkIt(g, nPatches))
    MYZpar$sigma  <- checkIt(sigma, nPatches)
    MYZpar$f      <- checkIt(f, nPatches)
    MYZpar$q      <- checkIt(q, nPatches)
    MYZpar$nu     <- checkIt(nu, nPatches)
    MYZpar$eggsPerBatch <- eggsPerBatch

    # Store as baseline values
    MYZpar$g0      <- MYZpar$g
    MYZpar$p0      <- MYZpar$p
    MYZpar$sigma0  <- MYZpar$sigma
    MYZpar$f0      <- MYZpar$f
    MYZpar$q0      <- MYZpar$q
    MYZpar$nu0     <- MYZpar$nu

    # The EIP model and the eip
    MYZpar$sEIP <- exp(-eip*g)

    MYZpar$calK <- calK

    Omega_par <- list()
    class(Omega_par) <- "static"
    MYZpar$Omega_par <- Omega_par
    MYZpar$Omega <- with(MYZpar, make_Omega_xde(g, sigma, mu, calK))

    return(MYZpar)
  })}

#' @title Setup MYZpar for the si model
#' @description Implements [dts_setup_MYZpar] for the si model
#' @inheritParams dts_setup_MYZpar
#' @return a [list] vector
#' @export
dts_setup_MYZpar.si = function(MYZname, pars, s, EIPopts, MYZopts=list(), calK){
  pars$MYZpar[[s]] = dts_make_MYZpar_si(pars$nPatches, pars$MYZday, calK, MYZopts)
  return(pars)
}


#' @title Make parameters for si adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZday is the runtime time step for the MYZ model
#' @param calK a mosquito dispersal matrix of dimensions `nPatches` by `nPatches`
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param g daily mosquito survival
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param eip the extrinsic incubation periodw
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @param p_mod a name to dispatch F_p
#' @param sigma_mod a name to dispatch F_sigma
#' @param mu_mod a name to dispatch F_sigma
#' @param f_mod a name to dispatch F_f
#' @param q_mod a name to dispatch F_q
#' @param nu_mod a name to dispatch F_nu
#' @return a [list]
#' @export
dts_make_MYZpar_si = function(nPatches, MYZday, calK, MYZopts=list(),
                              g=1/12, sigma=1/8, mu=0,
                              f=0.3, q=0.95, eip=12, nu=1,
                              eggsPerBatch=60,
                              p_mod = "static", sigma_mod = "static", mu_mod = "static",
                              f_mod = "static", q_mod = "static", nu_mod = "static"
){

  stopifnot(is.matrix(calK))
  stopifnot(dim(calK) == c(nPatches, nPatches))

  with(MYZopts,{
    MYZpar <- list()
    class(MYZpar) <- "si"

    MYZpar$nPatches <- nPatches
    if(nPatches == 1){
      sigma = 0
      calK = 1
    }

    p = exp(-g)
    ccc <-  (1-exp(-g*MYZday))/g/MYZday

    MYZpar$g       <- checkIt(g, nPatches)
    MYZpar$ccc     <- ccc
    MYZpar$p       <- exp(-g*MYZday)
    MYZpar$sigma   <- 1-exp(-checkIt(sigma, nPatches)*MYZday)
    MYZpar$mu      <- 1-exp(-checkIt(mu, nPatches))
    MYZpar$f       <- checkIt(f, nPatches)
    MYZpar$q       <- checkIt(q, nPatches)
    MYZpar$nu      <- 1-exp(-checkIt(nu, nPatches)*MYZday)
    MYZpar$eggsPerBatch <- eggsPerBatch
    MYZpar$sEIP    <- exp(-g*eip)



    # Store as baseline values
    MYZpar$g0      <- MYZpar$g
    MYZpar$p0      <- MYZpar$p
    MYZpar$sigma0  <- MYZpar$sigma
    MYZpar$mu0     <- MYZpar$mu
    MYZpar$f0      <- MYZpar$f
    MYZpar$q0      <- MYZpar$q
    MYZpar$nu0     <- MYZpar$nu

    MYZpar$p_par   <- list()
    class(MYZpar$p_par) <- p_mod
    MYZpar$f_par   <- list()
    class(MYZpar$f_par) <- f_mod
    MYZpar$q_par   <- list()
    class(MYZpar$q_par) <-  q_mod
    MYZpar$sigma_par   <- list()
    class(MYZpar$sigma_par) <- sigma_mod
    MYZpar$mu_par   <- list()
    class(MYZpar$mu_par) <- mu_mod
    MYZpar$nu_par   <- list()
    class(MYZpar$nu_par) <- nu_mod

    MYZpar$calK <- calK

    Omega_par <- list()
    class(Omega_par) = "static"
    MYZpar$Omega_par <- Omega_par
    MYZpar$Omega <- with(MYZpar, make_Omega_dts(p, sigma, mu, calK))

    return(MYZpar)
})}

#' @title Reset bloodfeeding and mortality rates to baseline
#' @description Implements [MBionomics] for the si model
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.si <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f       <- f0
    pars$MYZpar[[s]]$q       <- q0
    pars$MYZpar[[s]]$g       <- g0
    pars$MYZpar[[s]]$sigma   <- sigma0
    pars$MYZpar[[s]]$nu      <- nu0
    return(pars)
})}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the si model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.si <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], sEIP*f*q)*y[pars$ix$MYZ[[s]]$Z_ix]
}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqM] for the si model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.si <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]], f*q)*y[pars$ix$MYZ[[s]]$Z_ix]
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
#' @description Implements [setup_MYZinits] for the si model
#' @inheritParams setup_MYZinits
#' @return a [list]
#' @export
setup_MYZinits.si = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = with(pars$MYZpar[[s]], make_MYZinits_si(nPatches, MYZopts))
  return(pars)
}


#' @title Make inits for si adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M0 total mosquito density at each patch
#' @param Z0 infectious mosquito density at each patch
#' @return a [list]
#' @export
make_MYZinits_si = function(nPatches, MYZopts = list(),
                                M0=5, Z0=1){
  with(MYZopts,{
    M = checkIt(M0, nPatches)
    Z = checkIt(Z0, nPatches)
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

#' @title Make parameters for si ODE adult mosquito model
#' @param pars a [list]
#' @param g mosquito mortality rate
#' @param eip the extrinsic incubation period (in days)
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @param calK mosquito dispersal matrix of dimensions `nPatches` by `nPatches`
#' @return a [list]
#' @export
make_parameters_MYZ_si <- function(pars, g, eip, sigma, mu, f, q, nu, eggsPerBatch, calK) {
  stopifnot(is.numeric(g), is.numeric(sigma), is.numeric(f),
            is.numeric(q), is.numeric(nu), is.numeric(eggsPerBatch))

  MYZpar <- list()
  class(MYZpar) <- c('si')

  MYZpar$g      <- checkIt(g, pars$nPatches)
  MYZpar$sigma  <- checkIt(sigma, pars$nPatches)
  MYZpar$mu     <- checkIt(mu, pars$nPatches)
  MYZpar$f      <- checkIt(f, pars$nPatches)
  MYZpar$q      <- checkIt(q, pars$nPatches)
  MYZpar$nu     <- checkIt(nu, pars$nPatches)
  MYZpar$eggsPerBatch <- eggsPerBatch

  # Store as baseline values
  MYZpar$g0      <- MYZpar$g
  MYZpar$sigma0  <- MYZpar$sigma
  MYZpar$mu0     <- MYZpar$mu
  MYZpar$f0      <- MYZpar$f
  MYZpar$q0      <- MYZpar$q
  MYZpar$nu0     <- MYZpar$nu

  MYZpar$nPatches <- pars$nPatches
  MYZpar$calK <- calK

 MYZpar$sEIP <- exp(-eip*g)

  Omega_par <- list()
  class(Omega_par) <- "static"
  MYZpar$Omega_par <- Omega_par
  MYZpar$Omega <- with(MYZpar, make_Omega_xde(g, sigma, mu, calK))

  pars$MYZpar = list()
  pars$MYZpar[[1]] = MYZpar

  return(pars)
}

#' @title Make inits for si adult mosquito model
#' @param pars a [list]
#' @param M0 total mosquito density at each patch
#' @param Z0 infectious mosquito density at each patch
#' @return a [list]
#' @export
make_inits_MYZ_si <- function(pars, M0, Z0) {
  pars$MYZinits = list()
  pars$MYZinits[[1]] = list(M=M0, Z=Z0)
  return(pars)
}


#' @title Parse the output of deSolve and return variables for the si model
#' @description Implements [parse_outputs_MYZ] for the si model
#' @inheritParams parse_outputs_MYZ
#' @return a [list]
#' @export
parse_outputs_MYZ.si <- function(outputs, pars, s) {with(pars$ix$MYZ[[s]],{
  time = outputs[,1]
  M = outputs[,M_ix+1]
  Y = outputs[,Z_ix+1]
  Z = with(pars$MYZpar[[1]], sEIP*Y)
  y = Y/M
  z = Z/M
  return(list(time=time, M=M, Z=Z, Y=Y, y=y, z=z))
})}

#' @title Return initial values as a vector
#' @description Implements [get_inits_MYZ] for the si model.
#' @inheritParams get_inits_MYZ
#' @return [numeric]
#' @export
get_inits_MYZ.si <- function(pars, s) {with(pars$MYZinits[[s]],{
  c(M,  Z)
})}

#' @title Make inits for si adult mosquito model
#' @inheritParams update_inits_MYZ
#' @return a [list]
#' @export
update_inits_MYZ.si <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  Z = y0[Z_ix]
  pars$MYZinits[[s]] = make_MYZinits_si(pars$nPatches, list(), M0=M, Z0=Z)
  return(pars)
})}




