# specialized methods for a basic adult mosquito model

#' @title Derivatives for adult mosquitoes
#' @description Implements [dMYZdt] for the basicM xde ODE model.
#' @inheritParams dMYZdt
#' @return a [numeric] vector
#' @export
dMYZdt.basicM <- function(t, y, pars, s){

  Lambda = pars$Lambda[[s]]

  with(pars$ix$MYZ[[s]],{
    M <- y[M_ix]
    P <- y[P_ix]

    with(pars$MYZpar[[s]],{

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)

      return(c(dMdt, dPdt))
    })
  })
}

# specialized methods for the adult mosquito basicM model

#' @title Derivatives for adult mosquitoes
#' @description Implements [DT_MYZt] for the basicM model.
#' @inheritParams DT_MYZt
#' @return a [numeric] vector
#' @export
DT_MYZt.basicM <- function(t, y, pars, s) {
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
#' @description Implements [xde_setup_MYZpar] for the basicM xde model
#' @inheritParams xde_setup_MYZpar
#' @return a [list] vector
#' @export
xde_setup_MYZpar.basicM = function(MYZname, pars, s, EIPopts, MYZopts=list(), calK){
  pars$MYZpar[[s]] = xde_make_MYZpar_basicM(pars$nPatches, MYZopts, calK)
  return(pars)
}

#' @title Make parameters for basicM ODE adult mosquito model
#' @param nPatches the number of patches
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param calK mosquito dispersal matrix of dimensions `nPatches` by `nPatches`
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f blood feeding rate
#' @param q human blood feeding fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list] with a configured MYZpar
#' @export
xde_make_MYZpar_basicM = function(nPatches, MYZopts=list(), calK,
                              g=1/12, sigma=1/8, mu=0,
                              f=0.3, q=0.95,
                              nu=1, eggsPerBatch=60){
  stopifnot(is.matrix(calK))
  stopifnot(dim(calK) == c(nPatches, nPatches))
  with(MYZopts,{
    MYZpar <- list()
    class(MYZpar) <- "basicM"

    MYZpar$xde <- "ode"
    class(MYZpar$xde) <- "ode"

    MYZpar$nPatches <- nPatches

    MYZpar$g      <- checkIt(g, nPatches)
    MYZpar$sigma  <- checkIt(sigma, nPatches)
    MYZpar$mu     <- checkIt(mu, nPatches)
    MYZpar$f      <- checkIt(f, nPatches)
    MYZpar$q      <- checkIt(q, nPatches)
    MYZpar$nu     <- checkIt(nu, nPatches)
    MYZpar$eggsPerBatch <- eggsPerBatch

    # Store as baseline values
    MYZpar$g0      <- MYZpar$g
    MYZpar$sigma0  <- MYZpar$sigma
    MYZpar$mu0     <- MYZpar$mu
    MYZpar$f0      <- MYZpar$f
    MYZpar$q0      <- MYZpar$q
    MYZpar$nu0     <- MYZpar$nu

    MYZpar$calK <- calK

    Omega_par <- list()
    class(Omega_par) <- "static"
    MYZpar$Omega_par <- Omega_par
    MYZpar$Omega <- with(MYZpar, make_Omega_xde(g, sigma, mu, calK))

    return(MYZpar)
  })}


#' @title Setup MYZpar for the basicM model
#' @description Implements [dts_setup_MYZpar] for the basicM model
#' @inheritParams dts_setup_MYZpar
#' @return a [list] vector
#' @export
dts_setup_MYZpar.basicM = function(MYZname, pars, s, EIPopts=list(), MYZopts=list(), calK){
  pars$MYZpar[[s]] = dts_make_MYZpar_basicM(pars$nPatches, MYZopts, calK)
  return(pars)
}

#' @title Make parameters for basicM adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param calK a mosquito dispersal matrix of dimensions `nPatches` by `nPatches`
#' @param p daily mosquito survival
#' @param sigma emigration rate
#' @param mu emigration survival
#' @param f feeding rate
#' @param q human blood fraction
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
dts_make_MYZpar_basicM = function(nPatches, MYZopts=list(), calK,
                                  p=11/12, sigma=1/8, mu=1, f=0.3, q=0.95,
                                  nu=1, eggsPerBatch=60,
                                  p_mod = "static",
                                  sigma_mod = "static",
                                  mu_mod = "static",
                                  f_mod = "static",
                                  q_mod = "static",
                                  nu_mod = "static"){

  stopifnot(is.matrix(calK))
  stopifnot(dim(calK) == c(nPatches, nPatches))

  with(MYZopts,{
    MYZpar <- list()
    class(MYZpar) <- "basicM"

    MYZpar$nPatches <- nPatches
    if(nPatches == 1){
      sigma = 0
      calK = 1
    }

    MYZpar$p       <- checkIt(p, nPatches)
    MYZpar$sigma   <- checkIt(sigma, nPatches)
    MYZpar$mu      <- checkIt(mu, nPatches)
    MYZpar$f       <- checkIt(f, nPatches)
    MYZpar$q       <- checkIt(q, nPatches)
    MYZpar$nu      <- checkIt(nu, nPatches)
    MYZpar$eggsPerBatch <- eggsPerBatch

    # Store as baseline values
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
    class(MYZpar$q_par) <- q_mod
    MYZpar$sigma_par   <- list()
    class(MYZpar$sigma_par) <- sigma_mod
    MYZpar$mu_par   <- list()
    class(MYZpar$mu_par) <- mu_mod
    MYZpar$nu_par   <- list()
    class(MYZpar$nu_par) <- nu_mod

    MYZpar$calK <- calK

    Omega_par <- list()
    class(Omega_par) <- "static"
    MYZpar$Omega_par <- Omega_par
    MYZpar$Omega <- with(MYZpar, make_Omega_dts(p, sigma, mu, calK))

    return(MYZpar)
})}


#' @title Set bionomic parameters to baseline
#' @description Implements [MBionomics] for the basicM xde model
#' @inheritParams MBionomics
#' @return a named [list]
#' @export
MBionomics.basicM <- function(t, y, pars, s) {

  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f <- f0
    pars$MYZpar[[s]]$q <- q0
    pars$MYZpar[[s]]$g <- g0
    pars$MYZpar[[s]]$sigma <- sigma0
    pars$MYZpar[[s]]$mu <- mu0
    pars$MYZpar[[s]]$nu <- nu0
    pars$MYZpar[[s]]$Omega <- make_Omega(t, pars, s)

    return(pars)
})}

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
  y[pars$ix$MYZ[[s]]$M_ix]
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
setup_MYZinits.basicM = function(pars, s, MYZopts=list()){
  pars$MYZinits[[s]] = make_MYZinits_basicM(pars$nPatches, MYZopts)
  return(pars)
}

#' @title Make inits for basicM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param M0 total mosquito density at each patch
#' @param P0 total parous mosquito density at each patch
#' @return none
#' @export
make_MYZinits_basicM = function(nPatches, MYZopts = list(),
                                M0=5, P0=1){
  with(MYZopts,{
    M = checkIt(M0, nPatches)
    P = checkIt(P0, nPatches)
    return(list(M=M, P=P))
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
#' @description Implements [get_inits_MYZ] for the basicM model.
#' @inheritParams get_inits_MYZ
#' @return none
#' @export
get_inits_MYZ.basicM <- function(pars, s) {with(pars$MYZinits[[s]],{
  c(M, P)
})}

#' @title Make inits for RM adult mosquito model
#' @inheritParams update_inits_MYZ
#' @return none
#' @export
update_inits_MYZ.basicM <- function(pars, y0, s) {with(pars$ix$MYZ[[s]],{
  M = y0[M_ix]
  P = y0[P_ix]
  pars$MYZinits[[s]] = make_MYZinits_basicM(pars, list(), M0=M, P0=P)
  return(pars)
})}

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

#' @title Make parameters for a basic adult mosquito model
#' @param pars a [list]
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @param calK mosquito dispersal matrix of dimensions `nPatches` by `nPatches`
#' @return none
#' @export
make_parameters_MYZ_basicM <- function(pars, g, sigma, mu, f, q, nu, eggsPerBatch, calK) {
  stopifnot(is.numeric(g), is.numeric(sigma), is.numeric(f), is.numeric(q), is.numeric(nu), is.numeric(eggsPerBatch))

  MYZpar <- list()
  class(MYZpar) <- "basicM"

  MYZpar$g0 <- g
  MYZpar$sigma0 <- sigma
  MYZpar$mu0<- mu
  MYZpar$f0 <- f
  MYZpar$q0 <- q
  MYZpar$nu0 <- nu
  MYZpar$eggsPerBatch <- eggsPerBatch
  MYZpar$calK <- calK

  MYZpar$g <- g
  MYZpar$sigma <- sigma
  MYZpar$mu <- mu
  MYZpar$f <- f
  MYZpar$q <- q
  MYZpar$nu <- nu

  Omega_par <- list()
  class(Omega_par) <- "static"
  MYZpar$Omega_par <- Omega_par
  MYZpar$Omega <- with(MYZpar, make_Omega_xde(g, sigma, mu, calK))

  pars$MYZpar[[1]] <- MYZpar

  return(pars)
}

#' @title Make inits for basicM adult mosquito model
#' @param pars a [list]
#' @param M0 total mosquito density at each patch
#' @param P0 total parous mosquito density at each patch
#' @return none
#' @export
make_inits_MYZ_basicM <- function(pars, M0, P0) {
  pars$MYZinits[[1]] = list(M=M0, P=P0)
  return(pars)
}

