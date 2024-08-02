#' @title Make parameters for RM ODE adult mosquito model
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
create_MYZpar_RM_static = function(nPatches, MYZopts=list(), eip=12,
                                   g=1/12, sigma=1/8, mu=0, f=0.3, q=0.95,
                                   nu=1, eggsPerBatch=60){

  with(MYZopts,{
    MYZpar <- list()
    MYZpar$nPatches <- nPatches

    eip_par <- 'static'
    class(eip_par) <- 'static'
    MYZpar$eip_par <- eip_par

    MYZpar$eip    <- checkIt(eip, 1)
    MYZpar$g      <- checkIt(g, nPatches)
    MYZpar$sigma  <- checkIt(sigma, nPatches)
    MYZpar$mu     <- checkIt(mu, nPatches)
    MYZpar$f      <- checkIt(f, nPatches)
    MYZpar$q      <- checkIt(q, nPatches)
    MYZpar$nu     <- checkIt(nu, nPatches)
    MYZpar$eggsPerBatch <- eggsPerBatch
    MYZpar$calK <- diag(nPatches)

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsilon <- expm::expm(-Omega*eip)

    MYZpar$baseline <- 'RM_static'
    class(MYZpar$baseline) <- 'RM_static'
    MYZpar$effect_sizes <- 'unmodified'
    class(MYZpar$effect_sizes) <- 'unmodified'

    return(MYZpar)
})}

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
MBionomics.RM_static <- function(t, y, pars, s) {
  pars$MYZpar[[s]]$es_f     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_q     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_g     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_sigma <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_mu    <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_nu    <- rep(1, pars$nPatches)
  return(pars)
}

#' @title Make parameters for GeRM ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param MYZopts a [list] of values that overwrites the defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @export
create_MYZpar_RMfunc = function(nPatches, MYZopts=list(), eip =12,
                                 g=1/12,  sigma=1/8,  mu=0,
                                 f=0.3,  q=0.95,
                                 nu=1,  eggsPerBatch=60){
  with(MYZopts,{

    MYZpar <- list()

    MYZpar$nPatches <- nPatches

    MYZpar = list()
    class(MYZpar) <- "func"

    eip_par <- list()
    class(eip_par) <- 'static'
    eip_par$eip = eip
    MYZpar$eip_par <- eip_par
    MYZpar$eip=eip

    f=checkIt(f, nPatches)
    MYZpar$f_par <- list()
    class(MYZpar$f_par) <- "static"
    MYZpar$f_par$f = f
    MYZpar$f=f

    q=checkIt(q, nPatches)
    MYZpar$q_par <- list()
    class(MYZpar$q_par) <- "static"
    MYZpar$q_par$q = q
    MYZpar$q=q

    g=checkIt(g, nPatches)
    MYZpar$g_par <- list()
    class(MYZpar$g_par) <- "static"
    MYZpar$g_par$g = g
    MYZpar$g=g

    mu=checkIt(mu, nPatches)
    MYZpar$mu_par <- list()
    class(MYZpar$mu_par) <- "static"
    MYZpar$mu_par$mu = mu
    MYZpar$mu=mu

    sigma=checkIt(sigma, nPatches)
    MYZpar$sigma_par <- list()
    class(MYZpar$sigma_par) <- "static"
    MYZpar$sigma_par$sigma = sigma
    MYZpar$sigma=sigma

    nu=checkIt(nu, nPatches)
    MYZpar$nu_par <- list()
    class(MYZpar$nu_par) <- "static"
    MYZpar$nu_par$nu = nu
    MYZpar$nu=nu

    calK = diag(nPatches)
    MYZpar$calK_par <- list()
    class(MYZpar$calK_par) <- "static"
    MYZpar$calK_par$calK = calK
    MYZpar$calK=calK

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsilon <- expm::expm(-Omega*eip)
    MYZpar$nPatches <- nPatches

    Omega <- diag(g, nPatches)
    MYZpar$Omega <- Omega
    MYZpar$Upsilon <- expm::expm(-Omega*eip)
    MYZpar$nPatches <- nPatches

    MYZpar$eggsPerBatch <- eggsPerBatch

    MYZpar$baseline <- MYZpar
    class(MYZpar$baseline) <- 'RM_func'

    return(MYZpar)
})}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.RM_func <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f <- F_f(t, vars, f_par)
    pars$MYZpar[[s]]$q <- F_q(t, vars, f_par)
    pars$MYZpar[[s]]$g <- F_g(t, vars, f_par)
    pars$MYZpar[[s]]$sigma <- F_sigma(t, vars, f_par)
    pars$MYZpar[[s]]$mu <- F_mu(t, vars, f_par)
    pars$MYZpar[[s]]$nu <- F_nu(t, vars, f_par)
    pars$MYZpar[[s]]$eip <- F_eip(t, vars, f_par)
    pars$MYZpar[[s]]$calK <- F_calK(t, vars, f_par)
    pars$MYZpar[[s]]$eggsPerBatch <- eggsPerBatch
    Omega <- compute_Omega(pars, s)
    pars$MYZpar[[s]]$Omega <- Omega
    pars$MYZpar[[s]]$Upsilon <- expm::expm(-Omega*eip)
    pars <- make_MYZprobs(pars, s)
    return(pars)
  })}

#' @title Update Ross-Macdonald Bionomic Parameters
#' @description Update bionomic parameter values as the product of the baseline and
#' the effect sizes
#' @param MYZpar an `xds` adult mosquito  model object
#' @return the model as a [list]
#' @export
update_MYZpar_RM = function(MYZpar){
  UseMethod("update_MYZpar_RM", MYZpar$effect_sizes)
}

#' @title Update Ross-Macdonald Bionomic Parameters
#' @description If there is no modification, return
#' the parameters without modification
#' @inheritParams update_MYZpar_RM
#' @return the model as a [list]
#' @export
update_MYZpar_RM.unmodified = function(MYZpar){
  return(MYZpar)
}

#' @title Update Ross-Macdonald Bionomic Parameters
#' @description If there is no modification, return
#' the parameters without modification
#' @inheritParams update_MYZpar_RM
#' @return the model as a [list]
#' @export
update_MYZpar_RM.modified = function(MYZpar){
  MYZpar$f = with(MYZpar, f*es_f)
  MYZpar$q = with(MYZpar, q*es_q)
  MYZpar$g = with(MYZpar,g*es_g)
  MYZpar$sigma = with(MYZpar,sigma*es_sigma)
  MYZpar$mu = with(MYZpar,mu*es_mu)
  MYZpar$nu = with(MYZpar,nu*es_nu)
  MYZpar$Omega <-with(MYZpar, compute_Omega_xde(g,sigma,mu, calK))
  return(MYZpar)
}

#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @param MYZpar an adult mosquito `xds` model object
#' @param runtime the model `runtime` parameters
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs_RM = function(MYZpar, runtime){
  UseMethod("MYZ_rates2probs", MYZpar$effect_sizes)
}

#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @inheritParams MYZ_rates2probs
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs_RM.unmodified = function(MYZpar, runtime){
  return(MYZpar)
}

#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @inheritParams MYZ_rates2probs
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs_RM.modified = function(MYZpar, runtime){
  with(runtime,{
    with(MYZpar,{
      MYZpar$p <- exp(-g*MYZday*Dday)
      MYZpar$ff <- exp(-f*MYZday*Dday)
      MYZpar$ssigma <- exp(-sigma*MYZday*Dday)
      MYZpar$nnu <- exp(-nu*MYZday*Dday)
      return(MYZpar)
    })})}
