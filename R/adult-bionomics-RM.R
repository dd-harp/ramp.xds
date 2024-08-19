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
create_MYZpar_RM = function(nPatches, MYZopts=list(), eip=12,
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

    MYZpar$baseline <- 'RM'
    class(MYZpar$baseline) <- 'RM'
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
MBionomics.RM <- function(t, y, pars, s) {
  pars$MYZpar[[s]]$es_f     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_q     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_g     <- rep(1, pars$nPatches)
  pars$MYZpar[[s]]$es_sigma <- rep(1, pars$nPatches)
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
create_MYZpar_GeRM = function(nPatches, MYZopts=list(), eip =12,
                                 g=1/12,  sigma=1/8,  mu=0,
                                 f=0.3,  q=0.95,
                                 nu=1,  eggsPerBatch=60){
  with(MYZopts,{

    MYZpar <- list()

    MYZpar$nPatches <- nPatches

    eip_par <- list()
    class(eip_par) <- 'static'
    eip_par$eip = eip
    MYZpar$eip_par <- eip_par
    MYZpar$eip=eip

    f=checkIt(f, nPatches)
    MYZpar$f_par <- list()
    class(MYZpar$f_par) <- "static"
    MYZpar$f_par$f = f
    MYZpar$f_t = f
    MYZpar$es_f = 1

    q=checkIt(q, nPatches)
    MYZpar$q_par <- list()
    class(MYZpar$q_par) <- "static"
    MYZpar$q_par$q = q
    MYZpar$q_t = q
    MYZpar$es_q = 1

    g=checkIt(g, nPatches)
    MYZpar$g_par <- list()
    class(MYZpar$g_par) <- "static"
    MYZpar$g_par$g = g
    MYZpar$g_t = g
    MYZpar$es_g = 1

    mu=checkIt(mu, nPatches)
    MYZpar$mu_par <- list()
    class(MYZpar$mu_par) <- "static"
    MYZpar$mu_par$mu = mu
    MYZpar$mu = mu

    sigma=checkIt(sigma, nPatches)
    MYZpar$sigma_par <- list()
    class(MYZpar$sigma_par) <- "static"
    MYZpar$sigma_par$sigma = sigma
    MYZpar$sigma_t = sigma
    MYZpar$es_sigma = 1

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
    class(MYZpar$baseline) <- 'GeRM'

    return(MYZpar)
})}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.GeRM <- function(t, y, pars, s) {
  with(pars$MYZpar[[s]],{
    pars$MYZpar[[s]]$f_t      <- F_f(t, vars, f_par)
    pars$MYZpar[[s]]$es_f     <- rep(1, pars$nPatches)
    pars$MYZpar[[s]]$q_t      <- F_q(t, vars, f_par)
    pars$MYZpar[[s]]$es_q     <- rep(1, pars$nPatches)
    pars$MYZpar[[s]]$g_t      <- F_g(t, vars, f_par)
    pars$MYZpar[[s]]$es_g     <- rep(1, pars$nPatches)
    pars$MYZpar[[s]]$sigma_t  <- F_sigma(t, vars, f_par)
    pars$MYZpar[[s]]$es_sigma <- rep(1, pars$nPatches)
    pars$MYZpar[[s]]$mu       <- F_mu(t, vars, f_par)
    pars$MYZpar[[s]]$nu       <- F_nu(t, vars, f_par)
    pars$MYZpar[[s]]$eip      <- F_eip(t, vars, f_par)
    pars$MYZpar[[s]]$calK     <- F_calK(t, vars, f_par)
    pars$MYZpar[[s]]$eggsPerBatch <- eggsPerBatch
    pars <- make_MYZprobs(pars, s)
    return(pars)
  })}

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


#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], f_t*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], q_t*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], g_t*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.RM = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma_t*es_sigma)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], f_t*es_f)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], q_t*es_q)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], g_t*es_g)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.GeRM = function(pars, s=1){
  with(pars$MYZpar[[s]], sigma_t*es_sigma)
}
