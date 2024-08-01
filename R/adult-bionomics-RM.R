
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

    eip_par <- list()
    class(eip_par) <- 'static'
    MYZpar$eip_par <- eip_par

    base = list()
    base$eip    <- checkIt(eip, 1)
    base$g      <- checkIt(g, nPatches)
    base$sigma  <- checkIt(sigma, nPatches)
    base$mu     <- checkIt(mu, nPatches)
    base$f      <- checkIt(f, nPatches)
    base$q      <- checkIt(q, nPatches)
    base$nu     <- checkIt(nu, nPatches)
    base$eggsPerBatch <- eggsPerBatch
    base$calK <- diag(nPatches)

    Omega <- diag(g, nPatches)
    base$Omega <- Omega
    base$Upsilon <- expm::expm(-Omega*eip)
    base$nPatches <- nPatches
    class(base) <- 'static'

    MYZpar$baseline = base
    return(MYZpar)
  })}



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

    base = list()
    class(base) <- "func"

    eip_par <- list()
    class(eip_par) <- 'static'
    eip_par$eip = eip
    MYZpar$eip_par <- eip_par
    base$eip=eip

    f=checkIt(f, nPatches)
    MYZpar$f_par <- list()
    class(MYZpar$f_par) <- "static"
    MYZpar$f_par$f = f
    base$f=f

    q=checkIt(q, nPatches)
    MYZpar$q_par <- list()
    class(MYZpar$q_par) <- "static"
    MYZpar$q_par$q = q
    base$q=q

    g=checkIt(g, nPatches)
    MYZpar$g_par <- list()
    class(MYZpar$g_par) <- "static"
    MYZpar$g_par$g = g
    base$g=g

    mu=checkIt(mu, nPatches)
    MYZpar$mu_par <- list()
    class(MYZpar$mu_par) <- "static"
    MYZpar$mu_par$mu = mu
    base$mu=mu

    sigma=checkIt(sigma, nPatches)
    MYZpar$sigma_par <- list()
    class(MYZpar$sigma_par) <- "static"
    MYZpar$sigma_par$sigma = sigma
    base$sigma=sigma

    nu=checkIt(nu, nPatches)
    MYZpar$nu_par <- list()
    class(MYZpar$nu_par) <- "static"
    MYZpar$nu_par$nu = nu
    base$nu=nu



    calK = diag(nPatches)
    MYZpar$calK_par <- list()
    class(MYZpar$calK_par) <- "static"
    MYZpar$calK_par$calK = calK
    base$calK=calK

    Omega <- diag(g, nPatches)
    base$Omega <- Omega
    base$Upsilon <- expm::expm(-Omega*eip)
    base$nPatches <- nPatches

    Omega <- diag(g, nPatches)
    base$Omega <- Omega
    base$Upsilon <- expm::expm(-Omega*eip)
    base$nPatches <- nPatches

    class(base) <- 'func'

    MYZpar$baseline <- base
    MYZpar$now <- now

    return(MYZpar)
})}


