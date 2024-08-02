# specialized methods for the Le Menach model of ITN based vector control


#' @title Modify baseline values due to vector control
#' @description Implements [BedNetEffectSizes] for the Le Menach ITN model of vector control
#' @inheritParams BedNetEffectSizes
#' @return a named [list]
#' @importFrom stats pexp
#' @export
BedNetEffects.lemenach <- function(t, pars, s){
  pars$ITNefsz$phi <- pars$ITNeff$F_phi(t, pars)
  return(pars)
}

# https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-6-10
#' @title Modify baseline values due to vector control
#' @description Implements [BedNetEffectSizes] for the Le Menach ITN model of vector control
#' @inheritParams BedNetEffectSizes
#' @return a named [list]
#' @importFrom stats pexp
#' @export
BedNetEffectSizes.lemenach <- function(t, pars, s){
  with(pars$ITNefsz,
    with(pars$MYZpar[[s]],{
      es <- sapply(1:pars$nPatches, compute_bednet_effect_sizes_lemenach, phi=phi, f=f,q=q, g=g, tau0_frac=tau0_frac, r=r,ss=ss)
      pars$MYZpar[[s]]$es_f <- pars$MYZpar[[s]]$es_f*es[1,]
      pars$MYZpar[[s]]$es_q <- pars$MYZpar[[s]]$es_q*es[2,]
      pars$MYZpar[[s]]$es_g <- pars$MYZpar[[s]]$es_g*es[3,]
      return(pars)
}))}


# https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-6-10
#' @title Modify baseline values due to vector control
#' @description Implements [BedNetEffectSizes] for the Le Menach ITN model of vector control
#' @param ix an index over nPatches
#' @param phi ITN coverage
#' @param ff baseline blood feeding rate
#' @param qq baseline human fraction
#' @param gg baseline mosquito mortality rate
#' @param tau0_frac a [numeric] vector giving the proportion of a feeding cycle spent
#' host seeking/bloodfeeding *vs.* resting/oviposition
#' @param r probability of mosquito being repelled upon contact with ITN
#' @param ss probability of mosquito successfully feeding upon contact with ITN
#' @return a named [list]
#' @references{This implements the model for ITN effect sizes
#' from \insertRef{LeMenachA2007_ITN}{ramp.xds}}
#' @importFrom stats pexp
compute_bednet_effect_sizes_lemenach = function(ix, phi, ff, qq, gg, tau0_frac, r, ss){
  f=ff[ix]; q=qq[ix]; g=gg[ix]

  tau0 <- (1/f) * tau0_frac

  p0 <- pexp(q = g*tau0, lower.tail = FALSE)
  Q0 <- q
  W <- (1-Q0) + Q0*(1-phi) + Q0*phi*ss
  Z <- Q0*phi*r

  tau_phi <- tau0
  tau_phi[1] <- tau0[1]/(1-Z)

  f_phi <- 1 / sum(tau_phi) # feeding rate under control

  p_phi <- p0
  p_phi[1] <- (p0[1] * W) / (1 - Z*p0[1])

  g_phi <- -f_phi*log(prod(p_phi)) # mortality under control
  q_phi <- (Q0*(1-phi) + Q0*phi*ss)/W # human feeding fraction under control
  return(c(es_f = f_phi/f, es_q = q_phi/q, es_g = g_phi/g))
}


#' @title Make parameters for Le Menach ITN model of vector control
#' @description This model of ITN based vector control was originally described in \url{https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-6-10}.
#' @param pars a [list]
#' @param tau0_frac a [numeric] vector giving the proportion of time spent
#' in host seeking/bloodfeeding and resting/oviposition
#' @param r probability of mosquito being repelled upon contact with ITN
#' @param s probability of mosquito successfully feeding upon contact with ITN
#' @param F_phi a [function] that takes as argument `t` and `pars` and returns the level of ITN coverage at that time
#' @return none
#' @export
setup_itn_lemenach <- function(pars, tau0_frac = c(0.68/3, 2.32/3), r = 0.56, s = 0.03, F_phi = function(t, pars){.8} ) {
  stopifnot(sum(tau0_frac) == 1)
  stopifnot(F_phi(0, pars) >= 0)
  stopifnot(F_phi(0, pars) <= 1)

  efsz <- list()
  class(efsz) <- 'lemenach'
  efsz$tau0_frac <- tau0_frac
  efsz$r <- r
  efsz$ss <- s
  pars$ITNefsz <- efsz

  coverage = list()
  class(coverage) <- "lemenach"
  coverage$F_phi = F_phi
  pars$ITNeff = coverage

  return(pars)
}


