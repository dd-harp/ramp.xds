
#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_effectsizes
#' @export
setup_bednet_effectsizes.lemenach = function(name, pars, opts=list()){
  pars = setup_bednet_effectsizes_lemenach(pars, opts)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param tau0_frac a [numeric] vector giving the proportion of a feeding cycle spent
#' host seeking/bloodfeeding *vs.* resting/oviposition
#' @param rr probability of mosquito being repelled upon contact with ITN
#' @param ss probability of mosquito successfully feeding upon contact with ITN
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes_lemenach = function(pars, opts=list(),
                                             tau0_frac = c(0.68/3, 2.32/3),
                                             rr = 0.56, ss = 0.03){
  stopifnot(sum(tau0_frac) == 1)
  es <- list()
  es$name <- 'lemenach'
  class(es) <- 'lemenach'
  es$tau0_frac = tau0_frac
  es$rr=rr
  es$ss=ss
  pars$bednets$effectsizes <- es
  return(pars)
}

#' @title Modify baseline values due to vector control
#' @description Implements [BedNetEffectSizes] for the Le Menach ITN model of vector control
#' @inheritParams BedNetEffectSizes
#' @return a named [list]
#' @importFrom stats pexp
#' @seealso [compute_bednet_effect_sizes_lemenach()]
#' @export
BedNetEffectSizes.lemenach <- function(t, pars, s){
  phi = pars$vars$bednet_coverage
  with(pars$bednets$effectsizes,{
    with(pars$MYZpar[[s]],{
      es <- sapply(1:pars$nPatches,
                   compute_bednet_effect_sizes_lemenach,
                   phi=phi, ff=f_t, qq=q_t, gg=g_t, tau0_frac=tau0_frac, rr=rr, ss=ss)
      pars$MYZpar[[s]]$es_f <- pars$MYZpar[[s]]$es_f*es[1,]
      pars$MYZpar[[s]]$es_q <- pars$MYZpar[[s]]$es_q*es[2,]
      pars$MYZpar[[s]]$es_g <- pars$MYZpar[[s]]$es_g*es[3,]
      return(pars)
    })
})}


#' @title Modify baseline values due to vector control
#' @description Implements [BedNetEffectSizes] for the Le Menach ITN model of vector control
# https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-6-10
#' @param ix an index over nPatches
#' @param phi ITN coverage
#' @param ff baseline blood feeding rate
#' @param qq baseline human fraction
#' @param gg baseline mosquito mortality rate
#' @param tau0_frac a [numeric] vector giving the proportion of a feeding cycle spent
#' host seeking/bloodfeeding *vs.* resting/oviposition
#' @param rr probability of mosquito being repelled upon contact with ITN
#' @param ss probability of mosquito successfully feeding upon contact with ITN
#' @return an **`xds`** model object
#' @references{This implements the model for ITN effect sizes
#' from \insertRef{LeMenachA2007_ITN}{ramp.xds}}
#' @importFrom stats pexp
compute_bednet_effect_sizes_lemenach = function(ix, phi, ff, qq, gg, tau0_frac=c(0.68/3, 2.32/3), rr=0.56, ss=0.03){
  f=ff[ix]; q=qq[ix]; g=gg[ix]

  tau0 <- (1/f) * tau0_frac

  p0 <- pexp(q = g*tau0, lower.tail = FALSE)
  Q0 <- q
  W <- (1-Q0) + Q0*(1-phi) + Q0*phi*ss
  Z <- Q0*phi*rr

  tau_phi <- tau0
  tau_phi[1] <- tau0[1]/(1-Z)

  f_phi <- 1 / sum(tau_phi) # feeding rate under control

  p_phi <- p0
  p_phi[1] <- (p0[1] * W) / (1 - Z*p0[1])

  g_phi <- -f_phi*log(prod(p_phi)) # mortality under control
  q_phi <- (Q0*(1-phi) + Q0*phi*ss)/W # human feeding fraction under control
  return(c(es_f = f_phi/f, es_q = q_phi/q, es_g = g_phi/g))
}

