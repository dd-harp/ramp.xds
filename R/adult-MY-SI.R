# specialized methods for the adult mosquito SI model

#' @title The `SI` module for the **MY** component
#' 
#' @description
#' 
#' This implements a model for basic mosquito ecology and an `SI` 
#' compartmental model for mosquito infection dynamics. In its simplest form, the 
#' density of all mosquitoes \eqn{(M)} and infected mosquitoes \eqn{(Y)} are: 
#' \deqn{
#' \begin{array}{rl}
#' dM/dt &= \Lambda - g M \\ 
#' dY/dt &= fq\kappa(M-Y) - g Y
#' \end{array}
#' }
#' In this generalized implementation:
#' 
#' + the dynamics are spatial, a metapopulation model, so the states are replicated for the mosquito population in each patch
#' 
#' + the module includes parameters describing mosquito dispersal
#' 
#' + biting by infectious mosquitoes occurs as if the infected mosquitoes had survived and dispersed through the EIP \eqn{(\tau)} 
#' 
#' + parameters are included for egg laying
#' 
#' + the core bionomic parameters are set up to compute a *changing baseline* that can be *modified by vector control*
#' 
#' The [SI Vignette](https://dd-harp.github.io/ramp.xds/articles/adult_si.html) discusses the module. 
#'  
#' 
#' @section State Variables:
#' 
#' \describe{
#'   \item{`M`}{density of adult mosquitoes}
#'   \item{`Y`}{density of infected adult mosquitoes}
#' }
#' 
#' 
#' @section Dynamical Terms: 
#' 
#' \describe{
#'   \item{`Lambda` or \eqn{\Lambda}}{the number of adult mosquitoes emerging, per patch, per day, computed by [Emergence]}
#'   \item{`kappa` or \eqn{\kappa}}{net infectiousness: the probability a mosquito becomes infected after blood feeding on a human, is computed by [Transmission]}
#' }
#' 
#' @section Parameters:
#' \describe{
#'   \item{`f`}{blood feeding rate}
#'   \item{`q`}{human fraction}
#'   \item{`eip` or \eqn{\tau}}{extrinsic incubation period}
#'   \item{`g`}{mosquito mortality rate}
#'   \item{`sigma` or \eqn{\sigma}}{emigration rate}
#'   \item{`mu` or \eqn{\mu}}{emigration loss rate}
#'   \item{`K`}{mosquito dispersal matrix}
#'   \item{`nu` or \eqn{\nu}}{egg laying rate, the number of egg patches, per mosquito, per day}
#'   \item{`eggsPerBatch` or \eqn{\Gamma}}{the number of eggs per batch}
#' }
#' 
#' 
#' @section Ecology:
#' 
#' \deqn{
#' dM/dt = \Lambda - \Omega \cdot M 
#' }
#' 
#' **Emergence** 
#' The model for mosquito ecology uses the stored value `Lambda` computed in [Emergence]. 
#' 
#' **Survival and Dispersal** 
#' 
#' The parameter \eqn{g} is interpreted as the per-capita mortality rate for mosquitoes
#' while they are in a patch. Mosquitoes emigrate from patches at the per-capita rates \eqn{\sigma},  
#' and some fraction of emigrating mosquitoes is lost (\eqn{\mu}). The total rate of loss from a
#' a patch is \deqn{g + \mu \sigma.}
#'  
#' The columns of the mosquito dispersal matrix \eqn{(K)} sum
#' to zero. For non-trivial dispersal matrices:  
#' \deqn{\mbox{diag}\left(K\right)=-1} 
#' and the non-diagonal elements are fraction of surviving mosquitoes that arrive at another patch after
#' a flight bout. If there is any population loss associated with emigration, it goes in the term \eqn{\mu}. 
#' 
#' The demographic matrix, \eqn{\Omega} is computed as
#' \deqn{\Omega = \mbox{diag}\left(g + \sigma \mu \right) - K \cdot \mbox{diag}\left(\sigma \left(1-\mu\right)\right)}
#' 
#'  
#' 
#' @section Infection Dynamics: 
#' 
#' \deqn{
#' dY/dt = fq\kappa(M-Y) - \Omega \cdot Y \\
#' }
#' 
#' **Blood Feeding** 
#' 
#' Mosquitoes blood feed at the rate \eqn{f.} 
#' 
#' The human fraction, \eqn{q,} is the fraction of blood meals on human.
#' The model computes these derivatives: 
#' 
#' **Net Infectiousness** 
#' 

#' 
#' @section Infective Biting: 
#' 
#' \describe{
#'   \item{\eqn{f}}{blood feeding rate}
#'   \item{\eqn{q}}{human fraction}
#'   \item{\eqn{\Upsilon}}{survival and dispersal through the extrinsic incubation period}
#'   \item{\eqn{Z}}{density of infectious mosquitoes}
#'   \item{`fqZ`}{infective biting rate: the number of infectious bites, per patch, per day \deqn{fqZ = e^{-\Omega \tau} \cdot \left( f q Y \right) }}
#' } 
#' 
#' To compute the infective biting density, this model configures a parameter
#' for the EIP, \eqn{\tau.} 
#' Infective biting is given by 
#' \deqn{fqZ = \Upsilon \cdot \left( f q Y \right).}
#' 
#' \eqn{fqZ} is the dynamical term used by [Transmission] to compute the EIR. 
#'
#' @section Egg Laying:
#' 
#' \describe{
#'   \item{\eqn{\nu}}{egg laying rate, the number of egg patches, per mosquito, per day}
#'   \item{\eqn{\Gamma}}{the number of eggs laid, per batch}
#'   \item{`G`}{the number of eggs laid, per patch, per day \deqn{G = \nu M \Gamma }}
#' }
#' 
#' \eqn{G} is the dynamical term passed to [EggLaying] to distribute eggs among the habitats
#' that are found in each patch.
#'
#' @seealso
#' 
#' + Basic Setup: [make_MY_obj_SI], [setup_MY_obj.SI]
#' + Dynamics: [dMYdt.SI]
#' + Parameters: [get_MY_pars.SI], [change_MY_pars.SI]
#' + Variables: [make_MY_inits_SI], [get_MY_inits], [change_MY_inits.SI], [setup_MY_inits.SI]
#' + The [SI Vignette](https://dd-harp.github.io/ramp.xds/articles/adult_si.html)
#' 
#' @name SI
#' @rdname SI
NULL


#' @title The skill set
#'
#' @inheritParams setup_skillset_MY
#'
#' @return the **`xds`** object
#'
#' @keywords internal
#' @export
setup_skillset_MY.SI = function(xds_obj, s){
  skills = list(
    not_implemented = TRUE
  )
  xds_obj$MY_obj[[s]]$skill_set = skills
  return(xds_obj) 
}

#' Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_MY.SI = function(xds_obj, s){
  return(xds_obj)
}

#' @title Compute derivatives for `SI` (**MY**)
#' 
#' @description
#' 
#' The derivatives for the `SI` module are:
#' 
#' \deqn{
#' \begin{array}{rl}
#' dM/dt = & \Lambda - \Omega \cdot M \\
#' dY/dt = & f q \kappa (M-Y) - \Omega \cdot Y
#' \end{array}
#' }
#' 
#' @inheritSection SI Ecology
#' @inheritSection SI Infection Dynamics
#' 
#' @inheritParams dMYdt
#' @seealso [F_fqZ.SI] and [F_eggs.SI]
#' @return a vector with the derivatives
#' @keywords internal
#' @export
dMYdt.SI <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$terms$Lambda[[s]]
  kappa = xds_obj$terms$kappa[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      dM <- Lambda - (Omega %*% M)
      dY <- f*q*kappa*(M-Y) - (Omega %*% Y)
      
      return(c(dM, dY))
    })
  })
}


#' @title Update state variables for `SI` (**MY**)
#' @description Implements [Update_MYt] for the SI model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_MYt.SI <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$Lambda[[s]]
  kappa = xds_obj$kappa[[s]]
  D = xds_obj$MYday

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{
      Mt <- ccc*Lambda*D + Omega %*% M
      Yt <- Omega%*%((1-exp(-f*q*kappa*xds_obj$MYYday))*(M-Y)) + (Omega %*% Y)
      Yt <- Yt + (1-exp(-ccc))*(1-exp(-f*q*kappa*xds_obj$MYYday))*Lambda*ccc*D
      return(list(M=unname(Mt), Y=unname(Yt)))
    })
  })
}

#' @title Net blood feeding by infectious mosquitoes - `SI` mosquito model
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
#' @keywords internal
#' @export
F_fqZ.SI <- function(t, y, xds_obj, s) {
  Y = y[xds_obj$MY_obj[[s]]$ix$Y_ix]
  fqZ = with(xds_obj$MY_obj[[s]], f*q*(Upsilon %*%Y))
  return(fqZ)
}

#' @title Compute net blood feeding by mosquitoes for `SI`
#' @description  The variable \eqn{M} is the density of  mosquitoes.
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#' The daily HBR for the human / host population strata is \eqn{\beta \cdot fqM}
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM.SI <- function(t, y, xds_obj, s){
  M = y[xds_obj$MY_obj[[s]]$ix$M_ix]
  fqM = with(xds_obj$MY_obj[[s]], f*q*M)
  return(fqM)
}

#' @title Compute component egg laying rates for `SI`
#' @description The density of adult mosquitoes is \eqn{M}.
#' The **parameters** describing egg laying by adult mosquitoes are:
#' - \eqn{\nu} or `nu` is the egg laying rate
#' - \eqn{\Gamma} or `eggsPerBatch` is the number of eggs per batch
#'
#' The egg laying rate, per patch, is \deqn{\nu \Gamma M}
#'
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs.SI <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Mosquito bionomics for `SI` (**MY**)
#' @description Reset the effect sizes for static models.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MEffectSizes
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBionomics.SI <- function(t, y, xds_obj, s) {

  xds_obj$MY_obj[[s]]$f_t      <- F_f(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$q_t      <- F_q(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$g_t      <- F_g(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$sigma_t  <- F_sigma(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$mu       <- F_mu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$nu       <- F_nu(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$eip      <- F_eip(t, xds_obj, s)
  xds_obj$MY_obj[[s]]$K_matrix <- F_K_matrix(t, xds_obj, s)

  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  return(xds_obj)
}

#' @title Apply effect sizes for `SI` (**MY**)
#' @description Reset the effect sizes for static models.
#' When modules are added to compute effect sizes
#' from baseline parameters, those functions store
#' an effect size. The total effect size is the
#' product of the effect sizes for each intervention.
#' Since coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MEffectSizes
#' @return an **`xds`** object
#' @keywords internal
#' @export
MEffectSizes.SI <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    xds_obj$MY_obj[[s]]$f <- es_f*f_t
    xds_obj$MY_obj[[s]]$q <- es_q*q_t
    xds_obj$MY_obj[[s]]$g <- es_g*g_t
    xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
    xds_obj <- update_Omega(xds_obj, s)
    return(xds_obj)
  })}



#' @title Set up `SI` (**MY**)
#' @description 
#' 
#' This sets up an `SI` model object for **MY** module for the \eqn{s^{th}} vector species
#' + the object is created by [make_MY_obj_SI]
#' + the initial conditions are set by [setup_MY_inits]
#' + `options` is passed to both to override the defaults
#' 
#' @inheritSection SI State Variables
#' @inheritSection SI Parameters
#' 
#' @seealso [SI], [make_MY_obj_SI], [setup_MY_inits.SI]
#' 
#' @note The module object is stored at `xds_obj$MY_obj[[s]]`
#' 
#' @inheritParams setup_MY_obj
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_MY_obj.SI = function(MYname, xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]] <- make_MY_obj_SI(xds_obj$nPatches, options)
  xds_obj <- setup_MY_inits(xds_obj, options=options, s=s)
  xds_obj <- setup_F_circadian("setup", xds_obj, s=s)
  xds_obj <- setup_Omega_obj("xde", "static", xds_obj, s=s)
  xds_obj <- setup_Upsilon_obj(xds_obj, s=s, TRUE)
  xds_obj <- setup_K_matrix("zero", xds_obj, s=s)
  return(xds_obj)
}

#' @title Make parameters for SI ODE adult mosquito model
#' @inheritSection SI Parameters
#' 
#' @param nPatches is the number of patches, an integer
#' @param options a named list: named values override defaults
#' @param eip the extrinsic incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @seealso [SI], [setup_MY_obj.SI]
#' @keywords internal
#' @export
make_MY_obj_SI = function(nPatches, options=list(), eip=0,
                          g=1/12, sigma=1/8, mu=0, f=0.3, q=0.95,
                          nu=1, eggsPerBatch=60){

  with(options,{
    MY_obj <- list()
    MY_obj$nPatches <- nPatches

    eip_par <- 'static'
    class(eip_par) <- 'static'

    MY_obj <- setup_eip_obj(checkIt(eip, nPatches), MY_obj)
    MY_obj <- setup_f_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_q_obj(checkIt(q, nPatches), MY_obj)
    MY_obj <- setup_g_obj(checkIt(g, nPatches), MY_obj)
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj)
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj)
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj)
    MY_obj$eggsPerBatch <- eggsPerBatch
    
    class(MY_obj) = "SI" 

    return(MY_obj)
  })}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the `SI` model.
#' @inheritParams setup_MY_ix
#' @return a [list]
#' @keywords internal
#' @importFrom utils tail
#' @export
setup_MY_ix.SI <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(M_ix, 1)

  Y_ix <- seq(from = max_ix+1, length.out=nPatches)
  max_ix <- tail(Y_ix, 1)

  xds_obj$max_ix = max_ix
  xds_obj$MY_obj[[s]]$ix = list(M_ix=M_ix, Y_ix=Y_ix)
  return(xds_obj)
})}


#' @title List variables for `SI` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars.SI <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         Y = y[Y_ix]
       )))
}


#' @title Parse outputs for `SI` (**MY**)
#' @description Implements [parse_MY_orbits] for the `SI` model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits.SI <- function(outputs, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    M = outputs[,ix$M_ix]
    Y = outputs[,ix$Y_ix]
    Z = t(Upsilon %*% t(Y))
    y = Y/M
    z = Z/M
    return(list(M=M, Y=Y, Z=Z, y=y, z=z))
})}


#' @title Get parameters for `SI` (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritSection SI Parameters
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @seealso [SI], [change_MY_pars.SI]
#' @keywords internal
#' @export
get_MY_pars.SI <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}

#' @title Change parameters for `SI` (**MY**)
#' @description For the `SI` module (**MY**) for the \eqn{s^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#' @inheritSection SI Parameters
#' 
#' @inheritParams change_MY_pars
#' @seealso [SI], [get_MY_pars.SI]
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_pars.SI <- function(xds_obj, options=list(), s=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_obj$f = f
    xds_obj$MY_obj[[s]]$q_obj$q = q
    xds_obj$MY_obj[[s]]$g_obj$g = g
    xds_obj$MY_obj[[s]]$sigma_obj$sigma = sigma
    xds_obj$MY_obj[[s]]$eip_obj$eip = eip
    xds_obj$MY_obj[[s]]$mu_obj$mu = mu
    xds_obj$MY_obj[[s]]$nu_obj$nu = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    
    y <- get_inits(xds_obj, flatten=TRUE)
    xds_obj <- MBionomics(0, y, xds_obj, s)
    xds_obj <- MEffectSizes(0, y, xds_obj, s)
    
    return(xds_obj)
  }))}


#' @title Setup initial values for `SI` (**MY**)
#' @description Set initial values for all the variables in the `SI` module
#' (**MY**) for the \eqn{s^{th}} species.
#' + [make_MY_inits_SI] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or
#' see the inline module documentation.
#' @inheritSection SI State Variables
#' 
#' @inheritParams setup_MY_inits
#' @seealso [SI], [make_MY_inits_SI]
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_MY_inits.SI = function(xds_obj, options=list(), s=1){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_SI(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Make inits for `SI` adult mosquito model
#' @inheritSection SI State Variables
#' 
#' @param nPatches the number of patches in the model
#' @param options a [list] of values that overrides the defaults
#' @param M total mosquito density at each patch
#' @param Y infected mosquito density at each patch
#' @return a [list]
#' @seealso [SI], [setup_MY_inits.SI]
#' @keywords internal
#' @export
make_MY_inits_SI = function(nPatches, options = list(),
                            M=5, Y=1){
  with(options,{
    M = checkIt(M, nPatches)
    Y = checkIt(Y, nPatches)
    return(list(M=M,  Y=Y))
  })
}

#' @title Change initial values for `SI` (**MY**)
#' @description For the `SI` module (**MY**) for the \eqn{s^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#'
#' @inheritSection SI State Variables
#' 
#' @inheritParams change_MY_inits
#'
#' @seealso [SI], [get_MY_inits], [make_MY_inits_SI], [setup_MY_inits.SI]
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_inits.SI = function(xds_obj, options=list(), s=1){
  inits =  with(get_MY_inits(xds_obj, s), with(options,{
    list(M=M, Y=Y)}))
  xds_obj$MY_obj[[s]]$inits=inits
  return(xds_obj)
}

#' @title Get the feeding rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector
#' @keywords internal
#' @export
get_f.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the human fractions
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector
#' @keywords internal
#' @export
get_q.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the mortality rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector
#' @keywords internal
#' @export
get_g.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the patch emigration rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector
#' @keywords internal
#' @export
get_sigma.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Get the initial mosquito population density 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial mosquito population density 
#' @keywords internal
#' @export
get_M.SI = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$M
}

#' @title Get the initial infective mosquito density 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial infective mosquito population density 
#' @keywords internal
#' @export
get_Z.SI = function(xds_obj, s=1){
  get_MY_inits(xds_obj, s)$Z
}

#' @title Steady states: MY-SI
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritParams steady_state_MY
#' @return a [list]
#' @keywords internal
#' @export
steady_state_MY.SI_ode = function(Lambda, kappa, xds_obj, s=1){
  with(xds_obj$MY_obj[[s]],{
    Omega_inv <- solve(Omega)
    M_eq <- as.vector(Omega_inv %*% Lambda)
    Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
    return(list(M=M_eq, Y=Y_eq))
})}
