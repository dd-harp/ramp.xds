# X

# specialized methods for the human SIS module

#' @title The `SIS` module for the **XH** component
#' 
#' @description
#' 
#' `SIS` implements two generalized S-I-S compartmental 
#' models for malaria:
#' 1. a system of differential equations
#' 2. a discrete time system  
#' 
#' Enhancements to the basic S-I-S model include: 
#' + functions to model human demographic change; 
#' + parameters to model treating and curing infections; 
#' + functions to model mass treatment;
#' + support for progressive stratification
#' 
#' For a longer discussion, see the [SIS Vignette](https://dd-harp.github.io/ramp.xds/articles/human_sis.html). 
#'
#' @section State Variables:
#' 
#' \describe{
#'   \item{`H`}{the total human or host population density}
#'   \item{`I`}{the density of infected humans or hosts}
#'   \item{`S=H-I`}{the density of susceptible humans or hosts}
#' }
#' 
#' 
#' @section Dynamical Term:
#' 
#' \describe{
#'   \item{\eqn{h} or `foi`}{The force of infection is computed and stored by [Exposure] as `FoI`}
#' }
#' 
#' In the `SIS` module, there is no effect of pre-erythrocytic immunity, so 
#' a constant fraction of infectious bites causes an infection: the parameter is called \eqn{b.}
#' 
#' 
#' @section Parameters:
#' 
#' The parameters that are returned by [get_XH_pars.SIS] and also modifiable by [change_XH_pars.SIS] describe 
#' the dynamics, or detection by diagnostic methods. 
#' 
#' \describe{
#'   \item{`b`}{the fraction of infective bites that would cause an infection}
#'   \item{`r`}{clearance rate for infections}
#'   \item{`c`}{the probability an infected person would infect a mosquito}
#'   \item{`rho` or \eqn{\rho}}{the fraction of incident infections that are treated and cured}
#'   \item{`sigma` or \eqn{\sigma}}{the rate that prevalent infections are treated and cured}
#'   \item{`q_lm`}{test positivity by light microscopy}
#'   \item{`q_rdt`}{test positivity by RDT}
#'   \item{`q_pcr`}{test positivity by PCR}
#' } 
#' 
#' @section Infection Dynamics:
#' 
#' The model for infection dynamics (**X**) assumes that infections
#' clear naturally at a constant per-capita rate \eqn{r}. 
#' 
#' The model also includes parameters and functions to model
#' treating and curing malaria infections: 
#' + a fraction \eqn{\rho} of incident infections are treated and cured
#' + since incident infections are treated, the FoI is \eqn{h (1-\rho)}
#' + in addition, prevalent infections are treated and cured at the rate \eqn{\sigma}
#' + functions describe an increase in clearance caused by mass treatment, \eqn{\Xi(t)}, a *shock* function
#' 
#' The total per-capita clearance rate from treatment is thus \eqn{\rho h + \sigma + \Xi(t)}  
#' 
#' 
#' After clearing, individuals are 
#' susceptible to infection.
#' 
#' @section Demography:  
#' 
#' This model has no parameters
#' or terms to describe disease-induced mortality.
#' 
#' \describe{
#'   \item{[`Births`] or \eqn{\mathbf{B}}}{the births function}
#'   \item{[`Deaths`] or \eqn{\mathbf{D}}}{the deaths function}
#' } 
#' 
#' **Setup:** 
#' 
#' The default is a constant population model: birth 
#' and death rates are set to zero
#' 
#' + Use [setup_births] to configure [`Births`]
#' 
#' + Use [setup_deaths] to configure [`Deaths`]
#' 
#' Also, see [xds_info_demographic_change].
#' 
#' @section ODE:   
#' 
#' **Demography** ---
#' Demographic changes (**H**) are given by the equation:
#' 
#' \deqn{
#' dH/dt = \mathbf{B}(t,H) - \mathbf{D}(H)}
#' 
#' **Infection Dynamics** --- 
#' Infection dynamics (**X**) are described by:
#' 
#' \deqn{
#' dI/dt = h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I  - \mathbf{D}(I) }
#'
#' After solving, the orbit of the variable \eqn{S=H-I} is computed and attached.
#' 
#' 
#' @section DTS:
#' 
#' **Demography** ---
#' Demographic changes (**H**) are given by the equation:
#' 
#' \deqn{
#' H_t = H_{t-1}  + \mathbf{B}(t,H_{t-1}) - \mathbf{D}(H_{t-1})}
#' 
#' **Infection Dynamics** --- 
#' \eqn{I_t} is updated in two steps: survival first,
#' and disease dynamics second: 
#' Let \eqn{\alpha = 1-e^{-h}}.
#' \deqn{\begin{array}{rl}
#' I_s &= I_{t-1} - \mathbf{D}(I_{t-1}) \\
#' I_t &= \alpha (1-\rho)(H_t-I_s) + e^{-(r + \rho h + \sigma + \Xi(t))} I_s
#' \end{array}}
#' 
#' After solving, the orbit of the variable \eqn{S=H-I} is computed and attached.
#' 
#' @section Prevalence:
#' 
#' \describe{
#'   \item{\eqn{x}}{True prevalence is \eqn{x = I/H}}
#'   \item{\eqn{x_{lm}}}{Expected prevalence by light microscopy is \eqn{q_{lm}\; x}}
#'   \item{\eqn{x_{rdt}}}{Expected prevalence by RDT is \eqn{q_{rdt}\; x}}
#'   \item{\eqn{x_{pcr}}}{Expected prevalence by PCR is \eqn{q_{pcr}\; x}}
#' }
#' 
#' @section Infectiousness:
#' 
#' The fraction of mosquitoes that become infected after blood feeding on 
#' someone who is infectious is \eqn{c} so the fraction of mosquitoes that 
#' would get infected is \eqn{cx.} 
#'  
#' The infectious density of the human (host) population is \eqn{cI = cxH.}
#' 
#' 
#' @section Mass Treatment: 
#'
#' Mass treatment clears infections. At basic setup
#' \eqn{\Xi(t)=0}. To set up mass treatment, see [setup_mass_treatment]. 
#' Mass treatment is either: 
#' 
#' \describe{
#'   \item{`mda(t)` or \eqn{\Xi(t)}}{mass drug administration}
#'   \item{`msat(t)` or \eqn{\Xi(t)}}{mass screen and treat}
#' }
#' 
#' @seealso 
#' 
#' + Basic Setup:  [make_XH_obj_SIS], [setup_XH_obj.SIS]
#' + Dynamics: [dXHdt.SIS], [Update_XHt.SIS]
#' + Parameters: [get_XH_pars.SIS], [change_XH_pars.SIS]
#' + Variables: [make_XH_inits_SIS], [get_XH_inits], [change_XH_inits.SIS],  [setup_XH_inits.SIS]
#' + [The SIS Vignette](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#' 
#' @name SIS
#' @rdname SIS
NULL


#' @title Compute derivatives for `SIS` (**XH**)
#'
#' @description
#' 
#' The derivatives for the `SIS` module are:
#'  
#' \deqn{\begin{array}{rl}
#' dH/dt &= \mathbf{B}(t,H) - \mathbf{D}(H) \\
#' dI/dt &= h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I  - \mathbf{D}(I) 
#' \end{array}}
#'
#' 
#' @inheritSection SIS Infection Dynamics
#' @inheritSection SIS Demography
#' 
#' 
#' @inheritParams dXHdt
#'
#' @seealso [SIS], [SIS Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#' @return the derivatives, as a vector
#' @keywords internal
#' @export
dXHdt.SIS <- function(t, y, xds_obj, i) {

  foi <- xds_obj$terms$FoI[[i]]

  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]], {
      
      clear = r + sigma + rho*foi + mda(t) + msat(t)
      dH <- Births(t, y, xds_obj, i) - Deaths(t, H, xds_obj, i)
      dI <- foi*(1-rho)*(H-I) - clear*I - Deaths(t, I, xds_obj, i)
      return(c(dH, dI))
    })
  })
}

#' @title Update states for `SIS` (**XH**)
#'
#' @description
#' 
#' For the `SIS` discrete time system, the state variables are derived
#' from the differential equations: 
#'  
#' \deqn{\begin{array}{rl}
#' H_t &= H_{t-1} + \mathbf{B}(t,H) - \mathbf{D}(H) \\
#' I_s &= I_{t-1} - \mathbf{D}(I_{t-1}) \\
#' I_t &= \alpha (1-\rho)(H_t-I_s) + e^{-(r + \rho h + \sigma + \Xi(t))} I_s
#' \end{array}}
#'
#'
#' @inheritSection SIS Infection Dynamics
#' @inheritSection SIS Demography
#' 
#' @inheritParams dXHdt
#'
#' @seealso [SIS], [SIS Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#' @return updated dependent variables (a [vector])
#' @keywords internal
#' @export
Update_XHt.SIS <- function(t, y, xds_obj, i) {
  
  foi <- xds_obj$terms$FoI[[i]]
  ar = 1-exp(-foi)
  
  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]], {
      Ht <- H + Births(t, y, xds_obj, i) - Deaths(t, H, xds_obj, i)
      It <- I- Deaths(t, I, xds_obj, i)
      It <- ar*(1-rho)*(Ht-It) + exp(-(r + sigma + rho*foi + mda(t) + msat(t)))*It
      return(c(Ht, It))
    })
  })
}

#' @title Set up `SIS` (**XH**)
#'
#' @description
#'
#' This sets up a `SIS` model object for the **XH** module for the \eqn{i^{th}} host species
#' + the object is created by [make_XH_obj_SIS]
#' + the initial conditions are set by [setup_XH_inits]
#' + `options` is passed to both to override the defaults
#'
#' @inheritSection SIS State Variables
#' @inheritSection SIS Parameters
#' 
#' @seealso [SIS], [make_XH_obj_SIS]
#'
#' @note The module object is stored at `xds_obj$XH_obj[[i]]`
#'
#' @inheritParams setup_XH_obj
#'
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
setup_XH_obj.SIS = function(XHname, residence, xds_obj, options=list(), i=1){
  xds_obj$XHname = "SIS"
  xds_obj$XH_obj[[i]] = make_XH_obj_SIS(xds_obj$nStrata[i], options)
  xds_obj <- setup_XH_inits(xds_obj, options=options, i=i)
  xds_obj$XH_obj[[i]]$skill_set <- get_skillset_XH(xds_obj, i)
  xds_obj <- setup_blood_search_weights("setup", xds_obj, i=i)
  xds_obj <- setup_timespent("setup", xds_obj, list(residence=residence), i)
  xds_obj <- setup_mass_treatment(xds_obj, i=i)
  xds_obj <- setup_demog_obj(xds_obj, i)
  xds_obj <- setup_time_away("setup", xds_obj, i=i)
  xds_obj <- setup_travel_eir("no_travel", xds_obj, i=i)
  return(xds_obj)
}

#' @title Make a `SIS` **XH** model object
#'  
#' @inheritSection SIS Parameters 
#' 
#' @param nStrata is the number of population strata
#' @param options a named list with parameter values to override defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c transmission probability (efficiency) from human to mosquito
#' @param r recovery rate
#' @param rho the fraction of incident infections that are treated and cured
#' @param sigma the rate prevalent infections are treated and cured
#' @param q_lm test positivity by light microscopy
#' @param q_rdt test positivity by RDT
#' @param q_pcr test positivity by PCR
#'
#' @return an **XH** model object
#' @keywords internal
#'
#' @seealso [SIS]
#' @export
make_XH_obj_SIS = function(nStrata, options=list(),
                             b=0.55, r=1/180, c=0.15,
                             rho=0, sigma=0, 
                             q_lm =0.8, q_rdt=0.8, q_pcr=0.9){
  with(options,{
    XH_obj = list()
    class(XH_obj) <- "SIS"

    XH_obj$b = checkIt(b, nStrata)
    XH_obj$c = checkIt(c, nStrata)
    XH_obj$r = checkIt(r, nStrata)
    XH_obj$sigma = checkIt(sigma, nStrata)
    XH_obj$rho = checkIt(rho, nStrata)
    XH_obj$q_lm = checkIt(q_lm, nStrata)
    XH_obj$q_rdt = checkIt(q_rdt, nStrata)
    XH_obj$q_pcr = checkIt(q_pcr, nStrata)

    return(XH_obj)
  })}


#' @title Change parameters for `SIS` (**XH**)
#'
#' @description For the `SIS` module (**XH**) for the \eqn{i^{th}} host species,
#' change parameter values named in `options` leaving other values unchanged.
#' 
#' 
#' @inheritSection SIS Parameters
#' 
#' @inheritParams change_XH_pars
#'
#' @seealso [SIS], [get_XH_pars.SIS()] and the vignette on [SIS Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#'
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
change_XH_pars.SIS <- function(xds_obj, options=list(), i=1) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$XH_obj[[i]]$b <- b
    xds_obj$XH_obj[[i]]$c <- c
    xds_obj$XH_obj[[i]]$r <- r
    xds_obj$XH_obj[[i]]$sigma <- sigma
    xds_obj$XH_obj[[i]]$rho <- rho
    xds_obj$XH_obj[[i]]$q_lm <- q_lm
    xds_obj$XH_obj[[i]]$q_rdt <- q_rdt
    xds_obj$XH_obj[[i]]$q_pcr <- q_pcr
    return(xds_obj)
  }))}

#' @title Get parameters for `SIS` (**XH**)
#'
#' @description
#' Returns the stored parameter
#' values as a list.
#'
#' @note
#' Parameter values for the \eqn{i^{th}} host are
#' stored as `xds_obj$XH_obj[[i]]`.
#'
#' @inheritParams get_XH_pars
#' 
#' @inheritSection SIS Parameters 
#'
#' @return `SIS` model parameters
#' @keywords internal
#' @seealso [SIS]
#' @export
get_XH_pars.SIS <- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]],list(b=b, c=c, r=r,
                                rho=rho, sigma=sigma,
                                q_lm=q_lm, q_rdt=q_rdt, q_pcr=q_pcr))
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the `SIS` model.
#' @inheritParams setup_XH_ix
#' @return an **`xds`** model object
#' @importFrom utils tail
#' @keywords internal
#' @seealso [SIS]
#' @export
setup_XH_ix.SIS <- function(xds_obj, i) {with(xds_obj,{

  H_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(H_ix, 1)

  I_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(I_ix, 1)

  xds_obj$XH_obj[[i]]$ix = list(H_ix=H_ix, I_ix=I_ix)
  xds_obj$max_ix = max_ix

  return(xds_obj)
})}

#' @title List variables for `SIS` (**XH**)
#'
#' @description Get the value of variables
#' from the flat state variable
#' vector \eqn{y}, and return
#' the values as a named list
#'
#' @inheritParams get_XH_vars
#' @return a [list]
#' @keywords internal
#' @seealso [SIS]
#' @export
get_XH_vars.SIS <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]]$ix,{
    H = y[H_ix]
    I = y[I_ix]
    S = H-I
    return(list(S=S,I=I,H=H))})
}


#' @title Parse outputs for `SIS` (**XH**)
#' @description Implements [parse_XH_orbits] for the `SIS` model
#' @inheritParams parse_XH_orbits
#' @return a [list]
#' @keywords internal
#' @seealso [SIS]
#' @export
parse_XH_orbits.SIS <- function(outputs, xds_obj, i) {
  XH_obj = xds_obj$XH_obj[[i]]
  with(XH_obj$ix,{
    H <- outputs[,H_ix]
    I <- outputs[,I_ix]
    S <- H-I
    vars <- list(S=S, I=I, H=H)
    return(vars)
})}


#' @title Set up initial values for `SIS` (**XH**)
#'
#' @description Set initial values for all the variables in the `SIS` module
#' (**XH**) for the \eqn{i^{th}} host species.
#' + [make_XH_inits_SIS] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_XH_inits] or
#' see the inline module documentation.
#' 
#' @inheritSection SIS State Variables
#'  
#' @inheritParams setup_XH_inits
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @seealso [SIS]
#' @export
setup_XH_inits.SIS = function(xds_obj, options=list(), i=1){
  xds_obj$XH_obj[[i]]$inits = make_XH_inits_SIS(xds_obj$nStrata[i], options)
  return(xds_obj)
}

#' @title Make initial values `SIS` (**XH**)
#'
#' @description Make a named list of initial values
#'
#' @param nStrata the number of strata in the model
#' @param options a [list] to override defaults
#' @param H the initial human population density
#' @param I the initial number of humans infected
#' 
#' @seealso [SIS], [setup_XH_inits.SIS]
#' 
#' @keywords internal
#'
#' @return initial values as a named [list]
#' @export
make_XH_inits_SIS = function(nStrata, options=list(), H=1000, I=1){
  with(options,{
    H = unname(as.vector(checkIt(H, nStrata)))
    I = unname(as.vector(checkIt(I, nStrata)))
    return(list(H=H, I=I))
})}

#' @title Change initial values for `SIS` (**XH**)
#'
#' @description For the `SIS` module (**XH**) for the \eqn{i^{th}} host species,
#' change initial values named in `options` leaving other values unchanged.
#' 
#' @inheritSection SIS State Variables
#' 
#' @details
#' The variable \eqn{S} is redundant. It is computed as \eqn{S=H-I.}
#' Its initial value is not stored, so it can't be set or changed.
#' 
#' @inheritParams change_XH_inits
#'
#' @seealso [SIS]. The variable names are returned by calling [get_XH_inits].
#' The function [make_XH_inits_SIS] is called during setup. 
#'
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
change_XH_inits.SIS <- function(xds_obj, options=list(), i=1) {
  with(get_XH_inits(xds_obj, i), with(options,{
    xds_obj$XH_obj[[i]]$inits$H = H
    xds_obj$XH_obj[[i]]$inits$I = I
    return(xds_obj)
  }))}

#' @title Compute infectious density for `SIS`
#'
#' @description In the `SIS` model family, infectious
#' density is \eqn{cI}.
#'
#' @inheritParams F_I
#' @keywords internal
#' @return Infectious density
#' @seealso [SIS]
#' @export
F_I.SIS <- function(t, y, xds_obj, i) {
  I = y[xds_obj$XH_obj[[i]]$ix$I_ix]
  X = with(xds_obj$XH_obj[[i]], c*I)
  return(X)
}

#' @title Compute population density
#'
#' @description Implements [F_H] for the `SIS` module
#'
#' @inheritParams F_H
#' @keywords internal
#'
#' @return Human (host) population density, \eqn{H}
#'
#' @seealso [SIS]
#' @export
F_H.SIS <- function(t, y, xds_obj, i){
  with(get_XH_vars(y, xds_obj, i), return(H))
}

#' @title Compute infectivity
#'
#' @description
#' This function computes the fraction of infectious
#' bites that cause an infection. The `SIS` model
#' assumes a constant fraction causes infection, \eqn{b}.
#'
#' @inheritParams F_infectivity
#' @keywords internal
#'
#' @return The constant \eqn{b}
#'
#' @seealso [SIS]
#' @export
F_infectivity.SIS <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]], b)
}


#' @title Compute the prevalence of infection
#' @description Implements [F_prevalence] for the `SIS` model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [SIS]
#' @export
F_prevalence.SIS <- function(vars, XH_obj) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the net infectiousness
#' @description Implements [F_ni] for the `SIS` model 
#'
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [SIS]
#' @export
F_ni.SIS <- function(vars, XH_obj) {
  return(with(vars,with(XH_obj, c*I/H)))}

#' @title Compute the get_HTC for the `SIS` model
#' @description Implements \code{get_HTC} for the `SIS` model 
#' with no demography. 
#' @inheritParams get_HTC
#' @return a [numeric] vector
#' @keywords internal
#' @seealso [SIS]
#' @export
get_HTC.SIS <- function(xds_obj, i) {
  with(xds_obj$XH_obj[[i]], return(c*(1-rho)/(r + sigma)))}

#' @title Get the **XH** module skill set
#'
#' @description Return a list of the **XH** module's capabilities.
#'
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return the **XH** module skill set, as a list
#' @keywords internal
#'
#' @seealso [SIS]
#' @export
get_skillset_XH.SIS = function(xds_obj, i){
  skills = list(
    dts = TRUE,
    xde = TRUE,
    delay = FALSE,
    demography  = TRUE,
    prevalence  = TRUE,
    malaria     = TRUE,
    diagnostics = "linear"
  )
  return(skills)
}

#' Check / update before solving
#'
#' @inheritParams check_XH
#' @keywords internal
#'
#' @return an **`xds`** model object
#' @seealso [SIS]
#' @export
check_XH.SIS = function(xds_obj, i){
  return(xds_obj)
}

#' @title Compute *Pf*PR by light microscopy
#' @description Implements [F_pfpr_by_lm] for the `SIS` model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [SIS]
#' @export
F_pfpr_by_lm.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, q_lm*I/H))
  return(pr)
}

#' @title Compute *Pf*PR by RDT
#' @description Implements [F_pfpr_by_rdt] for the `SIS` model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [SIS]
#' @export
F_pfpr_by_rdt.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, q_rdt*I/H))
  return(pr)
}

#' @title Compute *Pf*PR by PCR
#' @description Implements [F_pfpr_by_pcr] for the `SIS` model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @seealso [SIS]
#' @export
F_pfpr_by_pcr.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, q_pcr*I/H))
  return(pr)
}

#' Plot the density of infected individuals for the `SIS` model
#'
#' @inheritParams xds_plot_X
#' @keywords internal
#' @seealso [SIS]
#' @export
xds_plot_X.SIS = function(xds_obj, i=1, clrs=c("darkblue","darkred"), llty=1, add=FALSE){
  XH = xds_obj$outputs$orbits$XH[[i]]
  time = xds_obj$outputs$time

  if(add==FALSE)
    plot(time, 0*time, type = "n", ylim = c(0, max(XH$H)),
         ylab = "# Infected", xlab = "Time")

  add_lines_X_SIS(time, XH, xds_obj$nStrata[i], clrs, llty)
}

#' Add lines for the density of infected individuals for the `SIS` model
#'
#' @param time time points for the observations
#' @param XH parsed outputs from [parse_XH_orbits.SIS]
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @keywords internal
#'
#' @seealso [SIS]
#' @export
add_lines_X_SIS = function(time, XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
  if (length(llty)< nStrata) llty = rep(llty, nStrata)
  with(XH,{
    if(nStrata == 1){
        lines(time, S, col=clrs[1], lty = llty)
        lines(time, I, col=clrs[2], lty = llty)
    } else {
    for(i in 1:nStrata){
      lines(time, S[,i], col=clrs[1], lty = llty[i])
      lines(time, I[,i], col=clrs[2], lty = llty[i])
    }
    }})
}



#' @title Compute the steady states for the `SIS` model as a function of the daily FoI
#' @description Compute the steady state of the `SIS` model as a function of the daily FoI,
#' with fixed population density, no mortality, and no mass-treatment events.
#' @inheritParams steady_state_X
#' @return the steady states as a named [list]
#' @keywords internal
#' @seealso [SIS]
#' @export
steady_state_X.SIS = function(foi, H, xds_obj, i=1){
  with(xds_obj$XH_obj[[i]],{
    Ieq = foi*(1-rho)/(foi*(1-rho) + r + rho*foi+sigma)*H
    Seq = H-Ieq
    return(list(S=Seq, I=Ieq))
})}
