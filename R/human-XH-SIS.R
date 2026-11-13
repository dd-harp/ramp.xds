# specialized methods for the human SIS module

#' @title The `SIS` module for the **XH** component
#' 
#' @description
#' This implements a family of SIS compartmental models for malaria with parameters to model treating and curing infections, 
#' and with functions describing human demographic change 
#' and mass treatment.
#' 
#' @section State Variables:
#' 
#' \describe{
#'   \item{`H`}{total human (host) population density}
#'   \item{`I`}{density of infected humans (hosts)}
#'   \item{`S=H-I`}{density of susceptible humans (hosts)}
#' }
#'
#' @section Prevalence:
#' 
#' \describe{
#'   \item{`x`}{true prevalence is \eqn{x = I/H}}
#' }
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
#'   \item{`sigma` or \eqn{\sigma}}{the fraction of incident infections that are treated and cured}
#'   \item{`q_lm`}{expected prevalence by light microscopy would be \eqn{q_{lm} \;x}}
#'   \item{`q_rdt`}{expected prevalence by RDT would be \eqn{q_{rdt} \;x}}
#'   \item{`q_pcr`}{expected prevalence by PCR would be \eqn{q_{pcr} \;x}}
#' } 
#' 
#' @section Dynamical Term:
#' 
#' \describe{
#'   \item{\eqn{h} or `foi`}{The force of infection is computed and stored by [Exposure] as `FoI`}
#' }
#' 
#' 
#' @section Dynamics:
#' 
#' The model for demographic change has terms to compute births (\eqn{\mathbf{B}}) and 
#' deaths (\eqn{\mathbf{D}}): 
#' \deqn{
#' dH/dt = B(t,H) - \mathbf{D}(H)}
#' 
#' The model for infection dynamics assumes that infections
#' clear naturally at a constant per-capita rate \eqn{r}. 
#' The model also includes two parameters to model
#' treating and curing of malaria: a fraction \eqn{\rho} of incident infections are treated and cured;
#' and a fraction \eqn{\sigma} of prevalent infections are treated and cured. 
#' After clearing, individuals are 
#' susceptible to infection. 
#' A term for mass treatment increases the rate infections are cured: (\eqn{\Xi(t)}).
#' 
#' Infection dynamics are described by:
#' 
#' \deqn{
#' dI/dt = h(1-\rho)(H-I) - (r + \rho h + \sigma) I - \Xi(t) I  - \mathbf{D}(I) }
#'
#' The mortality function \eqn{\mathbf{D}(I)} computes total mortality for \eqn{I} using
#' the same model as for \eqn{H}: 
#' for example, if \eqn{D(H)=\mu H}, then \eqn{D(I)= \mu I.} This model has no parameter 
#' or terms to describe disease-induced mortality.
#' 
#' In the compartmental model, individuals are either susceptible (\eqn{S}), or infected and 
#' infectious (\eqn{I}), so  \eqn{S=H-I.} The equation is redundant. 
#' After solving, the orbit of the variable \eqn{S=H-I} is computed and attached.
#' 
#' @section Exposure:
#' 
#' The FoI is computed (in [Exposure]) under the assumption that 
#' a constant fraction of infectious bites causes an infection the parameter \eqn{b.} 
#' 
#' @section Infectiousness:
#' 
#' The fraction of mosquitoes that become infected after blood feeding on 
#' someone who is infectious is \eqn{c.} 
#' The infectious density of the human (hos) population is \eqn{cI.}
#' 
#' @section Demographic Change: 
#' 
#' Demographic processes are handled in the standard way. Birth rates and death
#' rates are initially set to zero. See
#' + [setup_births] to configure birth rates; and 
#' + [setup_deaths] to configure death rates
#' 
#' @section Mass Treatment: 
#'
#' Mass treatment clears infections. At basic setup
#' \eqn{\Xi(t)=0}. To set up mass treatment, see [setup_mass_treatment]. 
#' Mass treatment is either: 
#' 
#' \describe{
#'   \item{`mda(t)` or \eqn{\Xi(t)}}{mass drug administration}
#'   \item{`msat` or \eqn{\Xi(t)}}{mass screen and treat}
#' }
#' 
#' 
#' @name SIS
#' @rdname SIS
NULL


#' @title Compute derivatives for `SIS` (**XH**)
#'
#' @description
#'
#' Computes the derivatives for SIS compartmental model with variables:
#'
#' + \eqn{I} - the density of infected humans (or hosts)
#' + \eqn{S} - the density of susceptible humans (or hosts)
#' + \eqn{H} - human (or host) population density
#'
#' The model assumes \eqn{S+I=H.} This function computes
#' \eqn{dH/dt} and \eqn{dI/dt}.
#' For convenience, \eqn{S} is also computed
#' by [get_XH_vars.SIS]
#' and [parse_XH_orbits.SIS].
#'
#' The force of infection or "happenings" rate is \eqn{h}. It is computed
#' upstream and used here.
#'
#' The clearance rate for infections is \eqn{r}, and by assumption,
#' individuals are assumed to be susceptible to infection after clearing
#' infections.
#'
#' The module includes a port to model mass treatment, \eqn{\Xi(t)}.
#'
#' **Human Demographic**  change is modeled with two functions:
#'
#' + \eqn{B(t, H)} is the time-dependent population birth rate;
#'
#' + \eqn{D} is a linear operator, a matrix describing mortality, migration, aging,
#' and dynamical transfers among population strata.
#'
#' The
#' derivatives computed are:
#'
#' \deqn{
#' \begin{array}{rl}
#' dH/dt = & B(t,H)  + D \cdot H \\
#' dI/dt = & h (H-I) - r I - \xi(t) +  D \cdot I
#' \end{array}
#' }
#'
#' @inheritParams dXHdt
#'
#' @seealso [SIS Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#' @return the derivatives, as a vector
#' @keywords internal
#' @export
dXHdt.SIS <- function(t, y, xds_obj, i) {

  foi <- xds_obj$terms$FoI[[i]]

  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]], {
      dH <- Births(t, xds_obj, i) - Deaths(t, H, xds_obj, i)
      dI <- foi*(1-rho)*(H-I) - (r + rho*h + sigma)*I - Deaths(t, I, xds_obj, i)
      dI <- dI - mda(t)*I - msat(t)*I
      return(c(dH, dI))
    })
  })
}

#' @title Set up `SIS` (**XH**)
#'
#' @description
#'
#' This sets up an `SIS` model object for the **XH** module for the \eqn{i^{th}} host species
#' + the object is created by [make_XH_obj_SIS]
#' + the initial conditions are set by [setup_XH_inits]
#' + `options` is passed to both to override the defaults
#'
#' @seealso [SIS], [make_XH_obj_SIS]
#'
#' @note The module object is stored at `xds_obj$XH_obj[[i]]`
#'
#' @inheritParams setup_XH_obj
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
setup_XH_obj.SIS = function(XHname, residence, xds_obj, options=list(), i=1){
  xds_obj$XHname = "SIS"
  xds_obj$XH_obj[[i]] = make_XH_obj_SIS(xds_obj$nStrata[1], options)
  xds_obj <- setup_XH_inits(xds_obj, options=options, i=i)
  xds_obj <- setup_skillset_XH(xds_obj, i)
  xds_obj <- setup_blood_search_weights("setup", xds_obj, i=i)
  xds_obj <- setup_timespent("setup", xds_obj, list(residence=residence), i)
  xds_obj <- setup_mass_treatment(xds_obj, i=i)
  xds_obj <- setup_demog_obj(xds_obj, i)
  xds_obj <- setup_time_away("setup", xds_obj, i=i)
  xds_obj <- setup_travel_eir("no_travel", xds_obj, i=i)
  return(xds_obj)
}

#' @title Make an SIS **XH** model object
#'
#' @param nStrata is the number of population strata
#' @param options a named list with parameter values to override defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c transmission probability (efficiency) from human to mosquito
#' @param r recovery rate
#' @param q_lm detection by light microscopy
#' @param q_rdt detection by RDT
#' @param q_pcr detection by pcr
#'
#' @return an **XH** model object
#' @keywords internal
#'
#' @export
make_XH_obj_SIS = function(nStrata, options=list(),
                             b=0.55, r=1/180, c=0.15,
                             sigma=0, rho=0,
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
    XH_obj$q_pcr = checkIt(q_pcr, nStrata)
    XH_obj$q_pcr = checkIt(q_pcr, nStrata)

    return(XH_obj)
  })}


#' @title Get parameters for `SIS` (**XH**)
#'
#' @description For the `SIS` module (**XH**) for the \eqn{i^{th}} species,
#' change parameter values named in `options` leaving other values unchanged.
#'
#' `options` is a named list, and:
#'
#' + \eqn{b} is set to `options$b`
#' + \eqn{c} is set to `options$c`
#' + \eqn{r} is set to `options$r`
#' + \eqn{q_{lm}} is set to `options$q_lm`
#' + \eqn{q_{rdt}} is set to `options$q_rdt`
#' + \eqn{q_{pcr}} is set to `options$q_pcr`
#'
#' @inheritParams change_XH_pars
#'
#' @seealso [get_XH_pars.SIS()] and the github.io essay on [SIS Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#'
#' @return an **`xds`** object
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
#' @return SIS model parameters
#' @keywords internal
#' @export
get_XH_pars.SIS <- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]],list(b=b, c=c, r=r,
                                q_lm=q_lm, q_rdt=q_rdt, q_pcr=q_pcr))
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the SIS model.
#' @inheritParams setup_XH_ix
#' @return a [list]
#' @importFrom utils tail
#' @keywords internal
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
#' @export
get_XH_vars.SIS <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]]$ix,{
    H = y[H_ix]
    I = y[I_ix]
    S = H-I
    return(list(S=S,I=I,H=H))})
}


#' @title Parse outputs for `SIS` (**XH**)
#' @description Implements [parse_XH_orbits] for the SIS model
#' @inheritParams parse_XH_orbits
#' @return a [list]
#' @keywords internal
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


#' @title Setup initial values for `SIS` (**XH**)
#'
#' @description Set initial values for all the variables in the `SIS` module
#' (**XH**) for the \eqn{i^{th}} species.
#' + [make_XH_inits_SIS] supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_XH_inits] or
#' see the inline module documentation.
#'
#' @inheritParams setup_XH_inits
#'
#' @return an **`xds`** object
#' @keywords internal
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
#' @seealso [setup_XH_inits.SIS]
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
#' @description For the `SIS` module (**XH**) for the \eqn{i^{th}} species,
#' change initial values named in `options` leaving other values unchanged.
#'
#' Change the initial value(s) for the variable \eqn{I}.
#'
#' The argument
#' passed as `options` should be a named list, and the
#' new initial value(s) are set to `options$I`
#'
#' @note Initial values for human population size,
#' the variable \eqn{H}, must be set using [change_H]
#'
#' @inheritParams change_XH_inits
#'
#' @seealso The variable names are returned by calling [get_XH_inits].
#' The function [make_XH_inits_SIS] is called during setup.
#'
#' @return an **`xds`** object
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
#' @description In the *SIS* model family, infectious
#' density is \eqn{cI}.
#'
#' @inheritParams F_I
#' @keywords internal
#' @return Infectious density
#' @export
F_I.SIS <- function(t, y, xds_obj, i) {
  I = y[xds_obj$XH_obj[[i]]$ix$I_ix]
  X = with(xds_obj$XH_obj[[i]], c*I)
  return(X)
}

#' @title Compute population density
#'
#' @description Implements [F_H] for the *SIS* module
#'
#' @inheritParams F_H
#' @keywords internal
#'
#' @return Human (host) population density, \eqn{H}
#'
#' @export
F_H.SIS <- function(t, y, xds_obj, i){
  with(get_XH_vars(y, xds_obj, i), return(H))
}

#' @title Compute infectivity
#'
#' @description
#' This function computes the fraction of infectious
#' bites that cause an infection. The SIS model,
#' assumes a constant fraction causes infection, \eqn{b}.
#'
#' @inheritParams F_infectivity
#' @keywords internal
#'
#' @return The constant \eqn{b}
#'
#' @export
F_infectivity.SIS <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]], b)
}


#' @title Compute the prevalence of infection
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_prevalence.SIS <- function(vars, XH_obj) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the net infectiousness
#' @description Implements [F_ni] for the SIS model.
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_ni.SIS <- function(vars, XH_obj) {
  return(with(vars,with(XH_obj, c*I/H)))}

#' @title Compute the get_HTC for the SIS model
#' @description Implements \code{get_HTC} for the SIS model with demography.
#' @inheritParams get_HTC
#' @return a [numeric] vector
#' @keywords internal
#' @export
get_HTC.SIS <- function(xds_obj, i) {
  with(xds_obj$XH_obj[[i]], return(c/r))}

#' @title The **XH** module skill set
#'
#' @description The **XH** skill set is a list of
#' a module's capabilities.
#'
#' @note This method dispatches on `class(xds_obj$XH_obj)`
#'
#' @inheritParams setup_skillset_XH
#'
#' @return the **`xds`** object
#' @keywords internal
#'
#' @export
setup_skillset_XH.SIS = function(xds_obj,i){
  skills =   list(
    demography  = TRUE,
    prevalence  = TRUE,
    malaria     = TRUE,
    diagnostics = "linear"
  )
  xds_obj$XH_obj[[i]]$skill_set = skills
  return(xds_obj) 
}

#' Check / update before solving
#'
#' @inheritParams check_XH
#' @keywords internal
#'
#' @return an **`xds`** object
#' @export
check_XH.SIS = function(xds_obj, i){
  return(xds_obj)
}







#' @title Compute *Pf*PR by light microscopy
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_lm.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, q_lm*I/H))
  return(pr)
}

#' @title Compute *Pf*PR by RDT
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_rdt.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, q_rdt*I/H))
  return(pr)
}

#' @title Compute *Pf*PR by PCR
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_pcr.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, q_pcr*I/H))
  return(pr)
}

#' Plot the density of infected individuals for the SIS model
#'
#' @inheritParams xds_plot_X
#' @keywords internal
#' @export
xds_plot_X.SIS = function(xds_obj, i=1, clrs=c("darkblue","darkred"), llty=1, add=FALSE){
  XH = xds_obj$outputs$orbits$XH[[i]]
  time = xds_obj$outputs$time

  if(add==FALSE)
    plot(time, 0*time, type = "n", ylim = c(0, max(XH$H)),
         ylab = "# Infected", xlab = "Time")

  add_lines_X_SIS(time, XH, xds_obj$nStrata[i], clrs, llty)
}

#' Add lines for the density of infected individuals for the SIS model
#'
#' @param time time points for the observations
#' @param XH parsed outputs, from parse_Xorbits_SIS
#' @param nStrata the number of population strata
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @keywords internal
#'
#' @export
add_lines_X_SIS = function(time, XH, nStrata, clrs=c("darkblue","darkred"), llty=1){
  if (length(llty)< nStrata) llty = rep(llty, nStrata)
  with(XH,{
    if(nStrata == 1){
        lines(time, S, col=clrs[1], lty = llty)
        lines(time, I, col=clrs[2], lty = llty)
    } else {
    for(i in 1:nStrata)
      lines(time, S[,i], col=clrs[1], lty = llty[i])
      lines(time, I[,i], col=clrs[2], lty = llty[i])
    }})
}



#' @title Compute the steady states for the SIS model as a function of the daily EIR
#' @description Compute the steady state of the SIS model as a function of the daily eir.
#' @inheritParams steady_state_X
#' @return the steady states as a named vector
#' @keywords internal
#' @export
steady_state_X.SIS = function(foi, H, xds_obj, i=1){
  with(xds_obj$XH_obj[[i]],{
    Ieq = foi/(foi+r)*H
    Seq = H-Ieq
    return(list(S=Seq, I=Ieq))
})}
