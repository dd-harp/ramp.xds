# specialized methods for the human SIS module

#' @title `SIS` Derivatives Function (an **XH** Module)
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
#' computes \eqn{dH/dt} and \eqn{dI/dt}. 
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
#' The module includes a port to model mass treatment, \eqn{\xi(t)}.
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
#' 
#' @export
dXHdt.SIS <- function(t, y, xds_obj, i) {

  foi <- xds_obj$terms$FoI[[i]]
  
  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]], {
      dH <- Births(t, H, births) + D_matrix %*% H
      dI <- foi*(H-I) - r*I + D_matrix %*% I 
      dI <- dI - mda(t)*I - msat(t)*I 
      return(c(dH, dI))
    })
  })
}



#' @title Setup `SIS` (**XH** component)
#' 
#' @description Set up an `SIS` model object 
#' for the **XH** component 
#' 
#' @inheritParams setup_XH_obj
#' 
#' @return the **`xds`** model object
#' @noRd
#'
#' @export
setup_XH_obj.SIS = function(Xname, xds_obj, i, options=list()){
  xds_obj$XH_obj[[i]] = make_XH_obj_SIS(xds_obj$nStrata[1], options)
  xds_obj$XH_obj[[i]]$skill_set <- skill_set_XH("SIS")
  return(xds_obj)
}

#' @title Make an SIS **XH** model object 
#' 
#' @param nStrata is the number of population strata
#' @param options a named list with parameter values to overwrite defaults
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c transmission probability (efficiency) from human to mosquito
#' @param r recovery rate
#' @param d_lm detection by light microscopy  
#' @param d_rdt detection by RDT 
#' @param d_pcr detection by pcr 
#' 
#' @return an **XH** model object
#' @keywords internal 
#' 
#' @export
make_XH_obj_SIS = function(nStrata, options=list(),
                             b=0.55, r=1/180, c=0.15,
                             d_lm =0.8, d_rdt=0.8, d_pcr=0.9){
  with(options,{
    XH_obj = list()
    class(XH_obj) <- "SIS"
    
    XH_obj$b = checkIt(b, nStrata)
    XH_obj$c = checkIt(c, nStrata)
    XH_obj$r = checkIt(r, nStrata)
    XH_obj$d_lm = checkIt(d_lm, nStrata)
    XH_obj$d_rdt = checkIt(d_rdt, nStrata)
    XH_obj$d_pcr = checkIt(d_pcr, nStrata)
    
    # Ports for demographic models
    XH_obj$D_matrix = diag(0, nStrata) 
    births = "zero"
    class(births) = births
    XH_obj$births = births 
    XH_obj$mda = F_zero 
    XH_obj$msat = F_zero 
    
    return(XH_obj)
  })}


#' @title Set new values for the SIS model 
#' 
#' @description A utility to change parameters
#' values for the SIS model (**XH** component)
#' 
#' `options` is a named list, and: 
#' 
#' + \eqn{b} is set to `options$b`
#' + \eqn{c} is set to `options$c`
#' + \eqn{r} is set to `options$r`
#' + \eqn{d_lm} is set to `options$d_lm`
#' + \eqn{d_rdt} is set to `options$d_rdt`
#' + \eqn{d_pcr} is set to `options$d_pcr`
#'  
#' @inheritParams change_XH_pars 
#' 
#' @seealso [get_XH_pars.SIS()] and the github.io essay on [SIS Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
#' 
#' @return an **`xds`** model object
#' 
#' @export
change_XH_pars.SIS <- function(xds_obj, i=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$XH_obj[[i]]$b <- b
    xds_obj$XH_obj[[i]]$c <- c
    xds_obj$XH_obj[[i]]$r <- r
    xds_obj$XH_obj[[i]]$d_lm <- d_lm
    xds_obj$XH_obj[[i]]$d_rdt <- d_rdt
    xds_obj$XH_obj[[i]]$d_pcr <- d_pcr
    return(xds_obj)
  }))}

#' @title Get *SIS* module parameters 
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
#' @export
get_XH_pars.SIS <- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]],list(b=b, c=c, r=r))
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the SIS model.
#' @inheritParams setup_XH_ix
#' @return none
#' @importFrom utils tail
#' @noRd
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

#' @title Get Variables by Name 
#' 
#' @description Get the the value of variables 
#' from the flat state variable 
#' vector \eqn{y}, and return 
#' the values as a named list 
#' 
#' @inheritParams get_XH_vars 
#' @keywords internal
#' @return a [list]
#' @export
get_XH_vars.SIS <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]]$ix,{
    H = y[H_ix]
    I = y[I_ix]
    S = H-I
    return(list(S=S,I=I,H=H))})
}


#' @title parse the output of deSolve and return variables for the SIS model
#' @description Implements [parse_XH_orbits] for the SIS model
#' @inheritParams parse_XH_orbits
#' @keywords internal
#' @return none
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


#' @title Setup initial values for *SIS*
#' 
#' @inheritParams setup_XH_inits
#' 
#' @return a **`ramp.xds`** object 
#' @noRd 
#' @export
setup_XH_inits.SIS = function(xds_obj, H, i=1, options=list()){
  xds_obj$XH_obj[[i]]$inits = make_XH_inits_SIS(xds_obj$nStrata[i], H, options)
  return(xds_obj)
}

#' @title Make initial values for the SIS xde human model, with defaults
#' 
#' @description Set the initial values  
#' 
#' @param nStrata the number of strata in the model
#' @param H the initial human population density
#' @param options a [list] to overwrite defaults
#' @param I the initial values of the parameter I
#' 
#' @keywords internal 
#' 
#' @return a [list]
#' @export
make_XH_inits_SIS = function(nStrata, H, options=list(), I=1){
  with(options,{
    H = unname(as.vector(checkIt(H, nStrata)))
    I = unname(as.vector(checkIt(I, nStrata)))
    return(list(H=H, I=I))
})}

#' @title `SIS` - Change Initial Values 
#' 
#' @description 
#' 
#' For the **XH** module, `SIS,`
#' change the initial value(s) for the variable \eqn{I}. 
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
#' @export
change_XH_inits.SIS <- function(xds_obj, i=1, options=list()) {
  with(get_XH_inits(xds_obj, i), with(options,{
    xds_obj$XH_obj[[i]]$inits$I = I
    return(xds_obj)
  }))}

#' @title Compute Infectious Density 
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

#' @title Compute Population Density 
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

#' @title Compute Infectivity 
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

#' @title Compute the HTC for the SIS model
#' @description Implements [HTC] for the SIS model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @keywords internal
#' @export
HTC.SIS <- function(xds_obj, i) {
  with(xds_obj$XH_obj[[i]], return(c/r))}

#' @title The **XH** Module Skill Set 
#' 
#' @description The **XH** skill set is a list of 
#' an module's capabilities. 
#' 
#' @note This method dispatches on `class(xds_obj$XH_obj)` 
#'
#' @inheritParams skill_set_XH
#' 
#' @return the skill set, as a list 
#' @keywords internal
#' 
#' @export
skill_set_XH.SIS = function(Xname = "SIS"){
  return(list(
    demography  = TRUE, 
    prevalence  = TRUE, 
    malaria     = TRUE, 
    diagnostics = FALSE 
  ))
}

#' Check / update before solving 
#'
#' @inheritParams check_XH
#' @keywords internal
#'
#' @returns an **`xds`** model object 
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
  pr = with(XH_obj, with(vars, d_lm*I/H))
  return(pr)
}

#' @title Compute *Pf*PR by RDT 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_rdt.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, d_rdt*I/H))
  return(pr)
}

#' @title Compute *Pf*PR by PCR 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_pcr.SIS <- function(vars, XH_obj) {
  pr = with(XH_obj, with(vars, d_pcr*I/H))
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


