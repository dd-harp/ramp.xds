# specialized methods for the human SIS model



#' @title Derivatives function for the `SIS` model (*XH** Model)
#'  
#' @description 
#' 
#' Compute the derivatives for SIS compartmental model:
#' + \eqn{S} is the density of susceptible humans (or hosts)
#' + \eqn{I} is the density of infected humans (or hosts)
#' + \eqn{H=S+I}  is human (or host) population density
#'  
#' The clearance rate for infections is \eqn{r}, and by assumption, 
#' individuals are assumed to be susceptible to infection after clearing
#' infections.
#' 
#' The model has a port to model mass treatment, in equations \eqn{\xi(t)}.
#' 
#' The **`xds`** implementation computes \eqn{dH/dt} 
#' rather than \eqn{dS/dt.} In the functions [get_XH_vars] 
#' and [parse_XH_orbits],
#' \eqn{S} is computed as \eqn{S=H-I} and listed as a variable. The
#' derivatives computed are:
#' 
#' \deqn{
#' \begin{array}{rl}
#' dH/dt = & B(t,H)  + D \cdot H \\
#' dI/dt = & h (H-I) - r I - \xi(t) +  D \cdot I
#' \end{array}
#' }
#' where \eqn{S=H-I}; 
#' 
#' \eqn{\xi(t)} is a function to simulate mass treatment;
#' \eqn{B(t, H)} is the time-dependent birth rate; and \eqn{D} is a linear operator, a matrix describing demographic changes,
#' including mortality, migration, and aging; 
#' 
#' @inheritParams dXHdt
#' 
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
#' 
#' @return an **XH** model object 
#' 
#' @export
make_XH_obj_SIS = function(nStrata, options=list(),
                             b=0.55, r=1/180, c=0.15){
  with(options,{
    XH_obj = list()
    class(XH_obj) <- "SIS"
    
    XH_obj$b = checkIt(b, nStrata)
    XH_obj$c = checkIt(c, nStrata)
    XH_obj$r = checkIt(r, nStrata)
    
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
#'  
#' @inheritParams change_XH_pars 
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
    return(xds_obj)
  }))}

#' @title Get *SIS* model parameters 
#' 
#' @description 
#' Parameter values for the \eqn{i^{th}} host are
#' stored as `xds_obj$XH_obj[[i]]`. 
#' This returns the stored parameter
#' values as a list.
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
#' @description Get the the value of variables from the flat state variable vector \eqn{y}, and return 
#' the values as a named list 
#' 
#' @inheritParams get_XH_vars 
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
#' @return none
#' @export
parse_XH_orbits.SIS <- function(outputs, xds_obj, i) {
  with(xds_obj$XH_obj[[i]]$ix,{
    H <- outputs[,H_ix]
    I <- outputs[,I_ix]
    S <- H-I
    ni <- xds_obj$XH_obj[[i]]$c*I/H
    true_pr <- I/H
    vars <- list(S=S, I=I, H=H, ni=ni, true_pr=true_pr)
    return(vars)
})}


#' @title Setup initial values for *SIS*
#' 
#' @inheritParams setup_XH_inits
#' 
#' @return a **`ramp.xds`** object 
#' @export
setup_XH_inits.SIS = function(xds_obj, H, i, options=list()){
  xds_obj$XH_obj[[i]]$inits = make_XH_inits_SIS(xds_obj$nStrata[i], H, options)
  return(xds_obj)
}

#' @title Make initial values for the SIS xde human model, with defaults
#' @param nStrata the number of strata in the model
#' @param H the initial human population density
#' @param options a [list] to overwrite defaults
#' @param I the initial values of the parameter I
#' @return a [list]
#' @export
make_XH_inits_SIS = function(nStrata, H, options=list(), I=1){
  with(options,{
    H = unname(as.vector(checkIt(H, nStrata)))
    I = unname(as.vector(checkIt(I, nStrata)))
    return(list(H=H, I=I))
  })}

#' @title Return the parameters as a list
#' 
#' @inheritParams change_XH_inits 
#' 
#' @return an **`xds`** object
#' @export
change_XH_inits.SIS <- function(xds_obj, i=1, options=list()) {
  with(get_XH_inits(xds_obj, i), with(options,{
    xds_obj$Xinits[[i]]$I = I
    return(xds_obj)
  }))}

#' @title Compute Infectious Density 
#' 
#' @description In the *SIS* model family, infectious
#' density is \eqn{cI}. 
#' 
#' @inheritParams F_X
#' 
#' @return Infectious density 
#' @export
F_X.SIS <- function(t, y, xds_obj, i) {
  I = y[xds_obj$XH_obj[[i]]$ix$I_ix]
  X = with(xds_obj$XH_obj[[i]], c*I)
  return(X)
}

#' @title Compute Population Density 
#' 
#' @description Implements [F_H] for the *SIS* module
#' 
#' @inheritParams F_H
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
#' @export
F_prevalence.SIS <- function(vars, XH_obj) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute the net infectiousness
#' @description Implements [F_ni] for the SIS model.
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ni.SIS <- function(vars, XH_obj) {
  return(with(vars,with(XH_obj, 
                        c*I/H 
  )))}

#' @title Compute the HTC for the SIS model
#' @description Implements [HTC] for the SIS model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.SIS <- function(xds_obj, i) {
  with(xds_obj$XH_obj[[i]],
       return(c/r)
  )
}


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
#' @export
F_pfpr_by_lm.SIS <- function(vars, XH_obj) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute *Pf*PR by RDT 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_rdt.SIS <- function(vars, XH_obj) {
  pr = with(vars, I/H)
  return(pr)
}

#' @title Compute *Pf*PR by PCR 
#' @description Implements [F_prevalence] for the SIS model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_pcr.SIS <- function(vars, XH_obj) {
  pr = with(vars, I/H)
  return(pr)
}

#' Plot the density of infected individuals for the SIS model
#'
#' @inheritParams xds_plot_X
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
#' @export
steady_state_X.SIS = function(foi, H, XH_obj){with(XH_obj,{
  Ieq = foi/(foi+r)*H
  Seq = H-Ieq
  return(list(S=Seq, I=Ieq))
})}


