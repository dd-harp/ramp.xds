# a hybrid model tracking mean MoI for all and apparent infections

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
skill_set_XH.hMoI = function(Xname = "hMoI"){
  return(list(
    H_component = FALSE, 
    X_component = TRUE, 
    mda         = FALSE, 
    msat        = FALSE, 
    malaria     = TRUE
  ))
}

#' Check / update before solving 
#'
#' @inheritParams check_XH
#'
#' @returns an **`xds`** model object 
#' @export
check_XH.hMoI = function(xds_obj, i){
  return(xds_obj)
}

#' @title Compute Derivatives for the `hMoI` (**X** Model)
#'  
#' @description Implements [dXHdt] for the hybrid MoI model.
#' @inheritParams dXHdt
#' @return a [numeric] vector
#' @export
dXHdt.hMoI <- function(t, y, xds_obj, i) {

  foi = xds_obj$terms$FoI[[i]]

  with(get_XH_vars(y, xds_obj, i),{
    with(xds_obj$XH_obj[[i]], {
      dm1dt <- foi - r1*m1
      dm2dt <- foi - r2*m2
      return(c(dm1dt, dm2dt))
    })
  })
}

#' @title Get Variables by Name 
#' 
#' @description Get the the value of variables from the flat state variable vector \eqn{y}, and return 
#' the values as a named list 
#' 
#' @inheritParams get_XH_vars 
#' @return a [list]
#' @export
get_XH_vars.hMoI <- function(y, xds_obj, i) {
  with(xds_obj$XH_obj[[i]]$ix,{
    m1 = y[m1_ix]
    m2 = y[m2_ix]
    H = xds_obj$XH_obj[[i]]$H
    return(list(H=H,m1=m1,m2=m2))})
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_XH_ix] for the hybrid MoI model.
#' @inheritParams setup_XH_ix
#' @return none
#' @importFrom utils tail
#' @export
setup_XH_ix.hMoI <- function(xds_obj, i) {with(xds_obj,{
  
  m1_ix <- seq(from=max_ix+1, length.out=nStrata[i])
  max_ix <- tail(m1_ix, 1)
  
  m2_ix <- seq(from=max_ix+1, length.out=nStrata[i])
  max_ix <- tail(m2_ix, 1)
  
  xds_obj$XH_obj[[i]]$ix = list(m1_ix=m1_ix, m2_ix=m2_ix)
  
  xds_obj$max_ix = max_ix
  return(xds_obj)
})}

#' @title Setup XH_obj.hMoI
#' @description Implements [setup_XH_obj] for the hMoI model
#' @inheritParams setup_XH_obj
#' @return a [list] vector
#' @export
setup_XH_obj.hMoI = function(Xname, xds_obj, i, options=list()){
  xds_obj$XH_obj[[i]] = make_XH_obj_hMoI(xds_obj$nStrata[i], options)
  return(xds_obj)
}

#' @title Make parameters for hybrid MoI human model
#' @description MoI stands for Multiplicity of Infection, and refers to malarial superinfection.
#' @param nStrata is the number of human population strata
#' @param options a [list] that overwrites default values
#' 
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c1 transmission probability (efficiency) from inapparent human infections to mosquito
#' @param c2 transmission probability (efficiency) from patent human infections to mosquito
#' @param r1 recovery rate from inapparent infections
#' @param r2 recovery rate from patent infections
#' 
#' @return none
#' @export
make_XH_obj_hMoI = function(nStrata, options=list(),
                            b=0.55, r1=1/180, r2 = 1/70,
                            c1=0.015, c2=0.15){
  with(options,{
    XH_obj = list()
    class(XH_obj) <- "hMoI"

    XH_obj$b = checkIt(b, nStrata)
    XH_obj$c1 = checkIt(c1, nStrata)
    XH_obj$c2 = checkIt(c2, nStrata)
    XH_obj$r1 = checkIt(r1, nStrata)
    XH_obj$r2 = checkIt(r2, nStrata)

    return(XH_obj)
  })}

#' @title Return the parameters as a list
#' 
#' @description Parameter values for the \eqn{i^{th}} host are
#' stored as `xds_obj$XH_obj[[i]]`. This returns the stored parameter
#' values as a list.
#' 
#' @inheritParams get_XH_pars 
#' 
#' @return a [list]
#' 
#' @seealso [make_XH_obj_hMoI]
#' @export
get_XH_pars.hMoI<- function(xds_obj, i=1) {
  with(xds_obj$XH_obj[[i]],list(b=b, c1=c1, c2=c2, r1=r1, r2=r2))
}

#' @title Return the parameters as a list
#' 
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' 
#' @inheritParams change_XH_pars
#' 
#' @return an **`xds`** object
#' 
#' @export
change_XH_pars.hMoI <- function(xds_obj, i=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$XH_obj[[i]]$b <- b
    xds_obj$XH_obj[[i]]$c1 <- c1
    xds_obj$XH_obj[[i]]$c2 <- c2
    xds_obj$XH_obj[[i]]$r1 <- r1
    xds_obj$XH_obj[[i]]$r2 <- r2
    return(xds_obj)
  }))}





#' @title Size of effective infectious human population
#' @description Implements [F_X] for the hybrid MoI model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @importFrom stats pexp
#' @export
F_X.hMoI <- function(t, y, xds_obj, i) {
  with(get_XH_vars(y, xds_obj, i),{ 
    with(xds_obj$XH_obj[[i]],{
      x1 <- pexp(q = m1)
      x2 <- pexp(q = m2)
      X <- ((c2 * x2) + (c1 * (x1 - x2)))*H
      return(X)
    })
})}

#' @title Size of the human population
#' @description Implements [F_H] for the hybrid MoI model.
#' @inheritParams F_H
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H.hMoI <- function(t, y, xds_obj, i) {
  xds_obj$XH_obj[[i]]$H
}


#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_infectivity] for the hMoI model.
#' @inheritParams F_infectivity
#' @return a [numeric] vector of length `nStrata`
#' @export
F_infectivity.hMoI <- function(y, xds_obj,i) {
  with(xds_obj$XH_obj[[i]], b)
}




#' @title Setup Xinits.hMoI
#' @description Implements [setup_XH_inits] for the hMoI model
#' @inheritParams setup_XH_inits
#' @return a [list] vector
#' @export
setup_XH_inits.hMoI = function(xds_obj, H, i, options=list()){
  xds_obj$XH_obj[[i]]$H = H
  xds_obj$XH_obj[[i]]$inits = make_XH_inits_hMoI(xds_obj$nStrata[i], options)
  return(xds_obj)
}

#' @title Make inits for hybrid MoI human model
#' @description MoI stands for Multiplicity of Infection, and refers to malarial superinfection.
#' @param nStrata the number of population strata
#' @param options a [list] that overwrites default values
#' @param m1 mean MoI among inapparent human infections
#' @param m2 mean MoI among patent human infections
#' @return none
#' @export
make_XH_inits_hMoI = function(nStrata, options = list(), m1=2, m2=1){with(options,{
  m1 = checkIt(m1, nStrata)
  m2 = checkIt(m2, nStrata)
  return(list(m1=m1, m2=m2))
})}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @inheritParams change_XH_inits
#' @return an **`xds`** object
#' @export
change_XH_inits.hMoI <- function(xds_obj, i=1, options=list()) {
  with(xds_obj$XH_obj[[i]], with(options,{
    xds_obj$Xinits[[i]]$m1 = m1
    xds_obj$Xinits[[i]]$m2 = m2
    return(xds_obj)
}))}


#' @title Compute the HTC for the hMoI model
#' @description Implements [HTC] for the hMoI model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.hMoI <- function(xds_obj, i) {
  with(xds_obj$XH_obj[[i]],
       return(c2/r2 + c1*(1/r1 - 1/r2))
  )
}


#' @title parse the output of deSolve and return variables for the hMoI model
#' @description Implements [parse_XH_orbits] for the hMoI model
#' @inheritParams parse_XH_orbits
#' @return none
#' @export
parse_XH_orbits.hMoI <- function(outputs, xds_obj, i){
  with(xds_obj$XH_obj[[i]]$ix, {
    m1 = outputs[,m1_ix]
    m2 = outputs[,m2_ix]
    H = xds_obj$XH_obj[[i]]$H
    return(list(H=H, m1=m1, m2=m2))
})}


#' @title Compute the "true" nievalence of infection / parasite rate
#' @description Implements [F_ni] for the hMoI model.
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ni.hMoI<- function(vars, XH_obj) {with(XH_obj,{
  x1 <- 1 - exp(-vars$m1)
  x2 <- 1 - exp(-vars$m2)
  ni <- (c2 * x2) + (c1 * (x1 - x2))
  return(ni)
})}

#' @title Compute the "true" prevalence of infection / parasite rate
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_prevalence.hMoI<- function(vars, XH_obj) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_lm.hMoI<- function(vars, XH_obj) {
  pr = with(vars, 1-exp(-m2))
  return(pr)
}

#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_rdt.hMoI<- function(vars, XH_obj) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}

#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_pcr.hMoI<- function(vars, XH_obj) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}

#' @title Compute Steady States for `hMoI` (**X**-Model)
#' 
#' @description Compute the steady state of the hMoI model as a function of the daily foi
#' @inheritParams steady_state_X
#' @return the steady states as a named vector
#' @export
steady_state_X.hMoI = function(foi, H, XH_obj){with(XH_obj,{
  m1 = foi/r1
  m2 = foi/r2
  return(c(m1=m1, m2=m2))
})}