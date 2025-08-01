# a hybrid model tracking mean MoI for all and apparent infections

#' @title Compute Derivatives for the `hMoI` (**X**-Model)
#'  
#' @description Implements [dXdt] for the hybrid MoI model.
#' @inheritParams dXdt
#' @return a [numeric] vector
#' @export
dXdt.hMoI <- function(t, y, pars, i) {

  foi = pars$FoI[[i]]

  with(pars$ix$X[[i]],{
    m1 = y[m1_ix]
    m2 = y[m2_ix]

    with(pars$Xpar[[i]], {
      dm1dt <- foi - r1*m1
      dm2dt <- foi - r2*m2
      return(c(dm1dt, dm2dt))
    })
  })
}

#' @title Compute Steady States for `hMoI` (**X**-Model)
#' 
#' @description Compute the steady state of the hMoI model as a function of the daily foi
#' @inheritParams xde_steady_state_X
#' @return the steady states as a named vector
#' @export
xde_steady_state_X.hMoI = function(foi, H, Xpar){with(Xpar,{
  m1 = foi/r1
  m2 = foi/r2
  return(c(m1=m1, m2=m2))
})}

#' @title Make parameters for hybrid MoI human model
#' @description MoI stands for Multiplicity of Infection, and refers to malarial superinfection.
#' @param nStrata is the number of human population strata
#' @param Xopts a [list] that overwrites default values
#' @param b transmission probability (efficiency) from mosquito to human
#' @param c1 transmission probability (efficiency) from inapparent human infections to mosquito
#' @param c2 transmission probability (efficiency) from patent human infections to mosquito
#' @param r1 recovery rate from inapparent infections
#' @param r2 recovery rate from patent infections
#' @return none
#' @export
make_Xpar_hMoI = function(nStrata, Xopts=list(),
                            b=0.55, r1=1/180, r2 = 1/70,
                            c1=0.015, c2=0.15){
  with(Xopts,{
    Xpar = list()
    class(Xpar) <- "hMoI"

    Xpar$b = checkIt(b, nStrata)
    Xpar$c1 = checkIt(c1, nStrata)
    Xpar$c2 = checkIt(c2, nStrata)
    Xpar$r1 = checkIt(r1, nStrata)
    Xpar$r2 = checkIt(r2, nStrata)

    return(Xpar)
  })}

#' @title Return the parameters as a list
#' @description Parameter values for the \eqn{i^{th}} host are
#' stored as `pars$Xpar[[i]]`. This returns the stored parameter
#' values as a list.
#' @inheritParams get_Xpars
#' @return a [list]
#' @seealso [make_Xpar_hMoI]
#' @export
get_Xpars.hMoI<- function(pars, i=1) {
  with(pars$Xpar[[i]],list(b=b, c1=c1, c2=c2, r1=r1, r2=r2))
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @inheritParams set_Xpars
#' @return an **`xds`** object
#' @export
set_Xpars.hMoI <- function(pars, i=1, Xopts=list()) {
  nHabitats <- pars$nHabitats
  with(pars$Xpar[[i]], with(Xopts,{
    pars$Xpar[[i]]$b <- b
    pars$Xpar[[i]]$c1 <- c1
    pars$Xpar[[i]]$c2 <- c2
    pars$Xpar[[i]]$r1 <- r1
    pars$Xpar[[i]]$r2 <- r2
    return(pars)
  }))}

#' @title Setup Xpar.hMoI
#' @description Implements [setup_Xpar] for the hMoI model
#' @inheritParams setup_Xpar
#' @return a [list] vector
#' @export
setup_Xpar.hMoI = function(Xname, pars, i, Xopts=list()){
  pars$Xpar[[i]] = make_Xpar_hMoI(pars$nStrata[i], Xopts)
  return(pars)
}



#' @title Size of effective infectious human population
#' @description Implements [F_X] for the hybrid MoI model.
#' @inheritParams F_X
#' @return a [numeric] vector of length `nStrata`
#' @importFrom stats pexp
#' @export
F_X.hMoI <- function(t, y, pars, i) {
  with(pars$ix$X[[i]],{
    m1 = y[m1_ix]
    m2 = y[m2_ix]
    H <- F_H(t, y, pars, i)
    with(pars$Xpar[[i]],{
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
F_H.hMoI <- function(t, y, pars, i) {
  pars$Xpar[[i]]$H
}


#' @title Infection blocking pre-erythrocytic immunity
#' @description Implements [F_b] for the hMoI model.
#' @inheritParams F_b
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b.hMoI <- function(y, pars,i) {
  with(pars$Xpar[[i]], b)
}




#' @title Setup Xinits.hMoI
#' @description Implements [setup_Xinits] for the hMoI model
#' @inheritParams setup_Xinits
#' @return a [list] vector
#' @export
setup_Xinits.hMoI = function(pars, H, i, Xopts=list()){
  pars$Xpar[[i]]$H = H
  pars$Xinits[[i]] = make_Xinits_hMoI(pars$nStrata[i], Xopts)
  return(pars)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @inheritParams set_Xinits
#' @return an **`xds`** object
#' @export
set_Xinits.hMoI <- function(pars, i=1, Xopts=list()) {
  with(pars$Xpar[[i]], with(Xopts,{
    pars$Xinits[[i]]$m1 = m1
    pars$Xinits[[i]]$m2 = m2
    return(pars)
  }))}

#' @title Make inits for hybrid MoI human model
#' @description MoI stands for Multiplicity of Infection, and refers to malarial superinfection.
#' @param nStrata the number of population strata
#' @param Xopts a [list] that overwrites default values
#' @param m1 mean MoI among inapparent human infections
#' @param m2 mean MoI among patent human infections
#' @return none
#' @export
make_Xinits_hMoI = function(nStrata, Xopts = list(), m1=2, m2=1){with(Xopts,{
  m1 = checkIt(m1, nStrata)
  m2 = checkIt(m2, nStrata)
  return(list(m1=m1, m2=m2))
})}


#' @title Compute the HTC for the hMoI model
#' @description Implements [HTC] for the hMoI model with demography.
#' @inheritParams HTC
#' @return a [numeric] vector
#' @export
HTC.hMoI <- function(pars, i) {
  with(pars$Xpar[[i]],
       return(c2/r2 + c1*(1/r1 - 1/r2))
  )
}

#' @title Add indices for human population to parameter list
#' @description Implements [setup_Xix] for the hybrid MoI model.
#' @inheritParams setup_Xix
#' @return none
#' @importFrom utils tail
#' @export
setup_Xix.hMoI <- function(pars, i) {with(pars,{
  m1_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(m1_ix, 1)

  m2_ix <- seq(from = max_ix+1, length.out=nStrata[i])
  max_ix <- tail(m2_ix, 1)

  pars$max_ix = max_ix
  pars$ix$X[[i]] = list(m1_ix=m1_ix, m2_ix=m2_ix)
  return(pars)
})}



#' @title Update inits for hybrid MoI human model from a vector of states
#' @inheritParams update_Xinits
#' @return none
#' @export
update_Xinits.hMoI <- function(pars, y, i) {
  with(pars$ix$X[[i]],{
    m1 = y[m1_ix]
    m2 = y[m2_ix]
    pars$Xinits[[i]] = make_Xinits_hMoI(pars$nStrata[i], m1=m1, m2=m2)
    return(pars)
  })}

#' @title Parse the output of deSolve and return variables for the hMoI model
#' @description Implements [parse_Xorbits] for the hMoI model
#' @inheritParams parse_Xorbits
#' @return none
#' @export
parse_Xorbits.hMoI <- function(outputs, pars, i){
  H = pars$Xpar[[i]]$H
  with(pars$ix$X[[i]], {
    m1 = outputs[,m1_ix]
    m2 = outputs[,m2_ix]
    ni = F_ni(list(m1=m1, m2=m2, H=H), pars$Xpar[[1]])
    pr = F_prevalence(list(m1=m1, m2=m2, H=H), pars$Xpar[[1]])
    pr_by_lm = F_pfpr_by_lm(list(m1=m1, m2=m2, H=H), pars$Xpar[[1]])
    return(list(H=H, m1=m1, m2=m2, ni=ni, pr=pr, pr_lm= pr_by_lm))
  })}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar`.
#' @inheritParams get_Xinits
#' @return none
#' @export
get_Xinits.hMoI <- function(pars, i){pars$Xinits[[i]]}

#' @title Compute the "true" nievalence of infection / parasite rate
#' @description Implements [F_ni] for the hMoI model.
#' @inheritParams F_ni
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ni.hMoI<- function(vars, Xpar) {with(Xpar,{
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
F_prevalence.hMoI<- function(vars, Xpar) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_lm.hMoI<- function(vars, Xpar) {
  pr = with(vars, 1-exp(-m2))
  return(pr)
}

#' @title Compute the prevalence of infection by RDT
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_rdt.hMoI<- function(vars, Xpar) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}

#' @title Compute the prevalence of infection by PCR
#' @description Implements [F_prevalence] for the hMoI model.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_pcr.hMoI<- function(vars, Xpar) {
  pr = with(vars, 1-exp(-m1))
  return(pr)
}
