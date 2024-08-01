# specialized methods for the aquatic mosquito basicL competition model

#' @title Make parameters for basicL competition aquatic mosquito model
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param psi maturation rates for each aquatic habitat
#' @param phi density-independent mortality rates for each aquatic habitat
#' @param theta density-dependent mortality terms for each aquatic habitat
#' @return a [list] with Lpar added
#' @export
create_Lpar_basicL = function(nHabitats, Lopts=list(), psi=1/8, phi=1/8, theta=1/100){
  with(Lopts,{
    Lpar = list()
    class(Lpar) <- "basicL"
    Lpar$psi = checkIt(psi, nHabitats)
    Lpar$phi = checkIt(phi, nHabitats)
    Lpar$theta = checkIt(theta, nHabitats)
    return(Lpar)
  })
}

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [dLdt] for the basicL competition model.
#' @inheritParams dLdt
#' @return a [numeric] vector
#' @export
dLdt.basicL <- function(t, y, pars, s) {
  eta <- pars$eggs_laid[[s]]
  with(pars$ix$L[[s]],{
    L <- y[L_ix]
    with(pars$Lpar[[s]], {
      dL = eta - (psi + phi + (theta*L))*L
      return(dL)
    })
  })
}


#' @title Compute the steady state as a function of the egg deposition rate eta
#' @description This method dispatches on the type of `Lpar`.
#' @inheritParams xde_steady_state_L
#' @return none
#' @export
xde_steady_state_L.basicL = function(eta, Lpar){with(Lpar,{
 t1 = (psi+phi)/theta
 t2 = 4*eta/theta
 return((-t1 + sqrt(t1^2 + t2))/2)
})}

# specialized methods for the aquatic mosquito basicL competition model

#' @title Derivatives for aquatic stage mosquitoes
#' @description Implements [Update_Lt] for the basicL competition model.
#' @inheritParams Update_Lt
#' @return a [numeric] vector
#' @export
Update_Lt.basicL <- function(t, y, pars, s) {
  eta <- pars$eggs_laid[[s]]
  with(list_Lvars(y, pars, s),{
    L <- y[L_ix]
    with(pars$Lpar[[s]], {
      Lt = eta + (1-exp(-psi*exp(-xi*L)))*exp(-(phi+theta*L))*L
      return(dL)
    })
  })
}

#' @title Number of newly emerging adults from each larval habitat
#' @description Implements [F_emerge] for the basicL competition model.
#' @inheritParams F_emerge
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge.basicL <- function(t, y, pars, s) {
  L <- y[pars$ix$L[[s]]$L_ix]
  with(pars$Lpar[[s]],{
    return(psi*L)
  })
}

#' @title xde_setup Lpar for the basicL model
#' @description Implements [make_Lpar] for the basicL model
#' @inheritParams make_Lpar
#' @return a [list] vector
#' @export
make_Lpar.basicL = function(Lname, pars, s, Lopts=list()){
  pars$Lpar[[s]] = create_Lpar_basicL(pars$nHabitats, Lopts)
  return(pars)
}


#' @title Reset aquatic parameters to baseline
#' @description Implements [LBionomics] for the RM model
#' @inheritParams LBionomics
#' @return a named [list]
#' @export
LBionomics.basicL <- function(t, y, pars, s) {with(pars$Lpar[[s]],{
  pars$Lpar[[s]]$psi <- psi
  pars$Lpar[[s]]$phi <- phi
  pars$Lpar[[s]]$theta <- theta

  return(pars)
})}



#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`
#' @inheritParams list_Lvars
#' @return a [list]
#' @export
list_Lvars.basicL <- function(y, pars, s){
  with(pars$ix$L[[s]],
       return(list(
         L = y[L_ix]
       )))
}

#' @title Setup the basicL model
#' @description Implements [make_Linits] for the basicL model
#' @inheritParams make_Linits
#' @return a [list]
#' @export
make_Linits.basicL = function(pars, s, Lopts=list()){
  pars$Linits[[s]] = create_Linits_basicL(pars$nHabitats, Lopts)
  return(pars)
}

#' @title Update inits for the basicL aquatic mosquito competition model
#' @inheritParams update_Linits
#' @return none
#' @export
update_Linits.basicL <- function(pars, y0, s) {
  L = y0[pars$ix$L[[s]]$L_ix]
  pars$Linits[[s]] = L
  return(pars)
}

#' @title Return initial values as a vector
#' @description Implements [get_Linits] for the GeRM model.
#' @inheritParams get_Linits
#' @return none
#' @export
get_Linits.basicL <- function(pars, s=1){
  pars$Linits[[s]]
}

#' @title Make inits for basicL competition aquatic mosquito model
#' @param nHabitats the number of habitats in the model
#' @param Lopts a [list] that overwrites default values
#' @param L initial conditions
#' @return a [list] with Linits added
#' @export
create_Linits_basicL = function(nHabitats, Lopts=list(), L=1){with(Lopts,{
  L = checkIt(L, nHabitats)
  return(list(L=L))
})}

#' @title Parse the variable names for the basicL model
#' @description Implements [parse_outputs_L] for basicL competition model.
#' @inheritParams parse_outputs_L
#' @return [list]
#' @export
parse_outputs_L.basicL <- function(outputs, pars, s) {
  time = outputs[,1]
  L = outputs[,pars$ix$L[[s]]$L_ix+1]
  return(list(time=time, L=L))
}

#' @title Add indices for aquatic mosquitoes to parameter list
#' @description Implements [make_indices_L] for the basic M model.
#' @inheritParams make_indices_L
#' @return none
#' @importFrom utils tail
#' @export
make_indices_L.basicL <- function(pars, s) {with(pars,{

  L_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(L_ix, 1)

  pars$max_ix = max_ix

  pars$ix$L[[s]] = list(L_ix=L_ix)

  return(pars)
})}


