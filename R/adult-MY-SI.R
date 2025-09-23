# specialized methods for the adult mosquito SI model

#' @title The **SI** Module Skill Set 
#' 
#' @description The **MY** skill set is a list of 
#' an module's capabilities: 
#' 
#' + `demography` is 
#'
#' @inheritParams skill_set_MY 
#' 
#' @return *MY* module skill set, as a list 
#' 
#' @export
skill_set_MY.SI = function(MYname){
  return(list())
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_MY.SI = function(xds_obj, s){
  return(xds_obj)
}

#' @title Compute Derivatives for **MY** module `SI` 
#' 
#' @description The `SI` model for mosquito infection
#' dynamics has the defined **variable** classes:
#' - \eqn{M} is the density of mosquitoes in each patch;
#' - \eqn{Y} is the density of infected mosquitoes in each patch.
#'
#' The density of infectious mosquitoes in each patch is
#' given by a term (returned by [F_fqZ.SI]):
#' \deqn{Z = e^{-\Omega \tau} \cdot Y}
#'
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#'
#' The **parameters** describing egg laying (see [F_eggs.SI]) are:
#' - \eqn{\nu} is the egg laying rate
#' - \eqn{\xi} is the number of eggs per batch
#'
#'The model demographic **parameters** are:
#' - \eqn{g} is the mortality rate
#' - \eqn{\sigma} is the emigration rate
#' - \eqn{\mu} is the emigration loss rate
#' - \eqn{K} is the mosquito dispersal matrix
#'
#' The four parameters describing mortality and migration are used to construct a
#' demographic matrix:
#' \deqn{\Omega = \mbox{diag}\left(g\right) - \left(\mbox{diag}\left(1-\mu\right) -  K \right) \cdot \mbox{diag}\left(\sigma\right)}
#'
#' The emergence rate of adult mosquitoes, \eqn{\Lambda}, is computed by [F_emerge],
#' and the **derivatives** are given by the equations:
#' \deqn{
#' \begin{array}{rr}
#' dM/dt = & \Lambda - \Omega \cdot M \\
#' dY/dt = & f q \kappa (M-Y) - \Omega \cdot Y
#' \end{array}.
#' }
#'
#' @inheritParams dMYdt
#' @seealso [F_fqZ.SI] and [F_eggs.SI]
#' @return a vector with the derivatives
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


#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the SI model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
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

#' @title Net Blood Feeding by Infectious Mosquitoes - `SI` Mosquito Model
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
#' @export
F_fqZ.SI <- function(t, y, xds_obj, s) {
  Y = y[xds_obj$MY_obj[[s]]$ix$Y_ix]
  fqZ = with(xds_obj$MY_obj[[s]], f*q*(Upsilon %*%Y))
  return(fqZ)
}

#' @title Compute Net Blood Feeding by Mosquitoes for `SI` 
#' @description  The variable \eqn{M} is the density of  mosquitoes.
#' The model blood feeding **parameters** are:
#' - \eqn{f} is the overall blood feeding rate
#' - \eqn{q} is the human fraction for blood feeding
#' The daily HBR for the human / host population strata is \eqn{\beta \cdot fqM}
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.SI <- function(t, y, xds_obj, s){
  M = y[xds_obj$MY_obj[[s]]$ix$M_ix]
  fqM = with(xds_obj$MY_obj[[s]], f*q*M)
  return(fqM)
}

#' @title Compute Component Egg Laying Rates for `SI` 
#' @description The density of adult mosquitoes is \eqn{M}.
#' The **parameters** describing egg laying by adult mosquitoes are:
#' - \eqn{\nu} or `nu` is the egg laying rate
#' - \eqn{\xi} or `eggsPerBatch` is the number of eggs per batch
#'
#' The egg laying rate, per patch, is \deqn{\nu \xi M}
#'
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.SI <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$MY_obj[[s]]$ix$M_ix]
  with(xds_obj$MY_obj[[s]],{
    return(M*nu*eggsPerBatch)
  })
}


#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect SIzes for static models
#' @description
#' When modules are added to compute effect SIzes
#' from baseline parameters, those functions store
#' an effect SIze. The total effect SIze is the
#' product of the effect SIzes for each intervention.
#' SInce coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBaseline.SI <- function(t, y, xds_obj, s) {
  
  xds_obj$MY_obj[[s]]$f_t      <- F_feeding_rate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$q_t      <- F_human_frac(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$g_t      <- F_mozy_mort(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$sigma_t  <- F_emigrate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$mu       <- F_dispersal_loss(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$nu       <- F_batch_rate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$eip      <- F_eip(t, xds_obj, s) 
  xds_obj                      <- F_K_matrix(t, xds_obj, s) 
  
  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  return(xds_obj)
}

#' @title Macdonald-style adult mosquito bionomics
#' @description Reset the effect SIzes for static models
#' @description
#' When modules are added to compute effect SIzes
#' from baseline parameters, those functions store
#' an effect SIze. The total effect SIze is the
#' product of the effect SIzes for each intervention.
#' SInce coverage could be changing dynamically, these
#' must be reset each time the derivatives are computed.
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.SI <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]],{
    xds_obj$MY_obj[[s]]$f <- es_f*f_t
    xds_obj$MY_obj[[s]]$q <- es_q*q_t
    xds_obj$MY_obj[[s]]$g <- es_g*g_t
    xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
    xds_obj <- setup_Omega(xds_obj, s)
    return(xds_obj)
  })}



#' @title Setup MY_obj for the SI model
#' @description Implements [setup_MY_obj] for the SI model
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @export
setup_MY_obj.SI = function(MYname, xds_obj, s, options=list()){
  MY_obj <- make_MY_obj_SI(xds_obj$nPatches, options)
  class(MY_obj) <- c("SI", paste("SI_", xds_obj$xds, sep=""))
  xds_obj$MY_obj[[s]] <- MY_obj
  return(xds_obj)
}

#' @title Make parameters for SI ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param options a named list: named values overwrite defaults
#' @param eip the extrinSIc incubation period
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu fraction lost during emigration
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu ovipoSItion rate, per mosquito
#' @param eggsPerBatch eggs laid per ovipoSItion
#' @return a [list]
#' @export
make_MY_obj_SI = function(nPatches, options=list(), eip=12,
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
    MY_obj <- setup_K_obj(nPatches, MY_obj) 

    MY_obj$Omega <- diag(MY_obj$g, nPatches) 
    MY_obj$Upsilon <- with(MY_obj, expm::expm(-Omega*eip))

    return(MY_obj)
  })}


#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the `SI` model.
#' @inheritParams setup_MY_ix
#' @return a [list]
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


#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @export
get_MY_vars.SI <- function(y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$ix,
       return(list(
         M = y[M_ix],
         Y = y[Y_ix]
       )))
}


#' @title parse the output of deSolve and return variables for the `SI` model
#' @description Implements [parse_MY_orbits] for the `SI` model
#' @inheritParams parse_MY_orbits
#' @return a [list]
#' @export
parse_MY_orbits.SI <- function(outputs, xds_obj, s) {
  with(xds_obj$MY_obj[[s]]$ix,{
    M = outputs[,M_ix]
    Y = outputs[,Y_ix]
    Z = Y*0
    y = Y/M
    z = Z/M
    return(list(M=M, Z=Z, Y=Y, y=y, z=z))
})}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MY_pars.SI <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, eip=eip, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, calK=calK
  ))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @export
change_MY_pars.SI <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_t = f
    xds_obj$MY_obj[[s]]$q_t = q
    xds_obj$MY_obj[[s]]$g_t = g
    xds_obj$MY_obj[[s]]$sigma_t = sigma
    xds_obj$MY_obj[[s]]$eip_t = eip
    xds_obj$MY_obj[[s]]$mu_t = mu
    xds_obj$MY_obj[[s]]$nu_t = nu
    xds_obj$MY_obj[[s]]$eggsPerBatch = eggsPerBatch
    return(xds_obj)
  }))}


#' @title Setup initial values for the `SI` model
#' @description Implements [setup_MY_inits] for the `SI` model
#' @inheritParams setup_MY_inits
#' @return a [list]
#' @export
setup_MY_inits.SI = function(xds_obj, s, options=list()){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_SI(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Make inits for `SI` adult mosquito model
#' @param nPatches the number of patches in the model
#' @param options a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param Y infectious mosquito density at each patch
#' @return a [list]
#' @export
make_MY_inits_SI = function(nPatches, options = list(),
                            M=5, Y=1){
  with(options,{
    M = checkIt(M, nPatches)
    Y = checkIt(Y, nPatches)
    return(list(M=M,  Y=Y))
  })
}

#' @title change initial values for the macdonald model
#' @description Implements [change_MY_inits] for the macdonald model
#' 
#' @inheritParams change_MY_inits
#' 
#' @return a [list]
#' @export
change_MY_inits.SI = function(xds_obj, s, options=list()){
  inits =  with(get_MY_inits(xds_obj, s), with(options,{ 
    list(M=M, Y=Y)}))
  xds_obj$MY_obj[[s]]$inits=inits 
  return(xds_obj)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector asSIgned the class "dynamic"
#' @export
get_f.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector asSIgned the class "dynamic"
#' @export
get_q.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector asSIgned the class "dynamic"
#' @export
get_g.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector asSIgned the class "dynamic"
#' @export
get_sigma.SI = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}

#' @title Steady States: MY-SI
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritParams steady_state_MY
#' @return none
#' @export
steady_state_MY.SI_ode = function(Lambda, kappa, xds_obj, s=1){
  with(xds_obj$MY_obj[[s]],{
    Omega_inv <- solve(Omega)
    M_eq <- as.vector(Omega_inv %*% Lambda)
    Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
    return(list(M=M_eq, Y=Y_eq))
})}
