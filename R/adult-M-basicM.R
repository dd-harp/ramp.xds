# specialized methods for a basic adult mosquito model

#' @title The **MY** Module Skill Set 
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
skill_set_MY.basicM = function(MYname){
  return(list())
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_MY.basicM = function(xds_obj, s){
  return(xds_obj)
}


#' @title **MY** Component Derivatives for `basicM`
#' @description Implements [dMYdt] for the basicM xde ODE model.
#' @details \deqn{\begin{array}{rl}dM/dt &= \Lambda(t) - \Omega \cdot M \\ dP/dt &= f(M-P) - \Omega\cdot P\end{array}}
#' @inheritParams dMYdt
#' @return a [numeric] vector
#' @export
dMYdt.basicM <- function(t, y, xds_obj, s){

  Lambda = xds_obj$terms$Lambda[[s]]

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      dMdt <- Lambda - (Omega %*% M)
      dPdt <- f*(M - P) - (Omega %*% P)
      
      return(c(dMdt, dPdt))
    })
  })
}


#' @title Set mosquito bionomics to baseline
#' @description Implements [MBaseline] for models with no forcing on the baseline
#' @inheritParams MBaseline
#' @return the model as a [list]
#' @export
MBaseline.basicM <- function(t, y, xds_obj, s){with(xds_obj$MY_obj[[s]],{
  # Baseline parameters
  xds_obj$MY_obj[[s]]$f_t      <- F_feeding_rate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$q_t      <- F_human_frac(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$g_t      <- F_mozy_mort(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$sigma_t  <- F_emigrate(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$mu       <- F_dispersal_loss(t, xds_obj, s) 
  xds_obj$MY_obj[[s]]$nu       <- F_batch_rate(t, xds_obj, s) 
  xds_obj                     <- F_K_matrix(t, xds_obj, s) 
  
  # Reset Effect Sizes
  xds_obj$MY_obj[[s]]$es_f     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_q     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_g     <- rep(1, xds_obj$nPatches)
  xds_obj$MY_obj[[s]]$es_sigma <- rep(1, xds_obj$nPatches)
  
  return(xds_obj)
})}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing on the baseline
#' @inheritParams MBionomics
#' @return the model as a [list]
#' @export
MBionomics.basicM <- function(t, y, xds_obj, s) {with(xds_obj$MY_obj[[s]],{
  xds_obj$MY_obj[[s]]$f     <- es_f*f_t
  xds_obj$MY_obj[[s]]$q     <- es_q*q_t
  xds_obj$MY_obj[[s]]$g     <- es_g*g_t
  xds_obj$MY_obj[[s]]$sigma <- es_sigma*sigma_t
  xds_obj$MY_obj[[s]]$mu    <- es_mu*mu_t
  
  xds_obj$MY_obj[[s]]$Omega <- make_Omega(xds_obj, s) 
  
  return(xds_obj)
})}


#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MY_obj`
#' @inheritParams steady_state_M
#' @return none
#' @export
steady_state_M.basicM = function(Lambda, MY_obj){with(MY_obj,{
  Omega_inv <- solve(Omega)
  M_eq  <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  return(c(M=M_eq, P=P_eq))
})}

# specialized methods for the adult mosquito basicM model

#' @title Derivatives for adult mosquitoes
#' @description Implements [Update_MYt] for the basicM model.
#' @inheritParams Update_MYt
#' @return a [numeric] vector
#' @export
Update_MYt.basicM <- function(t, y, xds_obj, s) {
  Lambda = xds_obj$Lambda[[s]]*xds_obj$Dday

  with(get_MY_vars(y, xds_obj, s),{
    with(xds_obj$MY_obj[[s]],{

      Mt <- Lambda + Omega %*% M
      Pt <- f*(M-P) + Omega %*% P

      return(c(Mt, Pt))
    })
  })
}



#' @title Setup MY_obj for the basicM xde model
#' @description Implements [setup_MY_obj] for the basicM xde model
#' @inheritParams setup_MY_obj
#' @return a [list] vector
#' @export
setup_MY_obj.basicM = function(MYname, xds_obj, s, options=list()){
  xds_obj$MY_obj[[s]]=make_M_obj_basicM(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Make parameters for GeRM ODE adult mosquito model
#' @param nPatches is the number of patches, an integer
#' @param options a named list: named values overwrite defaults
#' @param g mosquito mortality rate
#' @param sigma emigration rate
#' @param mu emigration loss
#' @param f feeding rate
#' @param q human blood fraction
#' @param nu oviposition rate, per mosquito
#' @param eggsPerBatch eggs laid per oviposition
#' @return a [list]
#' @export
make_M_obj_basicM = function(nPatches, options=list(), 
                            g=1/12,  sigma=1/8,  mu=0,
                            f=0.3,  q=0.95,
                            nu=1,  eggsPerBatch=60){

  with(options,{
    MY_obj <- list()
   
    class(MY_obj) = "basicM" 
    MY_obj$nPatches <- nPatches
    
    MY_obj <- setup_f_obj(checkIt(f, nPatches), MY_obj)
    MY_obj <- setup_q_obj(checkIt(q, nPatches), MY_obj) 
    MY_obj <- setup_g_obj(checkIt(g, nPatches), MY_obj) 
    MY_obj <- setup_mu_obj(checkIt(mu, nPatches), MY_obj) 
    MY_obj <- setup_nu_obj(checkIt(nu, nPatches), MY_obj) 
    MY_obj <- setup_sigma_obj(checkIt(sigma, nPatches), MY_obj) 
    
    MY_obj <- setup_K_obj(nPatches, MY_obj) 
   
    
    MY_obj$K_matrix <- diag(nPatches)
    
    MY_obj$Omega <- diag(g, nPatches)
    MY_obj$nPatches <- nPatches
    MY_obj$eggsPerBatch <- eggsPerBatch
    
    return(MY_obj)
})}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MY_pars.basicM <- function(xds_obj, s=1) {
  with(xds_obj$MY_obj[[s]], list(
    f=f_t, q=q_t, g=g_t, sigma=sigma_t, mu=mu_t,
    nu=nu_t, eggsPerBatch=eggsPerBatch, K_matrix=K_matrix
  ))
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @export
change_MY_pars.basicM <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_obj$f <- f
    xds_obj$MY_obj[[s]]$q_obj$q <- q
    xds_obj$MY_obj[[s]]$g_obj$g <- g
    xds_obj$MY_obj[[s]]$sigma_obj$sigma <- sigma
    xds_obj$MY_obj[[s]]$mu_obj$mu <- mu
    xds_obj$MY_obj[[s]]$nu_obj$nu <- nu
    xds_obj$MY_obj[[s]]$eggsPerBatch <- eggsPerBatch
    return(xds_obj)
  }))}

#' @title The net blood feeding rate of the infective mosquito population in a patch
#' @description Implements [F_fqZ] for the basicM xde model.
#' @inheritParams F_fqZ
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ.basicM <- function(t, y, xds_obj, s) {
  0*y[xds_obj$ix$MY[[s]]$M_ix]
}

#' @title The net blood feeding rate of the mosquito population in a patch
#' @description Implements [F_fqM] for the basicM xde model.
#' @inheritParams F_fqM
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM.basicM <- function(t, y, xds_obj, s) {
  f = get_f(xds_obj, s)
  q = get_q(xds_obj, s)
  M = y[xds_obj$ix$MY[[s]]$M_ix]
  return(f*q*M)
}

#' @title Number of eggs laid by adult mosquitoes
#' @description Implements [F_eggs] for the basic ecology model
#' @inheritParams F_eggs
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs.basicM <- function(t, y, xds_obj, s) {
  M <- y[xds_obj$ix$MY[[s]]$M_ix]
  with(xds_obj$MY_obj[[s]], {
    return(M*nu*eggsPerBatch)
  })
}

#' @title Setup the basicM model
#' @description Implements [setup_MY_inits] for the basicM model
#' @inheritParams setup_MY_inits
#' @return a [list] vector
#' @export
setup_MY_inits.basicM = function(xds_obj, s, options){
  xds_obj$MY_obj[[s]]$inits = make_MY_inits_basicM(xds_obj$nPatches, options)
  return(xds_obj)
}


#' @title Set new MY parameter values
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_inits
#' @return an `xds` object
#' @export
change_MY_inits.basicM <- function(xds_obj, s=1, options=list()) {
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MYinits[[s]]$M = M
    xds_obj$MYinits[[s]]$P = P
    return(xds_obj)
  }))}

#' @title Make inits for basicM adult mosquito model
#' @param nPatches the number of patches in the model
#' @param options a [list] of values that overwrites the defaults
#' @param M total mosquito density at each patch
#' @param P total parous mosquito density at each patch
#' @return none
#' @export
make_MY_inits_basicM = function(nPatches, options, M=5, P=1){
  with(options,{
    M = checkIt(M, nPatches)
    P = checkIt(P, nPatches)
    return(list(M=M, P=P))
  })}


#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams get_MY_vars
#' @return a [list]
#' @export
get_MY_vars.basicM <- function(y, xds_obj, s){
  with(xds_obj$ix$MY[[s]],
       return(list(
         M = y[M_ix],
         P = y[P_ix]
       )))
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @inheritParams change_MY_pars
#' @return an **`xds`** object
#' @export
change_MY_pars.basicM <- function(xds_obj, s=1, options=list()) {
  nHabitats <- xds_obj$nHabitats
  with(xds_obj$MY_obj[[s]], with(options,{
    xds_obj$MY_obj[[s]]$f_par <- f_par
    xds_obj$MY_obj[[s]]$q_par <- q_par
    xds_obj$MY_obj[[s]]$g_par <- g_par
    xds_obj$MY_obj[[s]]$sigma_par <- sigma_par
    xds_obj$MY_obj[[s]]$mu_par <- mu_par
    xds_obj$MY_obj[[s]]$nu_par <- nu_par
    xds_obj$MY_obj[[s]]$eip_par <- eip_par
    xds_obj$MY_obj[[s]]$K_matrix_par <- K_matrix_par
    xds_obj$MY_obj[[s]]$eggsPerBatch <- eggsPerBatch
    return(xds_obj)
  }))}

#' @title Add indices for adult mosquitoes to parameter list
#' @description Implements [setup_MY_ix] for the basic M model.
#' @inheritParams setup_MY_ix
#' @return none
#' @importFrom utils tail
#' @export
setup_MY_ix.basicM <- function(xds_obj, s) {with(xds_obj,{

  M_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(M_ix, 1)

  P_ix <- seq(from = max_ix+1, length.out = nPatches)
  max_ix <- tail(P_ix, 1)

  xds_obj$max_ix = max_ix

  xds_obj$ix$MY[[s]] = list(M_ix=M_ix, P_ix=P_ix)

  return(xds_obj)
})}

#' @title parse outputs for basicM
#' @description Implements [parse_MY_orbits] for the basicM model.
#' @inheritParams parse_MY_orbits
#' @return [list]
#' @export
parse_MY_orbits.basicM <- function(outputs, xds_obj, s) {
  with(xds_obj$ix$MY[[s]],{
    M = outputs[,M_ix]
    P = outputs[,P_ix]
    parous = P/M
    return(list(M=M, P=P, parous=parous))
  })}


#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], f)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], q)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], g)
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma.basicM = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]], sigma)
}