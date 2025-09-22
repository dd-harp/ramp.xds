# Functions to compute adult mosquito bionomic parameters:
# f, g, q, sigma, mu, nu 

#' @title Setup Blood Feeding Bionomic Object 
#' 
#' @description Set up an object
#' to compute dynamic blood feeding rates
#' 
#' @param f the blood feeding rate 
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_f_obj = function(f, MY_obj){
  MY_obj$f = f
  MY_obj$f_t = f
  MY_obj$es_f = 1
  MY_obj$f_obj <- list()  
  class(MY_obj$f_obj) <- "static" 
  MY_obj$f_obj$f <- f 
  return(MY_obj)
}

#' @title Compute the blood feeding rate, f
#' 
#' @description  It should
#' set the values of the bionomic parameters to baseline values
#' 
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#' 
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return a [numeric] vector of length `nPatches`
#' 
#' @export
F_feeding_rate = function(t, xds_obj, s) {
  UseMethod("F_feeding_rate", xds_obj$MY_obj[[s]]$f_obj)
}

#' @title Constant baseline blood feeding rate
#' 
#' @description Implements [F_feeding_rate] for a static model
#' 
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#' 
#' @inheritParams F_feeding_rate
#' 
#' @return \eqn{f}, the baseline blood feeding rate 
#' @export
F_feeding_rate.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$f_obj$f)
}


#' @title Setup a Human Fraction Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{q} 
#' 
#' @param q the human fraction 
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_q_obj = function(q, MY_obj){
  MY_obj$q = q
  MY_obj$q_t = q
  MY_obj$es_q = 1
  MY_obj$q_obj <- list()  
  class(MY_obj$q_obj) <- "static" 
  MY_obj$q_obj$q <- q 
  return(MY_obj)
}

#' @title Compute the blood qeeding rate, q
#' 
#' @description This method dispatches on the type of `q_obj`. It should
#' set the values oq the bionomic parameters to baseline values
#' 
#' @inheritParams F_feeding_rate
#' 
#' @return a [numeric] vector oq length `nPatches`
#' 
#' @export
F_human_frac = function(t, xds_obj, s) {
  UseMethod("F_human_frac", xds_obj$MY_obj[[s]]$q_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_human_frac] for a static model
#' @inheritParams F_feeding_rate
#' @return \eqn{q}, the baseline human fraction 
#' @export
F_human_frac.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$q_obj$q)
}

#' @title Setup a Mosquito Mortality Bionomic Object
#' 
#' @description Set up an object to return a 
#' constant baseline mosquito mortality rate, \eqn{g} 
#' 
#' @param g the mosquito mortality rate 
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_g_obj = function(g, MY_obj){
  MY_obj$g = g
  MY_obj$g_t = g
  MY_obj$es_g = 1
  MY_obj$g_obj <- list()  
  class(MY_obj$g_obj) <- "static" 
  MY_obj$g_obj$g <- g 
  return(MY_obj)
}

#' @title Compute the blood geeding rate, g
#' 
#' @description This method dispatches on the type of `g_obj`. It should
#' set the values og the bionomic parameters to baseline values
#' 
#' @inheritParams F_feeding_rate
#' 
#' @return a [numeric] vector og length `nPatches`
#' 
#' @export
F_mozy_mort = function(t, xds_obj, s) {
  UseMethod("F_mozy_mort", xds_obj$MY_obj[[s]]$g_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_mozy_mort] for a static model
#' @inheritParams F_mozy_mort
#' @return \eqn{g}, the baseline human fraction 
#' @export
F_mozy_mort.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$g_obj$g)
}

#' @title Setup a Patch Emigration Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{sigma} 
#' 
#' @param sigma the mosquito patch emigration rate
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_sigma_obj = function(sigma, MY_obj){
  MY_obj$sigma = sigma
  MY_obj$sigma_t = sigma
  MY_obj$es_sigma = 1
  MY_obj$sigma_obj <- list()  
  class(MY_obj$sigma_obj) <- "static" 
  MY_obj$sigma_obj$sigma <- sigma 
  return(MY_obj)
}

#' @title Compute the Mosquito Patch Emigration Rate 
#' 
#' @description This method dispatches on the type of `sigma_obj`. It should
#' set the values the patch emigration rate, \eqn{\sigma} 
#' 
#' @inheritParams F_feeding_rate
#' 
#' @return a [numeric] vector osigma length `nPatches`
#' 
#' @export
F_emigrate = function(t, xds_obj, s){
  UseMethod("F_emigrate", xds_obj$MY_obj[[s]]$sigma_obj)
}

#' @title Static model patch emigration 
#' 
#' @description Implements [F_emigrate] for a static model
#' 
#' @inheritParams F_emigrate
#' 
#' @return \eqn{sigma}, the patch emigration rate 
#' @export
F_emigrate.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$sigma_obj$sigma)
}

#' @title Setup a Dispersal Loss Bionomic Object 
#' 
#' @description Set up an object
#' to compute the dispersal loss fraction, \eqn{\mu} 
#' 
#' @param mu the emigration loss fraction 
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_mu_obj = function(mu, MY_obj){
  MY_obj$mu = mu
  MY_obj$mu_t = mu
  MY_obj$es_mu = 1
  MY_obj$mu_obj <- list()  
  class(MY_obj$mu_obj) <- "static" 
  MY_obj$mu_obj$mu <- mu 
  return(MY_obj)
}

#' @title Compute the emigration loss fraction 
#' 
#' @description This method dispatches on the type of `mu_obj`. It should
#' set the values omu the bionomic parameters to baseline values
#' 
#' @inheritParams F_feeding_rate
#' 
#' @return a [numeric] vector omu length `nPatches`
#' 
#' @export
F_dispersal_loss <- function(t, xds_obj, s){
  UseMethod("F_dispersal_loss", xds_obj$MY_obj[[s]]$mu_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_dispersal_loss] for a static model
#' @inheritParams F_dispersal_loss
#' @return \eqn{mu}, the baseline human fraction 
#' @export
F_dispersal_loss.static <- function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$mu_obj$mu)
}


#' @title Setup Laying Rate Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{nu} 
#' 
#' @param nu the egg laying rate (# batches, per mosquito, per day)
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_nu_obj = function(nu, MY_obj){
  MY_obj$nu = nu
  MY_obj$nu_t = nu
  MY_obj$es_nu = 1
  MY_obj$nu_obj <- list()  
  class(MY_obj$nu_obj) <- "static" 
  MY_obj$nu_obj$nu <- nu 
  return(MY_obj)
}

#' @title Compute the Mosquito Patch Emigration Rate 
#' 
#' @description This method dispatches on the type of `nu_obj`. It should
#' set the values the patch emigration rate, \eqn{\nu} 
#' 
#' @inheritParams F_feeding_rate
#' 
#' @return a [numeric] vector onu length `nPatches`
#' 
#' @export
F_batch_rate = function(t, xds_obj, s){
  UseMethod("F_batch_rate", xds_obj$MY_obj[[s]]$nu_obj)
}

#' @title Static model patch emigration 
#' 
#' @description Implements [F_batch_rate] for a static model
#' 
#' @inheritParams F_batch_rate
#' 
#' @return \eqn{nu}, the patch emigration rate 
#' @export
F_batch_rate.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$nu_obj$nu)
}


#' @title Setup Blood Feeding Bionomic Object 
#' 
#' @description Set up an object
#' to compute dynamic blood feeding rates
#' as a type 2 functional response to 
#' availability of blood hosts, \deqn{f=F_feeding_rate(B) = \frac{f_x s_f B}{1+s_f B}}
#' 
#' @param MY_obj an **`MY`** model object 
#' @param options a list of options 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_f_obj_B2 = function(MY_obj, options=list(), fx=0.35, sf=1){
  with(options,{
    MY_obj$f = fx
    MY_obj$f_t = fx
    MY_obj$es_f = 1
    MY_obj$f_obj <- list()  
    class(MY_obj$f_obj) <- "B2" 
    MY_obj$f_obj$fx <- fx
    MY_obj$f_obj$sf <- sf 
    return(MY_obj)
})}


#' @title Type 2 functional response for the blood feeding rate
#' @description Implements [F_feeding_rate] for a static model
#' @inheritParams F_feeding_rate
#' @return a [numeric] vector of length `nPatches`
#' @export
F_feeding_rate.B2 = function(t, xds_obj, s){
  B <- xds_obj$XY_interface$B 
  return(with(xds_obj$MY_obj[[s]]$f_obj, fx*sf*B/(1+sf*B)))
}

#' @title Setup Blood Feeding Bionomic Object 
#' 
#' @description Set up an object
#' to compute dynamic blood feeding rates
#' as a type 2 functional response to 
#' availability of blood hosts, \eqn{B}
#' 
#' @param MY_obj an **`MY`** model object 
#' @param options an **`MY`** model object 
#' @param sigma_x a scaling parameter
#' @param sigma_B maximum rate, blood searching
#' @param sigma_Q maximum rate, habitat searching 
#' @param sigma_S maximum rate, sugar searching
#' @param sB shape parameter, blood searching
#' @param sQ shape parameter, habitat searching
#' @param sS shape parameter, sugar searching 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_sigma_obj_BQS = function(MY_obj, options=list(), 
                               sigma_x = 1, sigma_B=3, 
                               sigma_Q=3, sigma_S=3, 
                               sB=1, sQ=1, sS=1){
  with(options,{
    MY_obj$sigma = sigma_x 
    MY_obj$sigma_t = sigma_x 
    MY_obj$es_sigma = 1
    MY_obj$sigma_obj <- list()  
    class(MY_obj$sigma_obj) <- "BQS" 
    MY_obj$sigma_obj$sigma_x
    MY_obj$sigma_obj$sigma_B
    MY_obj$sigma_obj$sigma_Q
    MY_obj$sigma_obj$sigma_S
    MY_obj$sigma_obj$sB
    MY_obj$sigma_obj$sQ
    MY_obj$sigma_obj$sS
  })
  return(MY_obj)
}

#' @title Model for mosquito emigration based on resource availability
#' @description Implements [F_emigrate] for a static model
#' @inheritParams F_emigrate
#' @return a [numeric] vector of length `nPatches`
#' @export
F_emigrate.BQS = function(t, xds_obj, s){
  B <- xds_obj$XY_interface$B 
  Q <- xds_obj$ML_interface$Q 
  S <- xds_obj$ML_interface$sugar
  with(xds_obj$MY_obj[[s]]$sigma_obj,
     return(sigma_x*(sigma_B/(1+sB*B) + sigma_Q/(1+sQ*Q) + sigma_S/(1+sS*S)))
)}


#' @title Setup Blood Feeding Bionomic Object 
#' 
#' @description Set up an object
#' to compute dynamic blood feeding rates
#' as a type 2 functional response to 
#' availability of blood hosts, \eqn{B}
#' 
#' @param MY_obj an **`MY`** model object 
#' @param options a list of options 
#' @param fx the maximum blood feeding rate
#' @param sf a shape parameter 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_f_obj_B2 = function(MY_obj, options=list(), fx=0.35, sf=1){
  with(options,{
    MY_obj$f = fx
    MY_obj$f_t = fx
    MY_obj$es_f = 1
    MY_obj$f_obj <- list()  
    class(MY_obj$f_obj) <- "B2" 
    MY_obj$f_obj$fx <- fx
    MY_obj$f_obj$sf <- sf 
    return(MY_obj)
  })}

#' @title Setup Blood Feeding Bionomic Object 
#' 
#' @description Set up an object
#' to compute dynamic egg laying rates
#' as a type 2 functional response to 
#' available habitat, \eqn{Q}
#' 
#' @param MY_obj an **`MY`** model object 
#' @param options a list of options 
#' @param nux the maximum egg laying rate 
#' @param snu a shape parameter  
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_nu_obj_Q2 = function(MY_obj, options=list(), nux=0.35, snu=1){
  with(options,{
    MY_obj$nu = nux
    MY_obj$nu_t = nux
    MY_obj$es_nu = 1
    MY_obj$nu_obj <- list()  
    class(MY_obj$nu_obj) <- "Q2" 
    MY_obj$nu_obj$nux <- nux
    MY_obj$nu_obj$snu <- snu 
    return(MY_obj)
  })}


#' @title Type 2 Functional Response for Egg Laying  
#' 
#' @description Implements [F_batch_rate] for a static model
#' 
#' @inheritParams F_feeding_rate
#' @return a [numeric] vector of length `nPatches`
#' @export
F_batch_rate.Q2 = function(t, xds_obj, s){
  Q <- xds_obj$ML_interface$Q[[s]]
  return(with(xds_obj$MY_obj[[s]]$nu_obj, 
              nux*snu*Q/(1+snu*Q)))
}




