#' @title The trivial function
#' @description A function that returns 0
#' @param t an arbitrary input
#' @param V an arbitrary input
#' @return a vector of ones of length x
#' @export
Zero_tV3 = function(t, V=list()){return(0*t)}


#' @title The trivial function
#' @description A function that returns 0
#' @param t an arbitrary input
#' @param V an arbitrary input
#' @return a vector of ones of length x
#' @export
Zero_tV = function(t, V=list()){return(0*t)}

#' @title The trivial function
#' @description A function that returns 0
#' @param t an arbitrary input
#' @param V an arbitrary input
#' @return a vector of ones of length x
#' @export
One_tV= function(t, V=list()){return(0*t+1)}

#' @title Get Variables 
#' 
#' @description A utility to write functions 
#' that respond to state variables or exogenous
#' variables 
#' 
#' @note The method dispatches on `class(func)`
#' 
#' @param t current simulation time
#' @param y variables 
#' @param func a function that dispatches [get_variables] and pulls variables it needs 
#' @param xds_obj an **`xds`** model object
#' 
#' @return a vector of variables 
#' @export
get_variables = function(t, y, func, xds_obj){
  UseMethod("get_variables", func) 
}

#' @title Get Variables 
#' 
#' @description The null case for [get_variables]
#' 
#' @inheritParams get_variables 
#' 
#' @return a set of variables 
#' @export
get_variables.na = function(t, y, func, xds_obj){
  return(numeric(0)) 
}

#' @title Exponential Function 
#' 
#' @description An exponential 
#' function.  
#' 
#' @param d an arbitrary input
#' 
#' @return a [numeric] vector of length d
#' 
#' @export
F_exp = function(d){return(exp(-d))}

#' @title Make a Function
#' @description Build a function of time for trace functions
#' exogenous forcing by weather, vector control, or mass
#' health interventions
#' @param opts a named list
#' @return a function
#' @export
make_function = function(opts){
  UseMethod("make_function", opts)
}

#' @title The trivial function
#' @description A function that returns 0
#' @param t an arbitrary input
#' @return a vector of ones of length x
#' @export
F_zero = function(t){return(0*t)}

#' @title parameters for make_function
#' @description Return an object to configure
#' a function [make_function.zero]
#' @return a function to return  
#' @seealso [make_function.zero]
#' @export
makepar_F_zero = function(){
  pars <- list()
  class(pars) <- "zero"
  return(pars)
}

#' @title Make a Function that is the sum of Two other Functions
#' @description Return [F_zero]
#' @inheritParams make_function
#' @return a function that is the sum of two other functions
#' @export
make_function.zero = function(opts){
  return(F_zero)
}

#' @title The trivial function
#' @description A function that returns 1
#' @param t an arbitrary input
#' @return a vector of ones of length x
#' @export
F_one = function(t){return(0*t+1)}

#' @title parameters for make_function
#' @description Return an object to return [F_one]
#' @return a function for seasonality
#' @seealso [make_function.one]
#' @export
makepar_F_one = function(){
  pars <- list()
  class(pars) <- "one"
  return(pars)
}

#' @title Make a Function 
#' @description Build a function that is the sum of two
#' other functions.
#' @inheritParams make_function
#' @return a function that is the sum of two other functions
#' @export
make_function.one = function(opts){
  return(F_one)
}

#' @title Make a Function that is the sum of Two other Functions
#' @description Build a function that is the sum of two
#' other functions.
#' @inheritParams make_function
#' @keywords internal
#' @return a function that is the sum of two other functions
#' @export
make_function.list = function(opts){
  return(F_one)
}


#' @title parameters for make_function
#' @description Return an object to return a value 
#' @param val the value to return
#' @return a function for seasonality
#' @seealso [make_function.val]
#' @export
makepar_F_val = function(val){
  pars <- list()
  class(pars) <- "val"
  pars$val <- val 
  return(pars)
}

#' @title Make a Function that is the sum of Two other Functions
#' @description Build a function that returns a constant value 
#' @inheritParams make_function
#' @return a function that returns a constant value 
#' @export
make_function.val = function(opts){
  Fv = function(t){return(0*t + opts$val)}
  return(Fv)
}

#' @title The trivial function
#' @description A function that returns 1
#' @param t an arbitrary input
#' @return a vector of ones of length x
#' @export
F_flat = function(t){return(0*t+1)}


#' @title Make a Sine-based Seasonality Function
#' @description Return a seasonal pattern \eqn{S(t)}, a function of the form
#' \deqn{S(t) = c \left(1+\epsilon + \sin\left(\frac{2 \pi (t-\tau)}{365}\right)\right)^p}
#' where \eqn{c} is a normalizing constant, and
#' + \eqn{\epsilon \geq 0} or `bottom`
#' + \eqn{\tau} or `phase`
#' + \eqn{p} or `pw`
#'
#' The algorithm sets the constant \eqn{c} or `norm`
#' such that \deqn{\int_0^{365} S(t) dt=c} where the default is `norm=365.`
#' @inheritParams make_function
#' @importFrom stats integrate
#' @seealso [makepar_F_sin]
#' @return a function for seasonality
#' @export
make_function.sin = function(opts){
  opts$normit = with(opts, rep(norm, N))
  for(i in 1:opts$N){
    F1 = with(opts,function(t){(1+abs(bottom[i])+sin(2*pi*(t)/365))^pw[i]})
    over_year <- integrate(F1, 0, 365)$val
    opts$normit[i] <- opts$normit[i]/over_year
  }
  F2 = with(opts,function(t){(1+abs(bottom) + sin(2*pi*(t-phase+91)/365))^pw*normit})
  F3 = function(t){if(length(t) == 1) return(F2(t)) else return(sapply(t, F2))}
  return(F3)
}

#' @title parameters for make_function
#' @description Return an object to configure
#' a function [make_function.sin]
#' @param phase the phase for a seasonal function
#' @param bottom shape parameter
#' @param pw shape parameter
#' @param norm the normalization period
#' @param N the length of the vector to return
#' @return a function for seasonality
#' @seealso [make_function.sin]
#' @export
makepar_F_sin = function(phase=0, bottom=0, pw=1, norm=365, N=1){
  pars <- list()
  class(pars) <- "sin"
  pars$phase = checkIt(phase, N)
  pars$bottom = abs(checkIt(bottom, N))
  pars$pw = abs(checkIt(pw, N))
  pars$norm = norm
  pars$N = N
  return(pars)
}

#' @title Make a type2 function for age
#' @description Return an age pattern \eqn{\omega(a)}, a function of the form
#' \deqn{\omega(a) = \frac{A(a+\tau)}{B+a+\tau}}
#' where \eqn{\tau} is a shift so that \eqn{\omega(0) > 0}
#' and \eqn{A} and \eqn{B} are shape parameters
#'
#' @inheritParams make_function
#' @importFrom stats integrate
#' @seealso [makepar_F_type2]
#' @return a function for seasonality
#' @export
make_function.type2 = function(opts){with(opts,{
  F = function(a){
    A*(a + shift)/(B*365+a+shift)
  }
  return(F)
})}

#' @title parameters for make_function
#' @description Return an object to configure
#' a function [make_function.type2]
#' @param shift a shift to set F(0)>0
#' @param A asymptote 
#' @param B slope 
#' @param N the length of the vector to return
#' @return a type 2 functional response 
#' @seealso [make_function.type2]
#' @export
makepar_F_type2 = function(shift=30, A=1.8, B=5, N=1){
  pars <- list()
  class(pars) <- "type2"
  pars$shift = checkIt(shift, N)
  pars$A = abs(checkIt(A, N))
  pars$B = abs(checkIt(B, N))
  pars$N = N 
  return(pars)
}

#' @title Make a Sigmoidal Function
#' @description Build a function to model a
#' forced seasonal signal. The shape parameters determine
#' the timing, frequency, and relative intensity
#' over the season. The function is normalized
#' to have an annual value set by `norm`
#' @inheritParams make_function
#' @importFrom stats integrate
#' @return a function
#' @export
make_function.sigmoid = function(opts){
  opts$normit = rep(1, opts$N)
  for(i in 1:opts$N){
    F1 = with(opts,function(t){1e-15+exp(k[i]*(t-D[i]))/(1+exp(k[i]*(t-D[i])))})
    over_T = ifelse(T>0, integrate(F1, 0, T)$val, 1)
    opts$normit[i] <- opts$normit[i]/over_T
  }
  F2 = with(opts,function(t){1e-15+exp(k*(t-D))/(1+exp(k*(t-D)))})
  F3 = function(t){if(length(t) == 1) return(F2(t)) else return(sapply(t, F2))}
  return(F3)
}

#' @title Make Parameters for a Sigmoidal Function
#' @description Return an object to configure
#' a function [make_function.sigmoid]
#' @param k the rate parameter
#' @param D the half-saturation day
#' @param Tl length of interval to normalize over
#' @param N the length of the vector to return
#' @return a sigmoidal function
#' @export
makepar_F_sigmoid = function(k=1/7, D=100, Tl=0, N=1){
  pars <- list()
  class(pars) <- "sigmoid"
  pars$k = checkIt(k, N)
  pars$D = checkIt(D, N)
  pars$Tl=Tl
  pars$N=N
  return(pars)
}


#' @title Make a Function that is the sum of Two other Functions
#' @description Build a function that is the sum of two
#' other functions.
#' @inheritParams make_function
#' @return a function that is the sum of two other functions
#' @export
make_function.sum = function(opts){
  F1 = make_function(opts$opts1)
  F2 = make_function(opts$opts2)
  F3 = function(t){F1(t)+F2(t)}
  return(F3)
}

#' @title parameters for make_function
#' @description Return an object to configure
#' a function [make_function.sum]
#' @param opts1 options for first function
#' @param opts2 options for the second function
#' @return a function
#' @export
makepar_F_sum = function(opts1, opts2){
  pars <- list()
  class(pars) <- "sum"
  pars$opts1 = opts1
  pars$opts2 = opts2
  return(pars)
}

#' @title Make a Sinusoidal Function
#' @description Build a function that is the
#' product of two other functions
#' @inheritParams make_function
#' @return a function that is the product of two other functions
#' @export
make_function.product = function(opts){
  F1 = make_function(opts$opts1)
  F2 = make_function(opts$opts2)
  F3 = function(t){F1(t)*F2(t)}
  return(F3)
}

#' @title parameters for make_function
#' @description Return an object to configure
#' a function [make_function.product]
#' @param opts1 options for first function
#' @param opts2 options for second function
#' @return a function
#' @export
makepar_F_product = function(opts1, opts2){
  pars <- list()
  class(pars) <- "product"
  pars$opts1 <- opts1
  pars$opts2 <- opts2
  return(pars)
}

#' @title Make a Sinusoidal Function
#' @description Build a function that is the
#' product of two other functions
#' @inheritParams make_function
#' @return a function that is the product of two other functions
#' @export
make_function.nproduct = function(opts){
  F1 = make_function(opts$opts1)
  F2 = make_function(opts$opts2)
  F3 = function(t){1-(1-F1(t))*(1-F2(t))}
  return(F3)
}

#' @title parameters for make_function
#' @description Return an object to configure
#' a function [make_function.product]
#' @param opts1 options for first function
#' @param opts2 options for second function
#' @return a function
#' @export
makepar_F_nproduct = function(opts1, opts2){
  pars <- list()
  class(pars) <- "nproduct"
  pars$opts1 <- opts1
  pars$opts2 <- opts2
  return(pars)
}

#' @title Make a Sharkfin Function
#' @description A sharkfin function is built in steps:
#' 1. take the product of two sigmoidal functions
#'      - the first one rises around day \eqn{D} with rate parameter \eqn{uk}
#'      - the second one decays around day \eqn{D+L} with rate \eqn{-dk}
#' 2. the product is raised a power \eqn{pw}
#' 3. the result is scaled so that the maximum is \eqn{mx}
#' For the default values, the function looks like a shark fin.
#' @inheritParams make_function
#' @return a function
#' @export
make_function.sharkfin = function(opts){
  siggy <- function(t, k=1, D=1){
    x = pmax(pmin(k*(t-D),80),-80); exp(-x)/(1+exp(-x))}
  opts$normit = opts$mx
  for(i in 1:opts$N){
    F1 = with(opts,function(t){((1-siggy(t, uk[i], D[i]))*siggy(t,dk[i],D[i]+L[i]))^pw[i]})
    tt <- with(opts, c(D[i]:(D[i]+L[i])))
    mx <- max(F1(tt))
    opts$normit[i] <- opts$normit[i]/mx
  }
  F2 = with(opts,function(t){normit*((1-siggy(t, uk, D))*siggy(t, dk, D+L))^pw})
  F3 = function(t){if(length(t) == 1) return(F2(t)) else return(sapply(t, F2))}
  return(F3)
}


#' @title Make Parameters for a Sharkfin Function
#' @description Return an object for [make_function.sharkfin]
#' @param D the half-saturation day for scale-up
#' @param L the half-saturation day for decay 
#' @param uk shape parameter for scale-up
#' @param dk shape parameter for decay 
#' @param pw shape parameter, power
#' @param mx a maximum value
#' @param N the length of the vector to return
#' @return a function F_season
#' @export
makepar_F_sharkfin = function(D=100, L=180, uk = 1/7, dk=1/40, pw=1, mx=1, N=1){
  pars <- list()
  class(pars) <- "sharkfin"
  pars$D = checkIt(D, N)
  pars$L = checkIt(L, N)
  pars$uk = checkIt(uk, N)
  pars$dk = checkIt(dk, N)
  pars$pw = checkIt(pw, N)
  pars$mx = checkIt(mx, N)
  pars$N = N
  return(pars)
}

#' @title Make a sharkbite Function
#' @description A sharkbite function is built in steps:
#' 1. take the product of two sigmoidal functions
#'      - the first one rises around day \eqn{D} with rate parameter \eqn{uk}
#'      - the second one decays around day \eqn{D+L} with rate \eqn{-dk}
#' 2. the product is raised a power \eqn{pw}
#' 3. the result is scaled so that the maximum is \eqn{mx}
#' For the default values, the function looks like a shark fin.
#' @inheritParams make_function
#' @return a function
#' @export
make_function.sharkbite = function(opts){
  siggy <- function(t, k=1, D=1){
    x = pmax(pmin(k*(t-D),80),-80); exp(-x)/(1+exp(-x))}
  opts$normit = opts$mx
  for(i in 1:opts$N){
    F1 = with(opts,function(t){((1-siggy(t, uk[i], D[i]))*siggy(t,dk[i],D[i]+L[i]))^pw[i]})
    tt <- with(opts, c(D[i]:(D[i]+L[i])))
    mx <- max(F1(tt))
    opts$normit[i] <- opts$normit[i]/mx
  }
#  F2 = with(opts,function(t){normit*((1-siggy(t, uk, D))*siggy(t, dk, D+L))^pw})
  F2 = with(opts,function(t){1-normit*((1-siggy(t, uk, D))*siggy(t, dk, D+L))^pw})
  F3 = function(t){if(length(t) == 1) return(F2(t)) else return(sapply(t, F2))}
  return(F3)
}


#' @title Make Parameters for a sharkbite Function
#' @description Return an object for [make_function.sharkbite]
#' @param D the half-saturation day for scale-up
#' @param L the half-saturation day for decay 
#' @param uk shape parameter for scale-up
#' @param dk shape parameter for decay 
#' @param pw shape parameter, power
#' @param mx a maximum value
#' @param N the length of the vector to return
#' @return a function F_season
#' @export
makepar_F_sharkbite = function(D=100, L=180, uk = 1/7, dk=1/40, pw=1, mx=1, N=1){
  pars <- list()
  class(pars) <- "sharkbite"
  pars$D = checkIt(D, N)
  pars$L = checkIt(L, N)
  pars$uk = checkIt(uk, N)
  pars$dk = checkIt(dk, N)
  pars$pw = checkIt(pw, N)
  pars$mx = checkIt(mx, N)
  pars$N = N
  return(pars)
}

#' @title Make a spline function
#' @description A spline function passes time points `tt` and
#' associated values `yy` and returns a spline function 
#' @inheritParams make_function
#' @return a function
#' @export
make_function.splinef = function(opts){
  ff <- function(t){
    stats::spline(opts$tt, opts$yy, xout = t)$y
  }
  return(ff)
}

#' @title Make a spline function
#' @description A spline function passes time points `tt` and
#' associated values `yy` and returns a spline function 
#' @inheritParams make_function
#' @return a function
#' @export
make_function.splineX = function(opts){
  ff <- function(t){
    exp(stats::spline(opts$tt, opts$yy, xout = t)$y)
  }
  return(ff)
}

#' @title Make a spline function
#' @description A spline function passes time points `tt` and
#' associated values `yy` and returns a spline function 
#' @inheritParams make_function
#' @return a function
#' @export
make_function.spline2 = function(opts){
  ff <- function(t){
    (stats::spline(opts$tt, opts$yy, xout = t)$y)^2
  }
  return(ff)
}


#' @title Make Parameters for a Spline 
#' @description Return an object for [make_function.splinef] or [make_function.splineX]
#' @param tt the nodes 
#' @param yy the y values 
#' @param X a switch to configure for splinef or splineX  
#' @return parameters to configure the `splinef` or `splineX` case of `make_function`
#' @export
makepar_F_spline = function(tt, yy, X=FALSE){
  pars <- list()
  class(pars) = "splinef"
  if(X==TRUE) class(pars) = "splineX"
  if(X==2) class(pars) = "spline2"
  pars$tt = tt
  pars$yy = yy 
  return(pars)
}

#' Make a Time Series Function 
#'
#' @param options configurable options 
#' @param N the length of the return value 
#' @param scale scale parameter, usually the average 
#' @param season_par seasonality function parameters 
#' @param trend_par trend function parameters 
#'
#' @returns a function
#' @export
make_ts_function = function(options=list(), 
                            N=1,
                            scale=1, 
                            season_par = list(),
                            trend_par = list()){
  with(options,{
    scale = checkIt(scale, N)
    if(length(season_par) == 0){
      F_season = F_one
    } else {
      season_par$N = N
      F_season <- make_function(season_par)
    } 
    if(length(trend_par) == 0){
      F_trend = F_one
    } else {
      trend_par$N = N
      F_trend <- make_function(trend_par)
    }
    return(function(t, V=list()){scale*F_season(t)*F_trend(t)})
})}
