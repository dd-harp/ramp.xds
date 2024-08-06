
#' @title Set up run-time time step support for `dts` models
#' @description Set the time steps for various discrete time models
#' @param pars an `xds` object
#' @param Xday is the run-time time step for X component (in days): integer or 1/integer
#' @param MYZday is the run-time time step for MYZ component (in days): integer or 1/integer
#' @param Lday is the run-time time step for L component (in days): integer or 1/integer
#' @param Lname is the S3 class of the L model
#' @return the modified `xds` object
#' @export
make_runtime = function(pars, Xday, MYZday, Lday, Lname){
  UseMethod("make_runtime", pars$xds)
}

#' @title Set up run-time time step support for `xde` models
#' @description Continuous time models don't use run-time support,
#' so this returns the `xds` object without modification
#' @inheritParams make_runtime
#' @return the unmodified `xds` object
#' @export
make_runtime.xde= function(pars, Xday, MYZday, Lday, Lname){
  pars$runtime = list()
  return(pars)
}

#' @title Set up run-time time step support for `dts` models
#' @description Set the time steps for various discrete time models
#' @inheritParams make_runtime
#' @return the unmodified `xds` object
#' @export
make_runtime.dts = function(pars, Xday, MYZday, Lday, Lname){
  runtime = list()
  runtime$Dday = set_Dday(Xday, MYZday, Lday, Lname)
  runtime$Lday = Lday
  runtime$MYZday = MYZday
  runtime$Xday = Xday
  pars$runtime = runtime
  return(pars)
}


#' @title Set up a model for dts_diffeqn
#' @param Xday is the run-time time step for X component (in days): integer or 1/integer
#' @param MYZday is the run-time time step for MYZ component (in days): integer or 1/integer
#' @param Lday is the run-time time step for L component (in days): integer or 1/integer
#' @param Lname is the S3 class of the L model
#' @return a [list]
#' @export
set_Dday = function(Xday, MYZday, Lday, Lname="trace"){
  if(Lname == "trace") Lday = min(Xday, MYZday)
  mnn = min(Xday, MYZday, Lday)
  mxx = max(Xday, MYZday, Lday)

  if(mnn<1){
    Xm = ifelse(Xday <=1, 1/Xday, 1)
    Lm = ifelse(Lday <=1, 1/Lday, 1)
    MYZm = ifelse(MYZday <=1, 1/MYZday, 1)
    return(1/DescTools::LCM(Xm, Lm, MYZm))
  } else if(mnn==1){
    return(1)
  } else {
    return(DescTools::LCM(Xday, Lday, MYZday))
  }
}


#' @title A run-time switch function for mismatched dynamical component run-times
#' @description Determine whether to update the variables at time t
#' @param t current simulation time
#' @param Dday the run-time time step for the simulation
#' @param xday the run-time time step for a component
#' @return [logical] TRUE or FALSE
#' @export
runt = function(t, Dday, xday){
  t1 = round(t/Dday)
  t2 = max(round(xday/Dday), 1)
  t1 %% t2 < 1e-2
}


