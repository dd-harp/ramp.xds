
#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @param MYZpar an adult mosquito `xds` model object
#' @param runtime the model `runtime` parameters
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs_RM = function(MYZpar, runtime){
  UseMethod("MYZ_rates2probs", MYZpar$effect_sizes)
}

#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @inheritParams MYZ_rates2probs
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs_RM.unmodified = function(MYZpar, runtime){
  return(MYZpar)
}

#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @inheritParams MYZ_rates2probs
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs_RM.modified = function(MYZpar, runtime){
  with(runtime,{
    with(MYZpar,{
      MYZpar$p <- exp(-g*MYZday*Dday)
      MYZpar$ff <- exp(-f*MYZday*Dday)
      MYZpar$ssigma <- exp(-sigma*MYZday*Dday)
      MYZpar$nnu <- exp(-nu*MYZday*Dday)
      return(MYZpar)
    })})}
