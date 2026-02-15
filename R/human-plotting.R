
#' @title Get the EIR 
#'   
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @export
get_EIR = function(xds_obj, i=1){
  get_XH_out(xds_obj, i)$eir
}

#' Plot the EIR *vs.* time
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param lty an integer (or integers) to set the `lty` for plotting
#' @param add a logical: plot axes only if FALSE
#' @param annual if true, plot as an annualized rate
#' 
#' @importFrom graphics plot 
#' 
#' @return eir, invisibly 
#'
#' @export
xds_plot_EIR <- function(xds_obj, i=1, clrs="black", lty=1, add=FALSE, annual=TRUE){
  ylb = ifelse(annual==TRUE, "aEIR", "dEIR")
  if(add==FALSE)
    with(get_XH_out(xds_obj, i),{ 
      fac = ifelse(annual==TRUE, 365, 1)
      plot(time, 0*time, type="n", ylim=range(0, fac*eir),
           xlab = "Time", ylab = ylb)
    })
  eir <- xds_lines_EIR(xds_obj, i, clrs, lty, annual)
  
  return(invisible(eir)) 
}

#' Add lines for the EIR *vs.* time
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param lty an integer (or integers) to set the `lty` for plotting
#' @param annual if true, plot as an annualized rate
#' 
#' @importFrom graphics lines
#'
#' @return eir, invisibly 
#' 
#' @export
xds_lines_EIR <- function(xds_obj, i=1, clrs="black", lty=1, annual=TRUE){
  n = xds_obj$nStrata[i] 
  if(length(clrs) != n) clrs=rep(clrs, n)
  if(length(lty) != n) lty=rep(lty, n)
  with(get_XH_out(xds_obj, i),{
    fac = ifelse(annual==TRUE, 365, 1)
    for(j in 1:n) lines(time, fac*eir[,j], col=clrs[j], lty = lty)
    return(invisible(eir)) 
})}

#' Plot the prevalence / parasite rate (PR) from a model of human infection and immunity
#'
#' @param xds_obj an **`xds`** model object
#' @param method the method used for computing *Pf*PR 
#' @param i the host species index
#' @param clrs a vector of colors
#' @param lty an integer (or integers) that specifies `lty` for plotting
#' @param y01 set ylim = c(0,1) 
#' @param add a logical: plot axes only if FALSE
#' 
#' @return true pr, invisibly 
#' 
#' @export
xds_plot_PR = function(xds_obj, method="true", i=1, 
                       clrs="black", lty=1, y01=FALSE, 
                       add=FALSE){
  if(add==FALSE){
    with(get_PR(xds_obj, method, i),{
      if(y01) {ylm=c(0,1)} else{ylm=range(0, pr)}
      plot(time, 0*time, type = "n", ylim = ylm,
           ylab = "Prevalence", xlab = "Time")
  })}
  
  true_pr <- xds_lines_PR(xds_obj, method, i, clrs, lty)
  return(invisible(true_pr)) 
}

#' Add lines for the prevalence / parasite rate (PR) from a model of human infection and immunity
#'
#' @param xds_obj an **`xds`** model object
#' @param method the method used for computing *Pf*PR 
#' @param i the host species index
#' @param clrs a vector of colors
#' @param lty an integer (or integers) that specifies `lty` for plotting
#' 
#' @importFrom graphics lines
#'
#' @return true pr, invisibly 
#' 
#' @export
xds_lines_PR = function(xds_obj, method, i, clrs="black", lty=1){
  n = xds_obj$nStrata[i] 
  if(length(clrs) != n) clrs=rep(clrs, n)
  if(length(lty) != n) lty=rep(lty, n)
  with(get_PR(xds_obj, method, i),{
    if(n==1) lines(time, pr, col=clrs, lty = lty)
    if(n>1) for(j in 1:n) lines(time, pr[,j], col=clrs[j], lty = lty[j])
    return(invisible(pr))
})}


