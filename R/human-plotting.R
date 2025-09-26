
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
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add a logical: plot axes only if FALSE
#' @param annual if true, plot as an annualized rate
#' 
#' @importFrom graphics plot 
#'
#' @export
xds_plot_EIR <- function(xds_obj, i=1, clrs="black", llty=1, add=FALSE, annual=TRUE){
  ylb = ifelse(annual==TRUE, "aEIR", "dEIR")
  if(add==FALSE)
    with(get_XH_out(xds_obj, i),{ 
      fac = ifelse(annual==TRUE, 365, 1)
      plot(time, 0*time, type="n", ylim=range(0, fac*eir),
           xlab = "Time", ylab = ylb)
    })
  xds_lines_EIR(xds_obj, i, clrs, llty, annual)
}

#' Add lines for the EIR *vs.* time
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param annual if true, plot as an annualized rate
#' 
#' @importFrom graphics lines
#'
#' @export
xds_lines_EIR <- function(xds_obj, i=1, clrs="black", llty=1, annual=TRUE){
  n = xds_obj$nStrata[i] 
  if(length(clrs) != n) clrs=rep(clrs, n)
  if(length(llty) != n) llty=rep(llty, n)
  with(get_XH_out(xds_obj, i),{
    fac = ifelse(annual==TRUE, 365, 1)
    for(j in 1:n) lines(time, fac*eir[,j], col=clrs[j], lty = llty)
    
})}

#' Plot the prevalence / parasite rate (PR) from a model of human infection and immunity
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) that specifies `lty` for plotting
#' @param add a logical: plot axes only if FALSE
#'
#' @export
xds_plot_PR = function(xds_obj, i=1, clrs="black", llty=1, add=FALSE){
  
  if(add==FALSE){
    with(get_XH_out(xds_obj, i),
      plot(time, true_pr, type = "n", ylim = c(0,1),
           ylab = "Prevalence", xlab = "Time")
  )}
  
  xds_lines_PR(xds_obj, i, clrs, llty)
}

#' Add lines for the prevalence / parasite rate (PR) from a model of human infection and immunity
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) that specifies `lty` for plotting
#' 
#' @importFrom graphics lines
#'
#' @export
xds_lines_PR = function(xds_obj, i, clrs="black", llty=1){
  n = xds_obj$nStrata[i] 
  if(length(clrs) != n) clrs=rep(clrs, n)
  if(length(llty) != n) llty=rep(llty, n)
  
  with(get_XH_out(xds_obj, i),{
    if(n==1) lines(time, true_pr, col=clrs, lty = llty)
    else for(j in 1:n) lines(time, true_pr[,j], col=clrs[j], lty = llty[j])
})}


