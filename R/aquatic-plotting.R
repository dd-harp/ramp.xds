
#' @title Plot aquatic mosquito population density
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#' @return invisible(NULL)
#'
#' @export
xds_plot_L = function(xds_obj, s=1, clrs="darkblue", llty=1, add=FALSE){
  L = get_L_orbits(xds_obj, s)
  
  if(add == FALSE)
    with(L, plot(time, 0*time, type = "n", ylim = range(0,L),
                  ylab = "Larval Mosquito Density", xlab = "Time"))
  
  xds_lines_L(xds_obj, s, clrs, llty)
}

#' @title Add lines for aquatic mosquito population density
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @return invisible(NULL)
#'
#' @export
xds_lines_L = function(xds_obj, s=1, clrs="darkblue", llty=1){
  L = get_L_orbits(xds_obj, s)
  with(L,{
    if(xds_obj$nPatches==1) lines(time, L, col=clrs[1], lty = llty[1])
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      for(i in 1:xds_obj$nPatches){
        lines(time, L[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}