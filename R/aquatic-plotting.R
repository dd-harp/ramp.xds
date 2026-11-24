# X
#' @title Plot aquatic mosquito population density
#' @description Plot aquatic mosquito population density over time for each habitat.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a color for all habitats or a vector of colors, one per habitat
#' @param llty a line type for all habitats or a vector of line types, one per habitat, passed to `lty`
#' @param add add lines to the existing plot if TRUE
#' @return invisible(NULL)
#' @seealso [xds_lines_L]
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
#' @description Add aquatic mosquito population density lines for each habitat to an existing plot.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a color for all habitats or a vector of colors, one per habitat
#' @param llty a line type for all habitats or a vector of line types, one per habitat, passed to `lty`
#' @return invisible(NULL)
#' @seealso [xds_plot_L]
#'
#' @export
xds_lines_L = function(xds_obj, s=1, clrs="darkblue", llty=1){
  L = get_L_orbits(xds_obj, s)
  with(L,{
    if(xds_obj$nHabitats==1) lines(time, L, col=clrs[1], lty = llty[1])
    if(xds_obj$nHabitats>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nHabitats)
      if (length(llty)==1) llty=rep(llty, xds_obj$nHabitats)
      for(i in 1:xds_obj$nHabitats){
        lines(time, L[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}