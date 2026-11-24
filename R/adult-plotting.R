# X
#' @title Plot adult mosquito density (**MY**)
#' @description Plot saved adult mosquito density over time, one line per patch
#' for the selected species.
#' @note Reads `M` and `time` through [get_MY_orbits].
#' Creates a new plot unless `add = TRUE`; then adds lines to the existing plot.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a color for all patches or a vector of colors, one per patch
#' @param llty a line type for all patches or a vector of line types, one per patch, passed to `lty`
#' @param add add lines to the existing plot if `TRUE`
#' @return invisible(NULL)
#' @seealso [xds_lines_M]
#'
#' @export
xds_plot_M = function(xds_obj, s=1, clrs="darkblue", llty=1, add=FALSE){
  MY = get_MY_orbits(xds_obj, s)

  if(add == FALSE)
    with(MY, plot(time, 0*time, type = "n", ylim = range(0,M),
                  ylab = "Mosquito Density", xlab = "Time"))

  xds_lines_M(xds_obj, s, clrs, llty)
}

#' @title Plot infected mosquito density (**MY**)
#' @description Plot saved infected mosquito density over time, one line per patch
#' for the selected species.
#' @note Reads `Y` and `time` through [get_MY_orbits].
#' Creates a new plot unless `add = TRUE`; then adds lines to the existing plot.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_lines_Y]
#'
#' @export
xds_plot_Y = function(xds_obj, s=1, clrs = "purple", llty=1, add=FALSE){
  MY = get_MY_orbits(xds_obj, s)

  if(add == FALSE)
    with(MY, plot(time, 0*time, type = "n", ylim = range(0,Y),
              ylab = "Mosquito Density", xlab = "time"))

  xds_lines_Y(xds_obj, s, clrs, llty)
}

#' @title Plot infective mosquito density (**MY**)
#' @description Plot saved infective mosquito density over time, one line per patch
#' for the selected species.
#' @note Reads `Z` and `time` through [get_MY_orbits].
#' Creates a new plot unless `add = TRUE`; then adds lines to the existing plot.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_lines_Z]
#'
#' @export
xds_plot_Z = function(xds_obj, s=1,  clrs="darkred", llty=1, add=FALSE){
  MY = get_MY_orbits(xds_obj, s)

  if(add == FALSE)
    with(MY,
         plot(time, 0*time, type = "n", ylim = range(0,Z),
              ylab = "Mosquito Density", xlab = "time"))

  xds_lines_Z(xds_obj, s, clrs, llty)
}

#' @title Plot the infected mosquito fraction (**MY**)
#' @description Plot the saved infected mosquito fraction over time, one line per
#' patch for the selected species.
#' @note Reads `y = Y/M` and `time` through [get_MY_orbits]; it does not
#' calculate the fraction. Creates a new plot unless `add = TRUE`;
#' then adds lines to the existing plot.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_lines_Y_fracs]
#'
#' @export
xds_plot_Y_fracs = function(xds_obj, s=1, clrs = "purple", llty=1,  add=FALSE){
  MY = get_MY_orbits(xds_obj, s)

  if(add == FALSE)
    with(MY, plot(time, 0*time, type = "n", ylim = range(0,y),
                  ylab = "Fraction Infected", xlab = "time"))

  xds_lines_Y_fracs(xds_obj, s, clrs, llty)
}

#' @title Plot the infective mosquito fraction (**MY**)
#' @description Plot the saved infective mosquito fraction over time, one line per
#' patch for the selected species.
#' @note Reads `z = Z/M` and `time` through [get_MY_orbits]; it does not
#' calculate the fraction. Creates a new plot unless `add = TRUE`;
#' then adds lines to the existing plot.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_lines_Z_fracs]
#'
#' @export
xds_plot_Z_fracs = function(xds_obj, s=1, clrs = "darkred", llty=1, add=FALSE){
  MY = get_MY_orbits(xds_obj, s)

  if(add == FALSE)
    with(MY,
         plot(time, 0*time, type = "n", ylim = range(0,z),
              ylab = "Fraction Infective", xlab = "time"))

  xds_lines_Z_fracs(xds_obj, s, clrs, llty)
}

#' @title Add lines for adult mosquito density (**MY**)
#' @description Add saved adult mosquito density lines to an existing plot,
#' one line per patch for the selected species.
#' @note Reads `M` and `time` through [get_MY_orbits].
#' An existing plot is required.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_plot_M]
#'
#' @export
xds_lines_M = function(xds_obj, s=1, clrs="darkblue", llty=1){
  MY = get_MY_orbits(xds_obj, s)
  with(MY,{
    if(xds_obj$nPatches==1) lines(time, M, col=clrs[1], lty = llty[1])
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      for(i in 1:xds_obj$nPatches){
        lines(time, M[,i], col=clrs[i], lty = llty[i])

      }
    }
  })}

#' @title Add lines for infected mosquito density (**MY**)
#' @description Add saved infected mosquito density lines to an existing plot,
#' one line per patch for the selected species.
#' @note Reads `Y` and `time` through [get_MY_orbits].
#' An existing plot is required.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_plot_Y]
#'
#' @export
xds_lines_Y = function(xds_obj, s=1, clrs="purple", llty=1){
  MY = get_MY_orbits(xds_obj, s)
  with(MY,{
    if(xds_obj$nPatches==1){
      lines(time, Y, col=clrs[1], lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)

      for(i in 1:xds_obj$nPatches){
        lines(time, Y[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

#' @title Add lines for infective mosquito density (**MY**)
#' @description Add saved infective mosquito density lines to an existing plot,
#' one line per patch for the selected species.
#' @note Reads `Z` and `time` through [get_MY_orbits].
#' An existing plot is required.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_plot_Z]
#'
#' @export
xds_lines_Z = function(xds_obj, s=1, clrs = "darkred", llty=1){
  MY = get_MY_orbits(xds_obj, s)
  
  with(MY,{
    if(xds_obj$nPatches==1){
      lines(time, Z, col=clrs[1], lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)

      for(i in 1:xds_obj$nPatches){
        lines(time, Z[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

#' @title Add lines for the infected mosquito fraction (**MY**)
#' @description Add saved infected mosquito fraction lines to an existing plot,
#' one line per patch for the selected species.
#' @note Reads `y = Y/M` and `time` through [get_MY_orbits]; it does not
#' calculate the fraction. An existing plot is required.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_plot_Y_fracs]
#'
#' @export
xds_lines_Y_fracs = function(xds_obj, s=1, clrs="purple", llty=1){
  MY = get_MY_orbits(xds_obj, s)
  with(MY,{
    if(xds_obj$nPatches==1) {
      lines(time, y, col=clrs, lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)

      for(i in 1:xds_obj$nPatches){
        lines(time, y[,i], col=clrs[i], lty = llty[i])
      }
    }
  })
}

#' @title Add lines for the infective mosquito fraction (**MY**)
#' @description Add saved infective mosquito fraction lines to an existing plot,
#' one line per patch for the selected species.
#' @note Reads `z = Z/M` and `time` through [get_MY_orbits]; it does not
#' calculate the fraction. An existing plot is required.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @inheritParams xds_plot_M
#' @return invisible(NULL)
#' @seealso [xds_plot_Z_fracs]
#'
#' @export
xds_lines_Z_fracs = function(xds_obj, s=1, clrs="darkred", llty=1){
  MY = get_MY_orbits(xds_obj, s)
  with(MY,{
    if(xds_obj$nPatches==1) {
      lines(time, z, col=clrs, lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)

      for(i in 1:xds_obj$nPatches){
        lines(time, z[,i], col=clrs[i], lty = llty[i])
      }
    }
})}
