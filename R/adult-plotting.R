
#' @title Plot adult mosquito population density
#'
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_M = function(pars, s=1, clrs="darkblue", llty=1, add=FALSE){
  MYZ = pars$outputs$orbits$MYZ[[s]]
  times = pars$outputs$time

  if(add == FALSE) with(MYZ,
                        plot(times, 0*times, type = "n", ylim = range(0,M),
                             ylab = "Mosquito Density", xlab = "times"))

  xds_lines_M(times, MYZ, pars, clrs, llty)
}

#' Add lines for adult mosquito population density
#'
#' @param times a sequence of times when variables were output
#' @param MYZ parsed ouptuts
#' @param pars an **`xds`** object
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_M = function(times, MYZ, pars, clrs="darkblue", llty=1){
  with(MYZ,{
    if(pars$nPatches==1) lines(times, M, col=clrs[1], lty = llty[1])
    if(pars$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, pars$nPatches)
      if (length(llty)==1) llty=rep(llty, pars$nPatches)
      for(i in 1:pars$nPatches){
        lines(times, M[,i], col=clrs[i], lty = llty[i])

      }
    }
  })}

#' Plot the density of infected and infective mosquitoes
#'
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param clrs a vector of colors for infected mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Y = function(pars, s=1, clrs = "purple", llty=1, add=FALSE){
  MYZ = pars$outputs$orbits$MYZ[[s]]
  times = pars$outputs$time

  if(add == FALSE)
    with(MYZ,
         plot(times, 0*times, type = "n", ylim = range(0,Y),
              ylab = "Mosquito Density", xlab = "times"))

  xds_lines_Y(times, MYZ, pars, clrs, llty)
}

#' Add lines for the density of infected and infective mosquitoes
#'
#' @param times a sequence of times when variables were output
#' @param MYZ parsed ouptuts
#' @param pars an **`xds`** object
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Y = function(times, MYZ, pars, clrs="purple", llty=1){
  with(MYZ,{
    if(pars$nPatches==1){
      lines(times, Y, col=clrs[1], lty = llty[1])
    }
    if(pars$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, pars$nPatches)
      if (length(llty)==1) llty=rep(llty, pars$nPatches)

      for(i in 1:pars$nPatches){
        lines(times, Y[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

#' Plot the density of infected and infective mosquitoes
#'
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param clrs a vector of colors for infective mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Z = function(pars, s=1,  clrs="darkred", llty=1, add=FALSE){
  MYZ = pars$outputs$orbits$MYZ[[s]]
  times = pars$outputs$time

  if(add == FALSE)
    with(MYZ,
         plot(times, 0*times, type = "n", ylim = range(0,Z),
              ylab = "Mosquito Density", xlab = "times"))

  xds_lines_Z(times, MYZ, pars, clrs, llty)
}

#' Add lines for the density of infected and infective mosquitoes
#'
#' @param times a sequence of times when variables were output
#' @param MYZ parsed ouptuts
#' @param pars an **`xds`** object
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Z = function(times, MYZ, pars, clrs = "darkred", llty=1){
  with(MYZ,{
    if(pars$nPatches==1){
      lines(times, Z, col=clrs[1], lty = llty[1])
    }
    if(pars$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, pars$nPatches)
      if (length(llty)==1) llty=rep(llty, pars$nPatches)

      for(i in 1:pars$nPatches){
        lines(times, Z[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

#' Plot the fraction of infected and infective mosquitoes
#'
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param clrs a vector of colors for infected mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Y_fracs = function(pars, s=1, clrs = "purple", llty=1,  add=FALSE){
  MYZ = pars$outputs$orbits$MYZ[[s]]
  times = pars$outputs$time

  if(add == FALSE)
    with(MYZ,
         plot(times, 0*times, type = "n", ylim = range(0,y),
              ylab = "Fraction Infected", xlab = "times"))

  xds_lines_Y_fracs(times, MYZ, pars, clrs, llty)
}

#' Add lines for the fraction of infected and infective mosquitoes
#'
#' @param times times points for the observations
#' @param MYZ parsed ouptuts
#' @param pars an **`xds`** object
#' @param clrs a vector of colors for infected mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Y_fracs = function(times, MYZ, pars, clrs="purple", llty=1){
  with(MYZ,{
    if(pars$nPatches==1) {
      lines(times, y, col=clrs, lty = llty[1])
    }
    if(pars$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, pars$nPatches)
      if (length(llty)==1) llty=rep(llty, pars$nPatches)

      for(i in 1:pars$nPatches){
        lines(times, y[,i], col=clrs[i], lty = llty[i])
      }
    }
  })
}

#' Plot the fraction infective
#'
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param clrs a vector of colors for infective mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Z_fracs = function(pars, s=1, clrs = "darkred", llty=1, add=FALSE){
  MYZ = pars$outputs$orbits$MYZ[[s]]
  times = pars$outputs$time

  if(add == FALSE)
    with(MYZ,
         plot(times, 0*times, type = "n", ylim = range(0,z),
              ylab = "Fraction Infected", xlab = "times"))

  xds_lines_Z_fracs(times, MYZ, pars, clrs, llty)
}

#' Add lines for the fraction of infected and infective mosquitoes
#'
#' @param times a sequence of times when variables were output
#' @param MYZ parsed outputs
#' @param pars an **`xds`** object
#' @param clrs a vector of colors for infective mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Z_fracs = function(times, MYZ, pars, clrs="darkred", llty=1){
  with(MYZ,{
    if(pars$nPatches==1) {
      lines(times, z, col=clrs, lty = llty[1])
    }
    if(pars$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, pars$nPatches)
      if (length(llty)==1) llty=rep(llty, pars$nPatches)

      for(i in 1:pars$nPatches){
        lines(times, z[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

