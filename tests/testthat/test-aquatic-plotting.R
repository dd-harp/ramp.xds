test_that("aquatic lines plot each habitat independently of patch count", {
  draw_lines <- xds_lines_L
  calls <- list()
  plotting_env <- new.env(parent=environment(draw_lines))
  plotting_env$lines <- function(x, y, col, lty) {
    calls[[length(calls)+1]] <<- list(x=x, y=y, col=col, lty=lty)
    invisible(NULL)
  }
  environment(draw_lines) <- plotting_env

  for (counts in list(c(1, 3), c(3, 1), c(3, 3))) {
    nPatches <- counts[1]
    nHabitats <- counts[2]
    time <- 0:2
    densities <- matrix(seq_len(3*nHabitats), nrow=3)
    model <- list(
      nPatches=nPatches, nHabitats=nHabitats,
      outputs=list(time=time, orbits=list(L=list(
        list(L=0*densities), list(L=densities)
      )))
    )
    for (per_habitat in c(FALSE, TRUE)) {
      calls <- list()
      colors <- if (per_habitat) rep(c("red", "blue", "green"), length.out=nHabitats) else "red"
      types <- if (per_habitat) seq_len(nHabitats) else 2
      draw_lines(model, s=2, clrs=colors, llty=types)

      expect_length(calls, nHabitats)
      for (habitat in seq_len(nHabitats)) {
        expect_identical(calls[[habitat]]$x, time)
        expect_equal(as.vector(calls[[habitat]]$y), densities[,habitat])
        expect_identical(calls[[habitat]]$col, rep(colors, length.out=nHabitats)[habitat])
        expect_equal(calls[[habitat]]$lty, rep(types, length.out=nHabitats)[habitat])
      }
    }
  }
})
