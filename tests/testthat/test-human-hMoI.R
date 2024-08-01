library(deSolve)

numeric_tol <- 1e-5

test_that("human hybrid MoI model remains at equilibrium", {
  nStrata <- 3
  H <- c(100, 500, 250)
  residence = 1:nStrata
  nPatches=3

  b <- 0.55
  c1 <- 0.05
  c2 <- 0.25
  r1 <- 1/250
  r2 <- 1/50

  Xo = list(b=b,c1=c1,c2=c2,r1=r1,r2=r2)

  m2 <- rnorm(3, 1.5, .2)
  foi <- r2*m2
  m1 <- foi/r1

  Xo = list(b=b,c1=c1,c2=c2,r1=r1,r2=r2, m2=m2, m1=m1)

  params <- xds_setup_human(Xname ="hMoI", Xopts=Xo, nPatches=nPatches, HPop=H, residence = residence)
  params$FoI[[1]] <- foi

  y0 <- as.vector(unlist(get_inits(params)))

  out <- deSolve::ode(y = y0, times = c(0, 365), func = function(t, y, pars, s) {
    list(dXdt(t, y, pars, s))
  }, parms = params, method = 'lsoda', s=1)

  expect_equal(as.vector(out[2L, params$ix$X[[1]]$m1_ix+1]), m1, tolerance = numeric_tol)
  expect_equal(as.vector(out[2L, params$ix$X[[1]]$m2_ix+1]), m2, tolerance = numeric_tol)

})
