library(deSolve)

numeric_tol <- 1e-5

test_that("human SIS_xde model remains at equilibrium", {
  HPop <- c(100, 500, 250)
  nPatches=3
  residence = 1:3

  I <- c(20, 120, 80)
  b <- 0.55
  c <- 0.15
  r <- 1/200
  Xo = list(b=b, c=c, r=r, I=I, H=HPop)

  foi <- ((r*I)/(HPop-I))

  params <- xds_setup_human(Xname ="SIS", XHoptions=Xo, nPatches=nPatches, HPop=HPop, residence = residence)
  params$terms$FoI[[1]] <- foi

  # set initial conditions
  y0 <- as.vector(unlist(get_inits(params)))

  out <- deSolve::ode(y = y0, times = c(0, 365), func = function(t, y, pars, s) {
    list(dXHdt(t, y, pars, s))
  }, parms = params, method = 'lsoda', s=1)

  expect_equal(as.vector(out[2L, params$XH_obj[[1]]$ix$I_ix+1]), I, tolerance = numeric_tol)
})
