library(deSolve)


test_that("basic competition stays at equilibrium", {

  numeric_tol <- 1e-5

  nHabitats <- 3
  alpha <- c(10, 50, 20)
  eggs_laid <- c(250, 500, 170)
  psi <- 1/10
  xi <- 0
  phi <- 1/12

  L <- alpha/psi
  theta <- (eggs_laid - psi*L - phi*L)/(L^2)

  Lo = list(psi=psi, xi=xi, phi=phi, theta=theta, L=L)
  class(Lo) <- "basicL"
  steady_state_L(eggs_laid, Lo)

  params <- xds_setup_aquatic(Lname = "basicL", nHabitats=nHabitats, Loptions=Lo)

  params$terms$eta[[1]] = eggs_laid

  y0 <- L

  out <- deSolve::ode(y = y0, times = c(0, 365), func = function(t, y, pars, s) {
    list(dLdt(t, y, pars, s))
  }, parms = params, method = 'lsoda', s=1)

  Lout <- tail(out, 1)[-1]
  expect_equal(Lout, L, tolerance = numeric_tol)
})
