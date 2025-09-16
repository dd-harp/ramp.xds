library(expm)
library(deSolve)


test_that("macdonald models reach equilibrium", {

  # tolerance for tests comparing floats
  numeric_tol <- 1e-4
  HPop = rep(1, 3)
  nPatches <- 3
  f <- rep(0.3, nPatches)
  q <- rep(0.9, nPatches)
  g <- rep(1/20, nPatches)
  sigma <- rep(1/10, nPatches)
  mu <- rep(0, nPatches)
  eip <- 12
  nu <- 1/2
  eggsPerBatch <- 30

  MYo = list(f=f,q=q,g=g,sigma=sigma,mu=mu,eip=eip,nu=nu,eggsPerBatch=eggsPerBatch)

  K_matrix <- matrix(0, nPatches, nPatches)
  K_matrix[1, 2:3] <- c(0.2, 0.8)
  K_matrix[2, c(1,3)] <- c(0.5, 0.5)
  K_matrix[3, 1:2] <- c(0.7, 0.3)
  K_matrix <- t(K_matrix)

  Omega <- make_Omega_xde(g, sigma, mu, K_matrix)
  Upsilon <- expm::expm(-Omega * eip)

  kappa <- c(0.1, 0.075, 0.025)
  Xo = list(kappa=kappa)
  Lambda <- c(5, 10, 8)
  Lo = list(Lambda=Lambda)

  Omega_inv <- solve(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(solve(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))


  MYo$M=M_eq
  MYo$P=P_eq
  MYo$Y=Y_eq
  MYo$Z=Z_eq

  params <- xds_setup(MYname= "macdonald", Xname = "trivial", Lname = "trivial", nPatches=nPatches, membership = c(1:nPatches), XHoptions=Xo, MYoptions=MYo, Loptions=Lo, K_matrix = K_matrix)

  params$terms$Lambda[[1]] = Lambda
  params$terms$kappa[[1]] = kappa

  y0 = unname(as.vector(unlist(get_MY_inits(params, 1))))
  params <- MBionomics(0,y0,params, 1)

  # solve ODEs
  out <- deSolve::dede(y = y0, times = c(0, 730), func = function(t, y, pars, s) {
    list(dMYdt(t, y, pars, s))
  }, parms = params, method = 'lsoda', s=1)

  MY_end <- out[2,-1]

  M_sim <- as.vector(MY_end[params$MY_obj[[1]]$ix$M_ix])
  P_sim <- as.vector(MY_end[params$MY_obj[[1]]$ix$P_ix])
  Y_sim <- as.vector(MY_end[params$MY_obj[[1]]$ix$Y_ix])
  Z_sim <- as.vector(MY_end[params$MY_obj[[1]]$ix$Z_ix])

  expect_equal(M_eq, M_sim, tolerance = numeric_tol)
  expect_equal(P_eq, P_sim, tolerance = numeric_tol)
  expect_equal(Y_eq, Y_sim, tolerance = numeric_tol)
  expect_equal(Z_eq, Z_sim, tolerance = numeric_tol)
})
