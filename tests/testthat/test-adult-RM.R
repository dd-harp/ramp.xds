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

  MYZo = list(f=f,q=q,g=g,sigma=sigma,mu=mu,eip=eip,nu=nu,eggsPerBatch=eggsPerBatch)

  calK <- matrix(0, nPatches, nPatches)
  calK[1, 2:3] <- c(0.2, 0.8)
  calK[2, c(1,3)] <- c(0.5, 0.5)
  calK[3, 1:2] <- c(0.7, 0.3)
  calK <- t(calK)

  Omega <- compute_Omega_xde(g, sigma, mu, calK)
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


  MYZo$M=M_eq
  MYZo$P=P_eq
  MYZo$Y=Y_eq
  MYZo$Z=Z_eq

  params <- xds_setup(MYZname= "macdonald", Xname = "trivial", Lname = "trivial", nPatches=nPatches, membership = c(1:nPatches), Xopts=Xo, MYZopts=MYZo, Lopts=Lo, calK = calK)

  params$Lambda[[1]] = Lambda
  params$kappa[[1]] = kappa

  y0 = unname(as.vector(unlist(get_MYZinits(params, 1))))
  params <- MBionomics(0,y0,params, 1)

  # solve ODEs
  out <- deSolve::dede(y = y0, times = c(0, 730), func = function(t, y, pars, s) {
    list(dMYZdt(t, y, pars, s))
  }, parms = params, method = 'lsoda', s=1)

  MYZ_end <- out[2,-1]

  M_sim <- as.vector(MYZ_end[params$ix$MYZ[[1]]$M_ix])
  P_sim <- as.vector(MYZ_end[params$ix$MYZ[[1]]$P_ix])
  Y_sim <- as.vector(MYZ_end[params$ix$MYZ[[1]]$Y_ix])
  Z_sim <- as.vector(MYZ_end[params$ix$MYZ[[1]]$Z_ix])

  expect_equal(M_eq, M_sim, tolerance = numeric_tol)
  expect_equal(P_eq, P_sim, tolerance = numeric_tol)
  expect_equal(Y_eq, Y_sim, tolerance = numeric_tol)
  expect_equal(Z_eq, Z_sim, tolerance = numeric_tol)
})
