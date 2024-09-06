library(expm)
library(deSolve)
library(MASS)

numeric_tol <- 1e-5

test_that("forced emergence works with equilibrium", {
  nPatches <- 3
  membership = c(1,2,3,3)
  nHabitats <- 4
  residence <- c(1:3)

  kappa <- c(0.1, 0.075, 0.025)
  Lambda <- c(5, 10, 8)

  f <- rep(0.3, nPatches)
  q <- rep(0.9, nPatches)
  g <- rep(1/20, nPatches)
  sigma <- rep(1/10, nPatches)
  mu <- rep(0, nPatches)
  eip <- 11
  nu <- 1/2
  eggsPerBatch <- 30

  calK <- matrix(0, nPatches, nPatches)
  calK[1, 2:3] <- c(0.2, 0.8)
  calK[2, c(1,3)] <- c(0.5, 0.5)
  calK[3, 1:2] <- c(0.7, 0.3)
  calK <- t(calK)
  Omega <- compute_Omega_xde(g, sigma, mu, calK)
  Upsilon <- expm(-Omega*eip)

  MYZo <- list(nPatches=nPatches, f=f, q=q, g=g, sigma=sigma,
               mu=mu, eip=eip, nu=nu, eggsPerBatch=eggsPerBatch,
               calK=calK, Omega=Omega, Upsilon=Upsilon)

  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))

  MYZo$M <- M_eq
  MYZo$P <- P_eq
  MYZo$Y <- Y_eq
  MYZo$Z <- Z_eq

  Xo = list(kappa = kappa)

  Lo = list(Lambda=c(5, 10, 4, 4))

  params <- xds_setup(MYZname = "macdonald", Lname="trivial", Xname="trivial", calK=calK,
                      MYZopts = MYZo, Lopts = Lo, Xopts = Xo, residence=residence,
                      nPatches=nPatches, membership=membership, HPop = rep(1000, nPatches))

  params <- xds_solve(params)
  out <- params$outputs$last_y

  M_sim <- out[params$ix$MYZ[[1]]$M_ix]
  P_sim <- out[params$ix$MYZ[[1]]$P_ix]
  Y_sim <- out[params$ix$MYZ[[1]]$Y_ix]
  Z_sim <- out[params$ix$MYZ[[1]]$Z_ix]

  expect_equal(M_eq, M_sim, tolerance = numeric_tol)
  expect_equal(P_eq, P_sim, tolerance = numeric_tol)
  expect_equal(Y_eq, Y_sim, tolerance = numeric_tol)
  expect_equal(Z_eq, Z_sim, tolerance = numeric_tol)
})
