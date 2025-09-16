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

  K_matrix <- matrix(0, nPatches, nPatches)
  K_matrix[1, 2:3] <- c(0.2, 0.8)
  K_matrix[2, c(1,3)] <- c(0.5, 0.5)
  K_matrix[3, 1:2] <- c(0.7, 0.3)
  K_matrix <- t(K_matrix)
  Omega <- make_Omega_xde(g, sigma, mu, K_matrix)
  Upsilon <- expm(-Omega*eip)

  MYo <- list(nPatches=nPatches, f=f, q=q, g=g, sigma=sigma,
               mu=mu, eip=eip, nu=nu, eggsPerBatch=eggsPerBatch,
               K_matrix=K_matrix, Omega=Omega, Upsilon=Upsilon)

  Omega_inv <- ginv(Omega)
  M_eq <- as.vector(Omega_inv %*% Lambda)
  P_eq <- as.vector(ginv(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)
  Y_eq <- as.vector(ginv(diag(f*q*kappa) + Omega) %*% diag(f*q*kappa) %*% M_eq)
  Z_eq <- as.vector(Omega_inv %*% Upsilon %*% diag(f*q*kappa) %*% (M_eq - Y_eq))

  MYo$M <- M_eq
  MYo$P <- P_eq
  MYo$Y <- Y_eq
  MYo$Z <- Z_eq

  Xo = list(kappa = kappa)

  Lo = list(Lambda=c(5, 10, 4, 4))

  params <- xds_setup(MYname = "macdonald", Lname="trivial", Xname="trivial", K_matrix=K_matrix,
                      MYoptions = MYo, Loptions = Lo, XHoptions = Xo, residence=residence,
                      nPatches=nPatches, membership=membership, HPop = rep(1000, nPatches))

  params <- xds_solve(params)
  out <- params$outputs$last_y

  M_sim <- out[params$MY_obj[[1]]$ix$M_ix]
  P_sim <- out[params$MY_obj[[1]]$ix$P_ix]
  Y_sim <- out[params$MY_obj[[1]]$ix$Y_ix]
  Z_sim <- out[params$MY_obj[[1]]$ix$Z_ix]

  expect_equal(M_eq, M_sim, tolerance = numeric_tol)
  expect_equal(P_eq, P_sim, tolerance = numeric_tol)
  expect_equal(Y_eq, Y_sim, tolerance = numeric_tol)
  expect_equal(Z_eq, Z_sim, tolerance = numeric_tol)
})
