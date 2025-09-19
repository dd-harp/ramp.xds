library(expm)
library(MASS)
library(deSolve)

numeric_tol <- 1e-5

test_that("test equilibrium with macdonald adults (DDE), hMoI humans, trivial", {

  # set number of patches and strata
  nPatches <- 2
  residence <- c(1:2)
  nStrata <- length(residence)
  membership <- c(1:2)

  # parameters
  b <- 0.55
  c1 <- 0.05
  c2 <- 0.25
  r1 <- 1/250
  r2 <- 1/50
  Xo = list(b=b,c1=c1,c2=c2,r1=r1,r2=r2)

  f <- rep(0.3, nPatches)
  q <- rep(0.9, nPatches)
  g <- rep(1/10, nPatches)
  sigma <- rep(1/100, nPatches)
  mu <- rep(0, nPatches)
  nu <- rep(1/2, nPatches)
  eggsPerBatch <- 30
  eip <- 11

  # mosquito movement K_matrix
  K_matrix <- matrix(0, nPatches, nPatches)
  K_matrix[upper.tri(K_matrix)] <- rexp(sum(1:(nPatches-1)))
  K_matrix[lower.tri(K_matrix)] <- rexp(sum(1:(nPatches-1)))
  K_matrix <- K_matrix/rowSums(K_matrix)
  K_matrix <- t(K_matrix)

  # omega matrix
  Omega <- make_Omega_xde(g, sigma, mu, K_matrix)
  Upsilon <- expm::expm(-Omega * eip)

  MYo <- list(nPatches=nPatches,
               f=f, q=q, g=g, sigma=sigma, mu=mu, nu=nu, eggsPerBatch=eggsPerBatch,
               eip=eip, Omega=Omega, Upsilon=Upsilon, K_matrix=K_matrix)


  # human PfPR and H
  H <- c(100, 120)
  residence = c(1,2)
  searchWtsH = c(1,1)

  m2 <- rnorm(2, 1.5, .2)
  foi <- r2*m2
  m1 <- foi/r1

  eir <- foi/b
  Xo$m2=m2
  Xo$m1=m1

  # TaR
  dg <- rbeta(nStrata, 80,20)
  TaR <- matrix(c(dg[1], 1-dg, dg[2]), 2, 2)

  # ambient pop
  W <- F_W_available(searchWtsH, H, TaR)
  beta <- F_beta(H, W, searchWtsH, TaR)

  # biting distribution matrix
  fqZ <- solve(beta) %*% eir 

  # kappa
  x1 = 1-exp(-m1)
  x2 = 1-exp(-m2)
  X <- ((c2 * x2) + (c1 * (x1 - x2)))*H
  kappa <- t(beta) %*% X

  # equilibrium solutions for adults
  Z <- fqZ/f/q
  MY <- diag(1/as.vector(f*q*kappa), nPatches, nPatches) %*% solve(Upsilon) %*% Omega %*% Z
  Y <- solve(Omega) %*% (diag(as.vector(f*q*kappa), nPatches, nPatches) %*% MY)
  M <- MY + Y
  P <- solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M
  Lambda <- Omega %*% M



  Lo = list(Lambda=Lambda)

  # parameters for exDE
  params <- xds_setup(MYname = "macdonald", MYoptions=MYo, Lname = "trivial", Loptions=Lo, TimeSpent = TaR, K_matrix=K_matrix,
                      Xname = "hMoI", XHoptions=Xo, HPop=H, membership=membership, nPatches=nPatches, residence=residence)

  steady_state_MY(Lambda, kappa, params, 1) -> ss

  MYo$M=ss$M
  MYo$P=ss$P
  MYo$Y=ss$Y
  MYo$Z=ss$Z
  
  params <- change_MY_inits(params, 1, MYo)

  params <- xds_solve(params, 730, 1)

  out <- params$outputs$last_y

  M_sim <- out[params$MY_obj[[1]]$ix$M_ix]
  P_sim <- out[params$MY_obj[[1]]$ix$P_ix]
  Y_sim <- out[params$MY_obj[[1]]$ix$Y_ix]
  Z_sim <- out[params$MY_obj[[1]]$ix$Z_ix]
  m1_sim <- out[params$XH_obj[[1]]$ix$m1_ix]
  m2_sim <- out[params$XH_obj[[1]]$ix$m2_ix]

  expect_equal(as.vector(M_sim), as.vector(M), tolerance = numeric_tol)
  expect_equal(as.vector(P_sim), as.vector(P), tolerance = numeric_tol)
  expect_equal(as.vector(Y_sim), as.vector(Y), tolerance = numeric_tol)
  expect_equal(as.vector(Z_sim), as.vector(Z), tolerance = numeric_tol)
  expect_equal(as.vector(m2_sim), as.vector(m2), tolerance = numeric_tol)
  expect_equal(as.vector(m1_sim), as.vector(m1), tolerance = numeric_tol)
})

