library(expm)
library(MASS)
library(deSolve)

numeric_tol <- 1e-5

test_that("test equilibrium with macdonald adults (DDE), SIS_xde humans, trivial", {

  # set number of patches and strata
  nPatches <- 2
  residence <- c(1:2)
  nStrata <- length(residence)
  membership <- c(1:2)

  # parameters
  b <- 0.55
  c <- 0.15
  r <- 1/200
  Xo <- list(b=b, c=c, r=r)

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
  pfpr <- rep(0.3, times = nStrata)
  H <- rpois(n = nStrata, lambda = 1000)
  residence = c(1,2)
  searchWtsH = c(1,1)
  I <- rbinom(n = nStrata, size = H, prob = pfpr)
  foi <- r*I/(H-I)
  eir <- foi/b
  Xo$I=I


  # TaR
  dg <- rbeta(nStrata, 80,20)
  TaR <- matrix(c(dg[1], 1-dg, dg[2]), 2, 2)

  # ambient pop
  W <- F_W_available(searchWtsH, H, TaR)
  beta <- F_beta(H, W, searchWtsH, TaR)

  # biting distribution matrix
  
  fqZ <- solve(beta) %*% eir

  # kappa
  kappa <- t(beta) %*% (I*c)

  # equilibrium solutions for adults
  Z <- fqZ/f/q
  fqk <- as.vector(f*q*kappa)
  MY <- solve(Upsilon) %*% Omega %*% Z
#  Y <- solve(Omega) %*% (diag(as.vector(f*q*kappa), nPatches, nPatches) %*% MY)
  Y <- solve(Omega) %*% MY
  M <- diag(1/fqk)%*%(diag(fqk)%*%Y + Omega %*%Y) 
  P <- solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M
  Lambda <- Omega %*% M

  class(MYo) <- "macdonald"
  steady_state_MY(Lambda, kappa, MYo) -> ss

  MYo$M=M
  MYo$P=P
  MYo$Y=Y
  MYo$Z=Z


  Lo = list(Lambda=Lambda)

  # parameters for exDE
  params <- xds_setup(MYname = "macdonald", MYoptions=MYo, 
                      Lname = "trivial", Loptions=Lo, 
                      TimeSpent = TaR, K_matrix=K_matrix,
                      Xname = "SIS", XHoptions=Xo, HPop=H, 
                      membership=membership, nPatches=nPatches, 
                      residence=residence)


  params <- xds_solve(params, 730, 1)

  outMY <- get_last(params, parse=TRUE)$MY[[1]]
  M_sim <- outMY$M
  P_sim <- outMY$P 
  Y_sim <- outMY$Y 
  Z_sim <- outMY$Z 
  
  outXH <- get_last(params, parse=TRUE)$XH[[1]]
  I_sim <- outXH$I


  expect_equal(as.vector(M_sim), as.vector(M), tolerance = numeric_tol)
  expect_equal(as.vector(P_sim), as.vector(P), tolerance = numeric_tol)
  expect_equal(as.vector(Y_sim), as.vector(Y), tolerance = numeric_tol)
  expect_equal(as.vector(Z_sim), as.vector(Z), tolerance = numeric_tol)
  expect_equal(as.vector(I_sim), as.vector(I), tolerance = numeric_tol)
})
