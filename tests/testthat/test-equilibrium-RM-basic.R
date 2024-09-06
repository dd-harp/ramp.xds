library(expm)
library(MASS)
library(deSolve)



test_that("test equilibrium with macdonald adults (DDE), basic competition", {

  numeric_tol <- 1e-5

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

  # mosquito movement calK
  calK <- matrix(0, nPatches, nPatches)
  calK[upper.tri(calK)] <- rexp(sum(1:(nPatches-1)))
  calK[lower.tri(calK)] <- rexp(sum(1:(nPatches-1)))
  calK <- calK/rowSums(calK)
  calK <- t(calK)

  # omega matrix
  Omega <- compute_Omega_xde(g, sigma, mu, calK)
  Upsilon <- expm::expm(-Omega * eip)

  MYZo <- list(nPatches=nPatches,
               f=f, q=q, g=g, sigma=sigma, mu=mu, nu=nu, eggsPerBatch=eggsPerBatch,
               eip=eip, Omega=Omega, Upsilon=Upsilon, calK=calK)


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
  W <- compute_W(searchWtsH, H, TaR)
  beta <- compute_beta(H, W, searchWtsH, TaR)

  # biting distribution matrix
  fqZ <- eir2fqZ(eir, beta)

  # kappa
  kappa <- t(beta) %*% (I*c)

  # equilibrium solutions for adults
  Z <- fqZ/f/q
  MY <- diag(1/as.vector(f*q*kappa), nPatches, nPatches) %*% solve(Upsilon) %*% Omega %*% Z
  Y <- solve(Omega) %*% (diag(as.vector(f*q*kappa), nPatches, nPatches) %*% MY)
  M <- MY + Y
  P <- solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M
  Lambda <- Omega %*% M



  xde_steady_state_MYZ.macdonald(Lambda, kappa, MYZo) -> ss

  MYZo$M=M
  MYZo$P=P
  MYZo$Y=Y
  MYZo$Z=Z

  psi <- 1/10
  phi <- 1/12
  eta <- M * nu * eggsPerBatch

  L <- Lambda/psi
  theta <- (eta - psi*L - phi*L)/(L^2)
  Lo = list(psi=psi, phi=phi, theta=theta, L=L)

  Xo = list(kappa=kappa, HPop=H)
  # parameters for exDE
  params <- xds_setup_mosy(MYZname = "macdonald", MYZopts=MYZo, Lname = "basicL", Lopts=Lo, calK=calK, kappa=kappa,
                       HPop=H, membership=membership, nPatches=nPatches)

  params$kappa[[1]] = kappa

  params <- xds_solve(params, 730, 1)

  out <- params$outputs$last_y

  M_sim <- out[params$ix$MYZ[[1]]$M_ix]
  P_sim <- out[params$ix$MYZ[[1]]$P_ix]
  Y_sim <- out[params$ix$MYZ[[1]]$Y_ix]
  Z_sim <- out[params$ix$MYZ[[1]]$Z_ix]
  L_sim <- out[params$ix$L[[1]]$L_ix]


  expect_equal(as.vector(M_sim), as.vector(M), tolerance = numeric_tol)
  expect_equal(as.vector(P_sim), as.vector(P), tolerance = numeric_tol)
  expect_equal(as.vector(Y_sim), as.vector(Y), tolerance = numeric_tol)
  expect_equal(as.vector(Z_sim), as.vector(Z), tolerance = numeric_tol)
  expect_equal(as.vector(L_sim), as.vector(L), tolerance = numeric_tol)
})
