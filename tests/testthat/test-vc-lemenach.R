library(MASS)
library(expm)
library(deSolve)

numeric_tol <- 1e-5

test_that("Le Menach VC model with 0 coverage stays roughly at equilibrium", {
  nPatches <- 3
  nStrata <- nPatches
  nHabitats <- nPatches
  residence <- c(1:nStrata)
  membership <- c(1:3)
  HPop <- rpois(n = nPatches, lambda = 1000)

  # human parameters
  b <- 0.55
  c <- 0.15
  r <- 1/200
  Xo = list(b=b,c=c,r=r)
  class(Xo) <- "SIS"

  foi <- rlnorm(nStrata, 3/365, .5)/365
  xde_steady_state_X(foi, HPop, Xo) -> ssI
  I <- ssI$I

  eir <- foi/b
  Xo$I=I

  searchWtsH = rep(1,3)

  TaR <- matrix(
    data = c(
      0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9
    ), nrow = nStrata, ncol = nPatches, byrow = T
  )
  TaR <- t(TaR)

  f <- rep(0.3, nPatches)
  q <- rep(0.9, nPatches)
  g <- rep(1/10, nPatches)
  mu <- rep(0, nPatches)
  sigma <- rep(1/100, nPatches)
  nu <- rep(1/2, nPatches)
  eggsPerBatch <- 30
  eip <- 11
  MYZo = list(f=f, q=q, g=g, sigma=sigma, mu=mu,
              nu=nu, eggsPerBatch=eggsPerBatch, eip=eip)

  calK = create_calK_herethere(nPatches)
  calK

  W <- compute_W(searchWtsH, HPop, TaR)
  beta <- compute_beta(HPop, W, searchWtsH, TaR)
  kappa <- compute_kappa(W, W, beta, c*I)

  Omega <- compute_Omega_xde(g, sigma, mu, calK)
  Omega_inv <- solve(Omega)
  Upsilon <- expm::expm(-Omega * eip)
  Upsilon_inv <- expm::expm(Omega * eip)

  fqZ <- eir2fqZ(eir, beta)

  # equilibrium solutions
  Y <-  Upsilon_inv %*% diag(1/as.vector(f*q), nPatches) %*% fqZ
  M <- diag(1/as.vector(f*q*kappa), nPatches) %*% (diag(as.vector(f*q*kappa)) + Omega) %*% Y
  Lambda <- as.vector(Omega %*% M)

  MYZo$M=M
  MYZo$Y=Y

  psi <- 1/10
  phi <- 1/12
  eta <- M * nu * eggsPerBatch

  alpha <- Lambda
  L <- alpha/psi
  theta <- (eta - psi*L - phi*L)/(L^2)

  # adult mosquito parameters

  Lo = list(psi=psi, phi=phi, theta=theta, L=L)



  xds_setup(MYZname="SI", Xname="SIS", Lname="basicL",
            nPatches=3, HPop=HPop, membership=membership,
            MYZopts=MYZo, calK=calK,
            Xopts=Xo, residence=1:3, searchB=searchWtsH,
            TimeSpent=TaR, searchQ=rep(1,3), Lopts=Lo) -> itn_mod

  itn_mod <- xds_solve(itn_mod, 730, 1)
  out <- itn_mod$outputs$last_y

  M_sim <- out[itn_mod$ix$MYZ[[1]]$M_ix]
  Y_sim <- out[itn_mod$ix$MYZ[[1]]$Y_ix]
  I_sim <- out[itn_mod$ix$X[[1]]$I_ix]

  expect_equal(as.vector(M_sim), as.vector(M), tolerance = numeric_tol)
  expect_equal(as.vector(Y_sim), as.vector(Y), tolerance = numeric_tol)
  expect_equal(as.vector(I_sim), as.vector(I), tolerance = numeric_tol)
})

test_that("Le Menach VC model under control reaches the enw predicted equilibrium", {
  set.seed(23)
  nPatches <- 3
  nStrata <- nPatches
  nHabitats <- nPatches
  residence <- c(1:nStrata)
  membership <- c(1:3)
  HPop <- rpois(n = nPatches, lambda = 1000)

  # human parameters
  b <- 0.55
  c <- 0.15
  r <- 1/200
  Xo = list(b=b,c=c,r=r)
  class(Xo) <- "SIS"

  foi <- rlnorm(nStrata, 3/365, .5)/365
  xde_steady_state_X(foi, HPop, Xo) -> ssI
  I <- ssI$I

  eir <- foi/b
  Xo$I=I

  searchWtsH = rep(1,3)

  TaR <- matrix(
    data = c(
      0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9
    ), nrow = nStrata, ncol = nPatches, byrow = T
  )
  TaR <- t(TaR)

  f <- rep(0.3, nPatches)
  q <- rep(0.9, nPatches)
  g <- rep(1/10, nPatches)
  mu <- rep(0, nPatches)
  sigma <- rep(1/100, nPatches)
  nu <- rep(1/2, nPatches)
  eggsPerBatch <- 30
  eip <- 11
  MYZo = list(f=f, q=q, g=g, sigma=sigma, mu=mu,
              nu=nu, eggsPerBatch=eggsPerBatch, eip=eip)

  calK = create_calK_herethere(nPatches)
  calK

  W <- compute_W(searchWtsH, HPop, TaR)
  beta <- compute_beta(HPop, W, searchWtsH, TaR)
  kappa <- compute_kappa(W, W, beta, c*I)

  Omega <- compute_Omega_xde(g, sigma, mu, calK)
  Omega_inv <- solve(Omega)
  Upsilon <- expm::expm(-Omega * eip)
  Upsilon_inv <- expm::expm(Omega * eip)

  fqZ <- eir2fqZ(eir, beta)

  # equilibrium solutions
  Y <-  Upsilon_inv %*% diag(1/as.vector(f*q), nPatches) %*% fqZ
  M <- diag(1/as.vector(f*q*kappa), nPatches) %*% (diag(as.vector(f*q*kappa)) + Omega) %*% Y
  Lambda <- as.vector(Omega %*% M)

  MYZo$M=M
  MYZo$Y=Y

  psi <- 1/10
  phi <- 1/12
  eta <- M * nu * eggsPerBatch

  alpha <- Lambda
  L <- alpha/psi
  theta <- (eta - psi*L - phi*L)/(L^2)

  # adult mosquito parameters

  Lo = list(psi=psi, phi=phi, theta=theta, L=L)



  xds_setup(MYZname="SI", Xname="SIS", Lname="basicL",
            nPatches=3, HPop=HPop, membership=membership,
            MYZopts=MYZo, calK=calK,
            Xopts=Xo, residence=1:3, searchB=searchWtsH,
            TimeSpent=TaR, searchQ=rep(1,3), Lopts=Lo) -> itn_mod

  itn_mod <- xds_solve(itn_mod, 730)

  out0 <- itn_mod$outputs$last_y


  M_sim0 <- out0[itn_mod$ix$MYZ[[1]]$M_ix]
  Y_sim0 <- out0[itn_mod$ix$MYZ[[1]]$Y_ix]
  I_sim0 <- out0[itn_mod$ix$X[[1]]$I_ix]

  as.vector(M_sim0) - as.vector(M) +1
  as.vector(Y_sim0) - as.vector(Y) +1

  set.seed(23)
nPatches <- 3
nStrata <- nPatches
nHabitats <- nPatches
residence <- c(1:nStrata)
membership <- c(1:3)
HPop <- rpois(n = nPatches, lambda = 1000)

# human parameters
b <- 0.55
c <- 0.15
r <- 1/200
Xo = list(b=b,c=c,r=r)
class(Xo) <- "SIS"

foi <- rlnorm(nStrata, 3/365, .5)/365
xde_steady_state_X(foi, HPop, Xo) -> ssI
I <- ssI$I

eir <- foi/b
Xo$I=I

searchWtsH = rep(1,3)

TaR <- matrix(
  data = c(
    0.9, 0.05, 0.05,
    0.05, 0.9, 0.05,
    0.05, 0.05, 0.9
  ), nrow = nStrata, ncol = nPatches, byrow = T
)
TaR <- t(TaR)

f <- rep(0.3, nPatches)
q <- rep(0.9, nPatches)
g <- rep(1/10, nPatches)
mu <- rep(0, nPatches)
sigma <- rep(1/100, nPatches)
nu <- rep(1/2, nPatches)
eggsPerBatch <- 30
eip <- 11
MYZo = list(f=f, q=q, g=g, sigma=sigma, mu=mu,
            nu=nu, eggsPerBatch=eggsPerBatch, eip=eip)

calK = create_calK_herethere(nPatches)
calK

W <- compute_W(searchWtsH, HPop, TaR)
beta <- compute_beta(HPop, W, searchWtsH, TaR)
kappa <- compute_kappa(W, W, beta, c*I)

Omega <- compute_Omega_xde(g, sigma, mu, calK)
Omega_inv <- solve(Omega)
Upsilon <- expm::expm(-Omega * eip)
Upsilon_inv <- expm::expm(Omega * eip)

fqZ <- eir2fqZ(eir, beta)

# equilibrium solutions
Y <-  Upsilon_inv %*% diag(1/as.vector(f*q), nPatches) %*% fqZ
M <- diag(1/as.vector(f*q*kappa), nPatches) %*% (diag(as.vector(f*q*kappa)) + Omega) %*% Y
Lambda <- as.vector(Omega %*% M)

MYZo$M=M
MYZo$Y=Y

psi <- 1/10
phi <- 1/12
eta <- M * nu * eggsPerBatch

alpha <- Lambda
L <- alpha/psi
theta <- (eta - psi*L - phi*L)/(L^2)

# adult mosquito parameters

Lo = list(psi=psi, phi=phi, theta=theta, L=L)



xds_setup(MYZname="SI", Xname="SIS", Lname="basicL",
          nPatches=3, HPop=HPop, membership=membership,
          MYZopts=MYZo, calK=calK,
          Xopts=Xo, residence=1:3, searchB=searchWtsH,
          TimeSpent=TaR, searchQ=rep(1,3), Lopts=Lo) -> itn_mod

itn_mod <- xds_solve(itn_mod, 730)

out0 <- itn_mod$outputs$last_y


M_sim0 <- out0[itn_mod$ix$MYZ[[1]]$M_ix]
Y_sim0 <- out0[itn_mod$ix$MYZ[[1]]$Y_ix]
I_sim0 <- out0[itn_mod$ix$X[[1]]$I_ix]


make_kappa(5*365, out0, itn_mod) -> itn_mod
kappa <- itn_mod$kappa[[1]]
Emergence(5*365, out0, itn_mod) -> itn_mod
Lambda <- itn_mod$Lambda[[1]]
Ln = list(Lambda=Lambda)

cov_opts <- list()
cov_opts$mean = 0.7
cov_opts$F_season = F_flat

es <- with(MYZo,
          sapply(1:nPatches, compute_bednet_effect_sizes_lemenach, phi=0.7,
                f=f,q=q, g=g, tau0_frac = c(0.68/3, 2.32/3), rr = 0.56, ss = 0.03))


# Turn the X component into a trivial function
Xn = list(kappa=kappa, H=HPop)

# The parameter values to the values they should reach under control
MYZn <- MYZo
class(MYZn) <- "SI"
MYZn$f <- MYZo$f*es[1,]
MYZn$q <- MYZo$q*es[2,]
MYZn$g <- MYZo$g*es[3,]
MYZn$calK <- calK
MYZn$Omega <- with(MYZn, compute_Omega_xde(g, sigma, mu, calK))

# Compute the steady state the model is expected to reach
ss <- xde_steady_state_MYZ(Lambda, kappa, MYZn)

# The model will be calibrated with the original values,
# but steady states under control
MYZo$M <- ss$M
MYZo$Y <- ss$Y


xds_setup(MYZname="SI", Xname="trivial", Lname="trivial",
          nPatches=3, HPop=HPop, membership=membership,
          MYZopts=MYZo, calK=calK,
          Xopts=Xn, residence=1:3, searchB=searchWtsH,
          searchQ=rep(1,3), Lopts=Ln) -> itn_mod_trivial


itn_mod_trivial <- xds_setup_bednets(itn_mod_trivial,
                             coverage_name = "func", coverage_opts = cov_opts,
                             effectsizes_name = "lemenach")

itn_mod_trivial <- xds_solve(itn_mod_trivial, 1000, 1)

out <- as.vector(unlist(itn_mod_trivial$outputs$last_y))
M_sim <- out[itn_mod_trivial$ix$MYZ[[1]]$M_ix]
Y_sim <- out[itn_mod_trivial$ix$MYZ[[1]]$Y_ix]

expect_equal(as.vector(M_sim0), as.vector(M), tolerance = numeric_tol)
expect_equal(as.vector(Y_sim0), as.vector(Y), tolerance = numeric_tol)
expect_equal(as.vector(M_sim), as.vector(ss$M), tolerance = numeric_tol)
expect_equal(as.vector(Y_sim), as.vector(ss$Y), tolerance = numeric_tol)

})

