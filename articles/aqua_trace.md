# Emergence

## A forced model

``` r
library(ramp.xds)
library(MASS)
library(expm)
library(deSolve)
library(data.table)
library(ggplot2)
```

This is the null model of aquatic mosquito dynamics; there are no
endogenous dynamics and the model is simply specified by `Lambda`, the
rate at which adult mosquitoes spontaneously emerge from aquatic
habitats.

Below we show an example, taken from the package tests, of using the
trivial-based aquatic model to keep an adult mosquito model at
equilibrium when using unequal numbers of aquatic habitats per patch.

### The Long Way

First, we set the parameter values.

``` r
nPatches <- 3
nHabitats <- 4
membership = c(1,2,3,3)
HPop = 1000
```

``` r
calN <- matrix(0, nPatches, nHabitats)
calN[1,1] <- 1
calN[2,2] <- 1
calN[3,3] <- 1
calN[3,4] <- 1

calU <- matrix(0, nHabitats, nPatches)
calU[1,1] <- 1
calU[2,2] <- 1
calU[3:4,3] <- 0.5
```

``` r
f <- rep(0.3, nPatches)
q <- rep(0.9, nPatches)
g <- rep(1/20, nPatches)
sigma <- rep(1/10, nPatches)
mu <- rep(0, nPatches) 
eip <- 11
nu <- 1/2
eggsPerBatch <- 30
```

``` r
MYo = list(
  g=g, sigma=sigma, mu=mu, f=f, q=q, nu=nu, eggsPerBatch = eggsPerBatch
)
```

``` r
K_matrix <- matrix(0, nPatches, nPatches)
K_matrix[1, 2:3] <- c(0.2, 0.8)
K_matrix[2, c(1,3)] <- c(0.5, 0.5)
K_matrix[3, 1:2] <- c(0.7, 0.3)
K_matrix <- t(K_matrix)

Omega <- make_Omega_xde(g, sigma, mu, K_matrix)
Upsilon <- expm::expm(-Omega * eip)

kappa <- c(0.1, 0.075, 0.025)
Lambda <- c(5, 10, 8)
```

Next, we calculate equilibrium values following the [Ross-Macdonald
vignette](https://dd-harp.github.io/ramp.xds/articles/adult_RM.md). We
use \\\mathcal{N}\\ and \\\mathcal{U}\\ to describe how aquatic habitats
are dispersed amongst patches, and how mosquitoes in each patch disperse
eggs to each habitat, respectively. Please note because we have unequal
numbers of aquatic habitats and patches, we use
[`MASS::ginv`](https://rdrr.io/pkg/MASS/man/ginv.html) to compute the
generalized inverse of \\\mathcal{N}\\ to get \\\alpha\\ required at
equilibrium.

``` r
# equilibrium solutions
Omega_inv <- solve(Omega)

M_eq <- as.vector(Omega_inv %*% Lambda)
P_eq <- as.vector(solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M_eq)

# the "Lambda" for the dLdt model
alpha <- as.vector(ginv(calN) %*% Lambda)
```

Now we set up the model. Please see the [Ross-Macdonald
vignette](https://dd-harp.github.io/ramp.xds/articles/adult_RM.md) for
details on the adult model. We use `alpha` as the `Lambda` parameter of
our forced emergence model, in `ramp.xds::make_parameters_L_trivial`.
Again, because we are not running the full transmission model, we must
use `ramp.xds::MosquitoBehavior` to pass the equilibrium values of those
bionomic parameters to `ramp.xds::xDE_diffeqn_mosy`.

``` r
xds_obj <- make_xds_object_template("ode", "mosy", nPatches, membership)

xds_obj = setup_L_obj("trivial", xds_obj, 1, options = list(Lambda=alpha))

xds_obj = setup_L_inits(xds_obj, 1)

xds_obj = setup_MY_obj("basicM", xds_obj, 1, options=MYo)

xds_obj = setup_MY_inits(xds_obj, 1, list(M=M_eq, P=P_eq))
```

``` r
xds_obj = setup_XH_obj("trivial", xds_obj, 1, list(HPop=HPop))
```

``` r
xds_obj = make_indices(xds_obj)
```

``` r
xds_obj$terms$kappa = list()
xds_obj$terms$kappa[[1]] = kappa  
```

``` r
xds_obj <- change_K_matrix(K_matrix, xds_obj)
```

``` r
y0 <- get_inits(xds_obj)
y0
#> $L
#> list()
#> 
#> $MY
#> $MY$M
#> [1] 157.8680 123.4518 178.6802
#> 
#> $MY$P
#> [1] 140.35170  98.87622 155.05779
#> 
#> 
#> $X
#> NULL
```

``` r
out <- deSolve::ode(y = get_inits(xds_obj, flatten=TRUE), times = seq(0,50, by=10), func = xde_derivatives, parms = xds_obj, method = 'lsoda')
out1 <- out
```

``` r
colnames(out)[xds_obj$MY_obj[[1]]$ix$M_ix+1] <- paste0('M_', 1:xds_obj$nPatches)
colnames(out)[xds_obj$MY_obj[[1]]$ix$P_ix+1] <- paste0('P_', 1:xds_obj$nPatches)

out <- as.data.table(out)
out <- melt(out, id.vars = 'time')
out[, c("Component", "Patch") := tstrsplit(variable, '_', fixed = TRUE)]
out[, variable := NULL]

ggplot(data = out, mapping = aes(x = time, y = value, color = Patch)) +
  geom_line() +
  facet_wrap(. ~ Component, scales = 'free') +
  theme_bw()
```

![](aqua_trace_files/figure-html/unnamed-chunk-15-1.png)

### Using Setup

``` r
Lo = list(
  Lambda = alpha 
)
```

``` r
MYo = list(
  f = 0.3,
  q = 0.9,
  g = 1/20,
  sigma = 1/10,
  eip = 11,
  nu = 1/2,
  eggsPerBatch = 30,
  M=M_eq,
  P=P_eq
)
```

``` r
xds_setup_mosy(MYname = "basicM", Lname = "trivial", 
               nPatches = 3, membership = c(1,2,3,3), 
               MYoptions = MYo, K_matrix = K_matrix, HPop=HPop, Loptions = Lo, 
               kappa = c(0.1, 0.075, 0.025))->mosy1
```

``` r
xds_solve(mosy1,Tmax=50,dt=10)$outputs$deout -> out2
```

``` r
sum(abs(out2-out1))
#> [1] 0
```
