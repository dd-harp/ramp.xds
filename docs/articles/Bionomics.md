# Mosquito Bionomics

In **`ramp.xds,`** mosquito bionomics are handled as a junction: each
parameter describing adult mosquito ecology or infection dynamics is
handled as a *port:*

- During *basic setup,* in **`ramp.xds`**, the parameters are assigned
  constant values.

- Some parameters can be time-dependent to model forcing by weather or
  resource availability. Forcing is an *advanced setup* option. Advanced
  setup options are in **`ramp.forcing`**

- Various modes of vector control are handled as another set of
  *advanced setup* options: advanced setup options for vector control
  are in **`ramp.control.`** Vector control effects can be handled in
  one of two ways:

  - vector control can affect the availability of resources, and the
    effects are computed through functional forms in response to
    available resources

  - vector control effect sizes can be computed.

Each module in **`ramp.xds`** or in the satellite package
**`ramp.library`** must be capable of handling time-varying parameters,
or it should report that does not. For example, the module `macdonald`
is not capable of handling either forcing or vector control. We would
say that it is a strictly autonomous model: those capabilities are not
in its *skill set.* To handle forcing and vector control, we developed
`GeM.`

## Mosquito Dispersal

In **`ramp.xds`**, mosquito dispersal is understood conceptually as the
result of a single flight bout. Regardless of how well a mosquito can
fly without the help of wind, wind ends up being an important factor
affecting mosquito dispersal, and it is possible for a mosquito to
travel reasonably long distances with the help of the wind.

In adult mosquito modules, mosquito dispersal is described by a square
matrix with `nPatches` rows and columns. By convention, all matrices
have the form: \\ K = \left\[ \begin{array}{ccccc} -1 & k\_{1,2} &
k\_{1,3} & \cdots & k\_{1, n} \\ k\_{2,1} & -1 & k\_{2,3} & \cdots &
k\_{2, n} \\ k\_{3,1} & k\_{3,2} & -1 & \cdots & k\_{3, n} \\ \vdots &
\vdots & \vdots & \ddots & \vdots \\ k\_{n,1} & k\_{n,2} & k\_{n,3} &
\cdots & -1 \\ \end{array} \right\] \\ The values of the elements are
thus constrained such that there is no net dispersal loss from the
system: \\\sum_j k\_{i,j} = 1.\\ The columns of sum up to zero. A
dispersal matrix, \\K,\\ thus describes the destinations of emigrating
mosquitoes that survive and stay in the system. Mosquito loss from a
system due to emigration is handled elsewhere.

## Mosquito Demography

Mosquito survival and dispersal is described by a *demographic matrix,*
denoted It is computed using several parameters:

- \\g\\ — the daily mosquito mortality rate
- \\\sigma\\ — the patch emigration rate
- \\\mu\\ — the rate of emigration-related loss
- \\K\\ — a dispersal matrix (see above)

These terms were designed to make it possible to develop and calibrate
models for mosquito mortality that make it easy to specify *how*
mosquitoes are lost from a system. The term \\\mu\\ describes loss from
a system that is associated with emigration: The emigration rate is
\\\sigma,\\ but only a fraction \\\sigma(1-\mu)\\ land in an other
patch. The remaining \\\sigma \mu\\ are lost from the system. Mosquito
mortality and migration are computed as a single object, \\\Omega,\\
where

\\ \Omega = \mbox{diag} \left( g + \sigma \mu \right) - K \cdot
\mbox{diag} \left( \sigma \left(1-\mu\right) \right) \\ and in general:

\\ \frac{dM}{dt} = \Lambda - \Omega \cdot M\\

In delay differential equations with a constant EIP (\\\tau\\), survival
and dispersal through the EIP is given by: \\ \Upsilon = e^{-\Omega
\tau} \\ which can be computed in R using `expm.`

## Setup

See `mosquito_dispersal.R`

``` r

library(ramp.xds)
```

By default, \\\sigma=\mu=0\\ and \\K\\ is a matrix of all zeros.

``` r

mod <- xds_setup(nPatches=3)
get_K_matrix(mod)
```

    ##      [,1] [,2] [,3]
    ## [1,]    0    0    0
    ## [2,]    0    0    0
    ## [3,]    0    0    0

### `herethere`

Another simple option is `herethere` that distributes mosquitoes to
every other patch with equal probability. The \\K\\ matrix can be
reconfigured after setup using `setup_K_matrix`

``` r

mod <- setup_K_matrix("herethere", mod) 
get_K_matrix(mod)
```

    ##      [,1] [,2] [,3]
    ## [1,] -1.0  0.5  0.5
    ## [2,]  0.5 -1.0  0.5
    ## [3,]  0.5  0.5 -1.0

The same options can be passed as a named list `Koptions` during basic
setup:

``` r

mod <- xds_setup(nPatches=6, Koptions = "herethere")
get_K_matrix(mod)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6]
    ## [1,] -1.0  0.2  0.2  0.2  0.2  0.2
    ## [2,]  0.2 -1.0  0.2  0.2  0.2  0.2
    ## [3,]  0.2  0.2 -1.0  0.2  0.2  0.2
    ## [4,]  0.2  0.2  0.2 -1.0  0.2  0.2
    ## [5,]  0.2  0.2  0.2  0.2 -1.0  0.2
    ## [6,]  0.2  0.2  0.2  0.2  0.2 -1.0

### `xy`

If the patches have locations, given by a pair of vectors \\x\\ and
\\y,\\ each one of length `nPatches` and passed concatenated as `xy`,
then a dispersal matrix \\K\\ is computed by passing a *kernel* function
that weights by distance. Setup calls `make_K_matrix`

``` r

x = y = c(1:5)
xy = cbind(x=x,y=y)
F_d = function(d){exp(-d^1.8)}
K <- make_K_matrix_xy(xy, F_d)
round(K*1000)/1000
```

    ##    [,1]   [,2]   [,3]   [,4]  [,5]
    ## 1 -1.00  0.498  0.005  0.000  0.00
    ## 2  0.99 -1.000  0.495  0.005  0.00
    ## 3  0.01  0.498 -1.000  0.498  0.01
    ## 4  0.00  0.005  0.495 -1.000  0.99
    ## 5  0.00  0.000  0.005  0.498 -1.00

``` r

mod <- xds_setup(nPatches=5, Koptions = list(name = "xy", xy=xy, ker=F_d))
get_K_matrix(mod)
```

    ##            [,1]          [,2]         [,3]          [,4]          [,5]
    ## 1 -1.000000e+00  4.975756e-01  0.004820927  4.489989e-06  9.533409e-10
    ## 2  9.903493e-01 -1.000000e+00  0.495179073  4.844259e-03  8.936647e-06
    ## 3  9.641768e-03  4.975756e-01 -1.000000000  4.975756e-01  9.641768e-03
    ## 4  8.936647e-06  4.844259e-03  0.495179073 -1.000000e+00  9.903493e-01
    ## 5  9.533409e-10  4.489989e-06  0.004820927  4.975756e-01 -1.000000e+00

### `as_matrix`

Finally, if the user wants to form some other kind of matrix, it can be
passed directly as `Koptions`:

``` r

mod <- xds_setup(nPatches=5, Koptions=K)
get_K_matrix(mod)
```

    ##            [,1]          [,2]         [,3]          [,4]          [,5]
    ## 1 -1.000000e+00  4.975756e-01  0.004820927  4.489989e-06  9.533409e-10
    ## 2  9.903493e-01 -1.000000e+00  0.495179073  4.844259e-03  8.936647e-06
    ## 3  9.641768e-03  4.975756e-01 -1.000000000  4.975756e-01  9.641768e-03
    ## 4  8.936647e-06  4.844259e-03  0.495179073 -1.000000e+00  9.903493e-01
    ## 5  9.533409e-10  4.489989e-06  0.004820927  4.975756e-01 -1.000000e+00

### `zero`

An option is `zero` which sets the \\K\\ matrix to all zeros.

``` r

mod <- setup_K_matrix("zero", mod)
get_K_matrix(mod)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    0    0    0    0    0
    ## [2,]    0    0    0    0    0
    ## [3,]    0    0    0    0    0
    ## [4,]    0    0    0    0    0
    ## [5,]    0    0    0    0    0
