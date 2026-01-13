# Compute Derivatives for **L** module `basicL`

This implements differential equation model for aquatic mosquito
ecology. The equations have been modified slightly from a version
published by Smith DL, *et al.* (2013).

**Variables:**

- \\L\\: the density of mosquito larvae in each habitat

**Input Term:**

- \\\eta\\ or `eta`: egg deposition rate (from
  [F_eggs](https://dd-harp.github.io/ramp.xds/reference/F_eggs.md))

**Parameters:**

- \\\psi\\ or `psi`: maturation rate

- \\\xi\\ or `xi`: delayed maturation parameter in response to mean
  crowding

- \\\phi\\ or `phi`: density-independent death rate

- \\\theta\\ or `theta`: the slope of the mortality rate in response to
  mean crowding

**Dynamical System:**

\$\$dL/dt = \eta - (\psi\\e^{-\xi L} + \phi + \theta L)L\$\$

**Output Term:**

- The function
  [F_emerge](https://dd-harp.github.io/ramp.xds/reference/F_emerge.md)
  computes the net emergence rate (\\\alpha\\):

\$\$\alpha = \psi e^{-\xi L} L \$\$

**Regulation:**

In this model, population is regulated in two ways. First, per-capita
mortality increases with mean crowding; per-capita mortality is \\\phi +
\theta L.\\ Second, maturation is delayed in response to mean crowding
\\\psi\\e^{-\xi L.}\\ Depending on the values of \\\xi,\\ productivity
in some habitats might not be a monotonically increasing function of egg
laying.

The model by Smith DL, *et al.* (2013) did not include delayed
maturation; that model is recovered by setting \\\xi=0.\\

## Usage

``` r
# S3 method for class 'basicL'
dLdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## References

Smith DL, Perkins TA, Tusting LS, Scott TW, Lindsay SW (2013). “Mosquito
population regulation and larval source management in heterogeneous
environments.” *PLoS ONE*, **8**(8), e71247.
[doi:10.1371/journal.pone.0071247](https://doi.org/10.1371/journal.pone.0071247)
.

## See also

[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md)
