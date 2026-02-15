# **MYZ** Component Derivatives for `basicM`

Implements
[dMYZdt](https://dd-harp.github.io/ramp.xds/reference/dMYZdt.md) for the
basicM xde ODE model.

## Usage

``` r
# S3 method for basicM
dMYZdt(t, y, pars, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an `xds` object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## Details

\$\$\begin{array}{rl}dM/dt &= \Lambda(t) - \Omega \cdot M \\ dP/dt &=
f(M-P) - \Omega\cdot P\end{array}\$\$
