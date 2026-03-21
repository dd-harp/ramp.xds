# Compute derivatives for `basicM` (**MY**)

Implements
[dMYdt](https://dd-harp.github.io/ramp.xds/reference/dMYdt.md) for the
basicM xde ODE model.

## Usage

``` r
# S3 method for class 'basicM'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## Details

\$\$\begin{array}{rl}dM/dt &= \Lambda(t) - \Omega \cdot M \\ dP/dt &=
f(M-P) - \Omega\cdot P\end{array}\$\$
