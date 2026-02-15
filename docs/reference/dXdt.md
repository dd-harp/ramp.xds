# Compute Derivatives for the **X** Component

Compute and return the derivatives for an **X**-Component module.

## Usage

``` r
dXdt(t, y, pars, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

the derivatives, a [numeric](https://rdrr.io/r/base/numeric.html) vector

## Note

The function dispatches on `class(Xpar[[i]]).`
