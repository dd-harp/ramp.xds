# Update X states for a discrete time system

This method dispatches on the type of `pars$Xpar[[i]]`.

## Usage

``` r
Update_Xt(t, y, pars, i)
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector
