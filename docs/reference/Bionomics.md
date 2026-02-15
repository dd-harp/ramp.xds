# Set bionomic parameter rates relative to baseline

This calls Mbionomics and Lbionmics for each species. This function
resets bionomic parameters to their pre-control baseline value, which
can later be modified by vector control. In some models, the pre-control
baseline is computed in here as a function of resource availability.

## Usage

``` r
Bionomics(t, y, pars)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [list](https://rdrr.io/r/base/list.html)
