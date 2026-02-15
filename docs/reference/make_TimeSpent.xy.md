# Develop a mosquito dispersal matrix from a kernel and xy-coordinates

Implements
[make_TimeSpent](https://dd-harp.github.io/ramp.xds/reference/make_TimeSpent.md)
for kernels

## Usage

``` r
# S3 method for xy
make_TimeSpent(pars, i, TimeSpent = "xy", opts = list())
```

## Arguments

- pars:

  an [list](https://rdrr.io/r/base/list.html)

- i:

  the host species index

- TimeSpent:

  a matrix; or a string

- opts:

  a list of options to configure TimeSpent

## Value

a [list](https://rdrr.io/r/base/list.html)
