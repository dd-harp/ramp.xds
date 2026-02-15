# Develop a mosquito dispersal matrix from a kernel and xy-coordinates

Implements
[setup_TimeSpent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md)
for kernels

## Usage

``` r
# S3 method for class 'xy'
setup_TimeSpent(TimeSpent, xds_obj, i, options = list())
```

## Arguments

- TimeSpent:

  a matrix or a setup function name

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

- options:

  configuration options

## Value

a [list](https://rdrr.io/r/base/list.html)
