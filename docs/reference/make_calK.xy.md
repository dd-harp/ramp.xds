# Develop a mosquito dispersal matrix from a kernel and xy-coordinates

Implements
[make_calK](https://dd-harp.github.io/ramp.xds/reference/make_calK.md)
for kernels

## Usage

``` r
# S3 method for xy
make_calK(calK = "xy", s, pars, opts = list())
```

## Arguments

- calK:

  a matrix or a string

- s:

  the vector species index

- pars:

  an `xds` object

- opts:

  a list of options to configure calK

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
