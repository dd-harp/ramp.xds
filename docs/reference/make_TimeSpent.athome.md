# Make a mosquito dispersal matrix, called TimeSpent with a here / away

Implements
[make_TimeSpent](https://dd-harp.github.io/ramp.xds/reference/make_TimeSpent.md)
for as_matrix

## Usage

``` r
# S3 method for athome
make_TimeSpent(pars, i, TimeSpent = "athome", opts = list())
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
