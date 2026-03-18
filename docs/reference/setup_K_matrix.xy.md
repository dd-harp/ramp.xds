# Setup a Kernel-Based Mosquito Dispersal Matrix

Set up a mosquito dispersal matrix from a set of \\x,y\\ coordinates and
a *kernel,* a function that assigns weights by distance. The fraction
leaving from each patch that arrive at other patch is the vector of
normalized weights.

## Usage

``` r
# S3 method for class 'xy'
setup_K_matrix(Kname = "xy", xds_obj, options = list(), s = 1)
```

## Arguments

- Kname:

  a matrix or a string

- xds_obj:

  an **`xds`** model object

- options:

  a list of options to configure K_matrix

- s:

  the vector species index

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
