# Setup a Here-There Dispersal Matrix

Implements
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
for the herethere model: dispersal to every other patch, with equal
probability

## Usage

``` r
# S3 method for class 'herethere'
setup_K_matrix(Kname, s, xds_obj, options = list())
```

## Arguments

- Kname:

  a matrix or a string

- s:

  the vector species index

- xds_obj:

  an **`xds`** model object

- options:

  a list of options to configure K_matrix

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
