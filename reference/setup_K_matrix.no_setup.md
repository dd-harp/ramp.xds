# Setup no dispersal matrix

Implements
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
for the "no_setup" case

## Usage

``` r
# S3 method for class 'no_setup'
setup_K_matrix(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a method name: or a matrix, or a list

- xds_obj:

  an **`xds`** model object

- options:

  a list of options to configure K_matrix

- s:

  the vector species index

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
