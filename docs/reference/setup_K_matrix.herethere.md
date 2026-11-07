# Setup a here-and-there dispersal matrix

Implements
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
for the `herethere` method. See
[make_K_matrix_herethere](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_herethere.md).

## Usage

``` r
# S3 method for class 'herethere'
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

an **`xds`** object

## See also

[make_K_matrix_herethere](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_herethere.md)
and
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
