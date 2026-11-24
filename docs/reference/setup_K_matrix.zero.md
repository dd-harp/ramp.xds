# Set up a zero mosquito dispersal matrix (**MY**)

Set up a zero mosquito dispersal matrix using
[change_K_matrix](https://dd-harp.github.io/ramp.xds/reference/change_K_matrix.md).

## Usage

``` r
# S3 method for class 'zero'
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

an **`xds`** model object
