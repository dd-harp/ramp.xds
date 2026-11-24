# Setup K_matrix

Set up the mosquito dispersal matrix using an options list.

## Usage

``` r
# S3 method for class 'list'
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

## Note

If an options list is passed as the first argument, use its `name` field
to select the setup method. If `name` is NULL, use the `no_setup`
method. Pass the list as `options` to
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md).
