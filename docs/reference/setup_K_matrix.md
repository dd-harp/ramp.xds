# Setup a mosquito dispersal matrix

The advanced set up function for the mosquito dispersal matrix (see
[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)).

Available methods are:

- "as_matrix" — sets up a passed matrix

- "herethere" — calls
  [make_K_matrix_herethere](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_herethere.md)

- "xy" – calls
  [make_K_matrix_xy](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_xy.md)

## Usage

``` r
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

## Note

The function was designed to work using arguments passed to
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md):

- if `name` is a string, then it is handled as a method name

- if `name` is a matrix, then `as_matrix` is dispatched

- `options` is a named list that sets method-specific parameters

Pre-dispatch cases were developed to make the function call more
flexible: any matrix can be passed as the first argument: or the user
could set up an options list ansd pass it (*e.g.* `Koptions` is passed
to `setup_K_matrix` in `xds_setup`). The pre-dispatch parsing:

- if `name` is a method name, set `class(options) = "name"`

- if `name` is a matrix, set `class(options) = "as_matrix"`

- if `name` is a list of options, then `list` is dispatched

Several methods are used during basic setup:

- "list" — for options lists

- "zero" — sets up the zero matrix

- "no_setup" — returns the **`xds`** object without modification

## See also

[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
