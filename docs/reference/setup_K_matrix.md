# Setup Mosquito Dispersal Matrix

A flexible function to set up or change the mosquito dispersal matrix
(see
[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)).

The function was designed to dispatch on the first argument, `name`:

- `name` is a method name

- `options` is a named list that sets the parameters in a function
  `make_K_matrix_name`

- Before dispatching, the function sets `class(option) = "name"`

Pre-dispatch cases were developed to make the function call more
flexible: any matrix can be passed as the first argument: or the user
could set up an options list and pass it (*e.g.* `Koptions` is passed to
`setup_K_matrix` in `xds_setup`). The pre-dispatch parsing:

- if `name` is a method name, set `class(options) = "name"`

- if `name` is a matrix, set `class(options) = "as_matrix"`

- if `name` is a list of options,

Available methods are:

- "as_matrix" — sets up the matrix

- "herethere" — calls
  [make_K_matrix_herethere](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_herethere.md)

- "xy" – calls
  [make_K_matrix_xy](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_xy.md)

- "list" — for options lists

- "zero" — sets up the zero matrix

- "no_setup" — returns the **`xds`** object without modification

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
