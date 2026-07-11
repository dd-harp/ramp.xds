# Setup Mosquito Dispersal Matrix

A flexible setup function for mosquito dispersal. The first argument
`Kname` can be either a matrix, the name of a method, or a list of
options that includes a method name:

- if `Kname` is a string of characters, it dispatches the method

- if `Kname` is a matrix, then a matrix is set up

- if `Kname` is a list of options, then dispatching is on `Kname$name`

the method dispatches on `class(options)`

Options for `Kname` are:

- `is.matrix(Kname)`: if the user passes a matrix, then
  `class(Kname) <- "as_matrix"`

- `Kname = "no_setup"` – the **`xds`** object is returned unmodified

- `Kname = "zero"` – set up a matrix of all zeros

- `Kname = "as_matrix"` – calls
  [change_K_matrix](https://dd-harp.github.io/ramp.xds/reference/change_K_matrix.md)
  and passes `K_matrix`

- `Kname = "herethere"` – calls
  [make_K_matrix_herethere](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_herethere.md)

- `Kname = "xy"` – calls
  [make_K_matrix_xy](https://dd-harp.github.io/ramp.xds/reference/make_K_matrix_xy.md)

## Usage

``` r
setup_K_matrix(Kname, xds_obj, options = list(), s = 1)
```

## Arguments

- Kname:

  a name, a matrix, or a list

- xds_obj:

  an **`xds`** model object

- options:

  a list of options to configure K_matrix

- s:

  the vector species index

## Value

an **`xds`** object
