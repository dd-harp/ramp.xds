# Set up a time spent matrix

Available methods are:

- `as_matrix` — use a matrix provided

- `at_home` — a fraction at home, or evenly among the rest (see
  [make_timespent_at_home](https://dd-harp.github.io/ramp.xds/reference/make_timespent_at_home.md))

- `xy` — use patch locations and a kernel function (see
  [make_timespent_xy](https://dd-harp.github.io/ramp.xds/reference/make_timespent_xy.md))

If the first argument is a matrix, the `as_matrix` method is used.

## Usage

``` r
setup_timespent(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a matrix or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** object
