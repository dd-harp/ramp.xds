# Set up visitors

Set up visitors using an options list.

## Usage

``` r
# S3 method for class 'list'
setup_visitors(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a numeric vector, a setup function name, or an options list

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

If an options list is passed as the first argument, then set

- `Vname = name$name`

- `options = name` and call
  `setup_visitors(Vname, xds_obj, options, s)`.
