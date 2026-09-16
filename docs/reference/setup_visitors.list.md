# Set up a visitors

If an options list is passed as the first argument, then set

- `Vname = name$name`

- `options = name` and call `setup_visitors(Vname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_visitors(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the host species index

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
