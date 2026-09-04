# Set up a time spent matrix

If an options list is passed as the first argument, then set

- `TSname = name$name`

- `options = name` and call
  `setup_timespent(TSname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_timespent(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a matrix or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
