# Set up time away

If an options list is passed as the first argument, then set

- `TAname = name$name`

- `options = name` and call
  `setup_time_away(TAname, xds_obj, options, i)`

## Usage

``` r
# S3 method for class 'list'
setup_time_away(name, xds_obj, options = list(), i = 1)
```

## Arguments

- name:

  a setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- i:

  the host species index

## Value

an **`xds`** model object
