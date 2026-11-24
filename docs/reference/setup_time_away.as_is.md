# Set up time away

Pass a pre-configured time away vector. If it passes the checks, it
replaces the current time_away.

If called with `name = "as_is"`, the time away vector must be at
`options$time_away`.

## Usage

``` r
# S3 method for class 'as_is'
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
