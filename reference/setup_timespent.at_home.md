# Set up a time spent matrix

Implements the "at_home" case for
[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md).
See
[make_timespent_at_home](https://dd-harp.github.io/ramp.xds/reference/make_timespent_at_home.md)

## Usage

``` r
# S3 method for class 'at_home'
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
