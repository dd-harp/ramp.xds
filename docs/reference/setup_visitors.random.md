# Set up visitors

Implements the "random" case for
[setup_visitors](https://dd-harp.github.io/ramp.xds/reference/setup_visitors.md).
See
[make_visitors_random](https://dd-harp.github.io/ramp.xds/reference/make_visitors_random.md).

## Usage

``` r
# S3 method for class 'random'
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
