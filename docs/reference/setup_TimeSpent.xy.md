# Set up a time spent matrix

Implements
[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md)
from a set of xy coordinates and a spatial kernel. See
[make_timespent_xy](https://dd-harp.github.io/ramp.xds/reference/make_timespent_xy.md)

## Usage

``` r
# S3 method for class 'xy'
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
