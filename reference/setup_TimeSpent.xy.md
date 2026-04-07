# Develop a time spent matrix from a kernel and xy-coordinates

Implements
[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md)
for kernels

## Usage

``` r
# S3 method for class 'xy'
setup_timespent(name, xds_obj, i, options = list())
```

## Arguments

- name:

  a matrix or setup function name

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

- options:

  configuration options

## Value

an **`xds`** object
