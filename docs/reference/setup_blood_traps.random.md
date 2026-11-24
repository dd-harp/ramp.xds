# Set up blood traps

Generate random blood trap availability using
[make_blood_traps_random](https://dd-harp.github.io/ramp.xds/reference/make_blood_traps_random.md).

## Usage

``` r
# S3 method for class 'random'
setup_blood_traps(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a setup method name or a numeric vector of availability values

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
