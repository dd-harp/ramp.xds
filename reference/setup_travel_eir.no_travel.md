# Set up no travel EIR

Set travel EIR to zero for each stratum of the selected host species
using
[change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_EIR.md).

## Usage

``` r
# S3 method for class 'no_travel'
setup_travel_eir(name, xds_obj, options = list(), i = 1)
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
