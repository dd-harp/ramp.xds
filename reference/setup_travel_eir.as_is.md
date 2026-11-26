# Set up a travel eir

- `travel_eir` or \\E\_\delta\\:

  the average daily EIR experienced during travel by each population
  stratum

## Usage

``` r
# S3 method for class 'as_is'
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

## Details

Pass a pre-configured travel eir vector. If it passes the checks, it
replaces the current travel_eir.

If called with `name = "as_is"`, the travel eir must be at
`options$travel_eir`.

## See also

[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[get_travel_eir](https://dd-harp.github.io/ramp.xds/reference/get_travel_eir.md),
[change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_EIR.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_travel_eir](https://dd-harp.github.io/ramp.xds/reference/update_travel_eir.md)
