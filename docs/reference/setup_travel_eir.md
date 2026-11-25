# Set up travel eir

- `travel_eir` or \\(E\_\delta\\):

  the average daily EIR experienced during travel by each population
  stratum

## Usage

``` r
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

## Note

Travel EIR is stored in `xds_obj$XH_obj[[i]]$travel_eir`.

## Setup methods

- `as_is` uses a supplied travel EIR vector.

- `random` generates a random travel EIR value for each population
  stratum.

## See also

[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[get_travel_eir](https://dd-harp.github.io/ramp.xds/reference/get_travel_eir.md),
[change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_eir.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_travel_eir](https://dd-harp.github.io/ramp.xds/reference/update_travel_eir.md)
