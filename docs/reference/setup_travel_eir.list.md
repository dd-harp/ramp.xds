# Set up a travel eir

- `travel_eir` or \\(E\_\delta\\):

  the average daily EIR experienced during travel by each population
  stratum

## Usage

``` r
# S3 method for class 'list'
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

If an options list is passed as the first argument, then set

- `Teirname = name$name`

- `options = name` and call
  `setup_travel_eir(Teirname, xds_obj, options, i)`

## See also

[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[get_travel_eir](https://dd-harp.github.io/ramp.xds/reference/get_travel_eir.md),
[change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_eir.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_travel_eir](https://dd-harp.github.io/ramp.xds/reference/update_travel_eir.md)
