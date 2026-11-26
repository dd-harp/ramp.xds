# Get the travel eir

Get the travel eir

## Usage

``` r
get_travel_eir(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

the travel eir, a vector of length nStrata

## Note

Travel EIR is stored in `xds_obj$XH_obj[[i]]$travel_eir`.

## See also

[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md),
[change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_EIR.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md)
