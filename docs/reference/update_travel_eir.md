# Update the travel eir

Port function for travel EIR. Dispatches on
`class(xds_obj$XH_obj[[i]]$travel_eir_obj)`.

## Usage

``` r
update_travel_eir(t, xds_obj, i)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

an **`xds`** model object

## See also

[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[importation_dynamics](https://dd-harp.github.io/ramp.xds/reference/importation_dynamics.md)
