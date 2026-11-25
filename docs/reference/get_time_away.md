# Get time away

Get time away

## Usage

``` r
get_time_away(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata[i]`

## Note

Time away is stored in `xds_obj$XH_obj[[i]]$time_away`.

## See also

[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
[change_time_away](https://dd-harp.github.io/ramp.xds/reference/change_time_away.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md)
