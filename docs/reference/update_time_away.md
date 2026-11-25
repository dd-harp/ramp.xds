# Update time away

Port function for time away. Dispatches on
`class(xds_obj$XH_obj[[i]]$time_away_obj)`.

## Usage

``` r
update_time_away(t, xds_obj, i)
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

[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[importation_dynamics](https://dd-harp.github.io/ramp.xds/reference/importation_dynamics.md)
