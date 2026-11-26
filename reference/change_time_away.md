# Change time away

Update time away for the \\i^{th}\\ host species, and trigger updates
for the `XY` interface.

## Usage

``` r
change_time_away(time_away, xds_obj, i = 1)
```

## Arguments

- time_away:

  a time away vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

an **`xds`** model object

## See also

[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
[get_time_away](https://dd-harp.github.io/ramp.xds/reference/get_time_away.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md)
