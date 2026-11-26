# Set up time away

- `time_away` or \\\delta\\:

  the fraction of time spent outside the modeled area

Time away weights travel exposure in
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md)
and is used to calculate the availability of resident hosts.

## Usage

``` r
setup_time_away(name, xds_obj, options = list(), i = 1)
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

Time away is stored in `xds_obj$XH_obj[[i]]$time_away`. Setup methods
call
[change_time_away](https://dd-harp.github.io/ramp.xds/reference/change_time_away.md)
to validate the value and update the static port values. This change
function is intended for use by setup methods, not for direct use.

## Setup methods

- `as_is` uses a supplied time-away vector.

- `random` generates a random time-away value for each population
  stratum.

## See also

[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[get_time_away](https://dd-harp.github.io/ramp.xds/reference/get_time_away.md),
[change_time_away](https://dd-harp.github.io/ramp.xds/reference/change_time_away.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_time_away](https://dd-harp.github.io/ramp.xds/reference/update_time_away.md)
