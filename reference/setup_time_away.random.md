# Set up time away

- `time_away` or \\\delta\\:

  the fraction of time spent outside the modeled area

Time away weights travel exposure in
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md)
and is used to calculate the availability of resident hosts.

## Usage

``` r
# S3 method for class 'random'
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

## Details

Set up random values for time away

## See also

[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[get_time_away](https://dd-harp.github.io/ramp.xds/reference/get_time_away.md),
[change_time_away](https://dd-harp.github.io/ramp.xds/reference/change_time_away.md),
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[update_time_away](https://dd-harp.github.io/ramp.xds/reference/update_time_away.md)
