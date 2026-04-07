# Update Time Away

Port function for the fraction of time spent outside the spatial domain,
`time_away`. Dispatches on
`class(xds_obj$XY_interface$time_away_obj[[i]])`.

## Usage

``` r
update_time_away(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

an **`xds`** object
