# Update the time spent Matrix

Port function for the time spent matrix, \\\Theta\\. Dispatches on
`class(xds_obj$XY_interface$timespent_obj[[i]])`.

## Usage

``` r
update_timespent(t, y, xds_obj, i)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

an **`xds`** object
