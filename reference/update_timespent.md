# Update the time spent Matrix

Port function for the time spent matrix, \\\Theta\\. Dispatches on
`class(xds_obj$XH_obj[[i]]$timespent_obj)`.

## Usage

``` r
update_timespent(t, y, xds_obj, i)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

an **`xds`** model object
