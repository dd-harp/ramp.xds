# Update Blood Search Weights

Port function for blood feeding search weights, \\\omega\\. Dispatches
on `class(xds_obj$XH_obj[[i]]$search_obj)`.

## Usage

``` r
update_search_weights(xds_obj, s, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

- i:

  the host species index

## Value

an **`xds`** object
