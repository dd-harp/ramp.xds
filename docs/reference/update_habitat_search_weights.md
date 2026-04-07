# Update habitat search weights

Port function for habitat feeding search weights, \\\omega\\. Dispatches
on `class(xds_obj$L_obj[[s]]$search_obj)`.

## Usage

``` r
update_habitat_search_weights(xds_obj, s)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
