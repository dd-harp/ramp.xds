# Update habitat search weights

Port function for the habitat_search_weights, \\(w\\). Dispatches on
`xds_obj$L_obj[[s]]$search_obj`.

## Usage

``` r
update_habitat_search_weights(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** model object
