# Update the habitat_search_weights Matrix

Port function for the habitat_search_weights, \\w\\. Dispatches on
`xds_obj$L_obj[[s]]$habitat_search_obj`.

## Usage

``` r
update_habitat_search_weights(t, y, xds_obj, s)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** object
