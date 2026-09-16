# Update the blood_search_weights Matrix

Port function for the blood_search_weights, \\w\\. Dispatches on
`xds_obj$XH_obj[[i]]$blood_search_weights_obj`.

## Usage

``` r
update_blood_search_weights(t, y, xds_obj, i)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

an **`xds`** object
