# Change the blood search weights

Change the static blood search weights for host species `i` and
recompute derived blood-feeding and transmission terms.

## Usage

``` r
change_blood_search_weights(blood_search_weights, xds_obj, i = 1)
```

## Arguments

- blood_search_weights:

  a numeric vector with one weight per population stratum

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

an **`xds`** model object
