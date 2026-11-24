# Update NI of visitors

Port function to compute the net infectiousness of visitors.

## Usage

``` r
update_vis_kappa(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

Dispatches on `class(xds_obj$patches$vis_kappa_obj[[s]])`.
