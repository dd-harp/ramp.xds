# Update NI of visitors

Port function to compute the availability of vis_kappa. Dispatches on
`class(xds_obj$patches$vis_kappa_obj[[s]])`.

## Usage

``` r
update_vis_kappa(t, y, xds_obj, s)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
