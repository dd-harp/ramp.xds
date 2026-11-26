# Update NI of visitors

Port function to compute the net infectiousness of visitors.

## Usage

``` r
update_vis_kappa(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

Dispatches on `class(xds_obj$patches$vis_kappa_obj[[s]])`.

## See also

[visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md),
[importation_dynamics](https://dd-harp.github.io/ramp.xds/reference/importation_dynamics.md)
