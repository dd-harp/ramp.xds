# Get net infectiousness of visitors

Get net infectiousness of visitors

## Usage

``` r
get_vis_kappa(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [vector](https://rdrr.io/r/base/vector.html)

## Note

Visitor net infectiousness is stored in
`xds_obj$patches$vis_kappa[[s]]`.

## See also

[visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md),
[setup_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/setup_vis_kappa.md),
[change_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/change_vis_kappa.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md)
