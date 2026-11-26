# Get availability of visitors

Get availability of visitors

## Usage

``` r
get_visitors(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [vector](https://rdrr.io/r/base/vector.html)

## Note

Visitor availability is stored in `xds_obj$patches$visitors[[s]]`.

## See also

[visitor_availability](https://dd-harp.github.io/ramp.xds/reference/visitor_availability.md),
[setup_visitors](https://dd-harp.github.io/ramp.xds/reference/setup_visitors.md),
[change_visitors](https://dd-harp.github.io/ramp.xds/reference/change_visitors.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md)
