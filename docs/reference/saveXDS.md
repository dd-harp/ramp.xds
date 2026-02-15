# saveRDS for `xds` Objects

Removes forcing functions (to reduce the file size) and stores the `xds`
object using `saveRDS`

## Usage

``` r
saveXDS(xds_obj, filename)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- filename:

  the file name

## Value

invisible()

## Note

Forcing functions (*e.g.* F_trend) are stored on the `xds` object in a
form that takes up enormous space.

Since they are created by calling
[`make_function()`](https://dd-harp.github.io/ramp.xds/reference/make_function.md)
from the stored *function objects* (*e.g.* `trend_par`), they are
redundant.

## See also

[`readXDS()`](https://dd-harp.github.io/ramp.xds/reference/readXDS.md)
