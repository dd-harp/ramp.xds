# saveRDS for `xds` Objects

Removes forcing functions (to reduce the file size) and stores the `xds`
object using `saveRDS`.

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

Forcing functions (*e.g.* `F_trend`) are stored on the `xds` object in a
form that takes up enormous space. This removes the functions before
saving.

The function
[readXDS](https://dd-harp.github.io/ramp.xds/reference/readXDS.md) reads
the saved model object and rebuilds forcing functions from their
parameter sets.

## See also

`ramp.func`
