# saveRDS for `xds` Objects

Removes forcing functions from the `EIR_obj` (to reduce the size) and
stores the `xds` object using `saveRDS`

## Usage

``` r
# S3 method for class 'MY'
saveXDS(xds_obj, filename)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- filename:

  the file name

## Value

invisible()
