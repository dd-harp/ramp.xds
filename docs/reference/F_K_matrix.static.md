# Return the static mosquito dispersal matrix (**MY**)

Return the mosquito dispersal matrix stored in the port object `K_obj`.

## Usage

``` r
# S3 method for class 'static'
F_K_matrix(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a mosquito dispersal [matrix](https://rdrr.io/r/base/matrix.html) of
dimensions `nPatches` by `nPatches`
