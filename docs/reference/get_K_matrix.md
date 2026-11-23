# Get the Mosquito Dispersal Matrix

Get (inspect) a mosquito dispersal matrix.

## Usage

``` r
get_K_matrix(xds_obj, which_K = "K", s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- which_K:

  which K_matrix

- s:

  the vector species index

## Value

a mosquito dispersal [matrix](https://rdrr.io/r/base/matrix.html) of
dimensions `nPatches` by `nPatches`

## Note

The argument `K` is used to retrieve state-dependent mosquito dispersal
arguments for behavioral state models. The default "K" returns
`K_matrix`
