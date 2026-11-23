# Compute the mosquito dispersal matrix (**MY**)

Compute the mosquito dispersal matrix by dispatching on the class of the
port object `K_obj`.

## Usage

``` r
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

## Note

The port object is called `K_obj`
