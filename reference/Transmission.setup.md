# Compute transmission, the static case

Set up and compute transmission terms with a static mixing matrix

## Usage

``` r
# S3 method for class 'setup'
Transmission(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

an `xds` object

## Details

The `setup` case is called whenever any parameter affecting the mixing
matrix in a static model is changed
