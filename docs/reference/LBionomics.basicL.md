# Bionomics for `basicL` (**L** Component)

Compute the bionomic parameter values for `basicL` by calling the
forcing functions for each parameter. Also resets all effect sizes
(`es_psi`, `es_phi`, `es_xi`, `es_theta`) to 1.

## Usage

``` r
# S3 method for class 'basicL'
LBionomics(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** object
