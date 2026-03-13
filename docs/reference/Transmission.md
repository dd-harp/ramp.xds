# Transmission

Transmission computes:

- `beta` – the bite distribution matrix

- `eir` – the daily EIR

- `kappa` – the net infectiousness

This method dispatches on the type of `xds_obj$beta`

## Usage

``` r
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

an **`xds`** object
