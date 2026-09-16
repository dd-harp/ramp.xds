# Update the blood_traps

Port function for the blood_traps, \\\Theta\\. Dispatches on
`class(xds_obj$XY_interface$traps_obj[[s]])`.

## Usage

``` r
update_blood_traps(t, y, xds_obj, s)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
