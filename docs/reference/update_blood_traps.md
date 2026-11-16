# Update the blood_traps

Port function for the blood_traps, \\(\Theta\\). Dispatches on
`class(xds_obj$patches$blood_traps_obj[[s]])`.

## Usage

``` r
update_blood_traps(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object
