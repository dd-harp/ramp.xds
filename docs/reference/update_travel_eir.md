# Update the travel eir

Port function for travel EIR. Dispatches on
`class(xds_obj$XH_obj[[i]]$travel_eir_obj)`.

## Usage

``` r
update_travel_eir(t, y, xds_obj, i)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

an **`xds`** model object
