# Update bad habitat availability

Port function for the bad_habitats, \\\Theta\\. Dispatches on
`class(xds_obj$patches$bad_habitats_obj[[s]])`.

## Usage

``` r
update_bad_habitats(t, y, xds_obj, s)
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
