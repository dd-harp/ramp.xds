# Set up blood traps from supplied values

Pass a pre-configured blood_traps. If it passes the checks, it replaces
the current blood_traps.

If called with `name = "as_is"`, the blood_traps must be at
`options$blood_traps_asis`.

## Usage

``` r
# S3 method for class 'as_is'
setup_blood_traps(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a setup method name or a numeric vector of availability values

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
