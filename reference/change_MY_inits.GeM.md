# Change initial values for `GeM` (**MY**)

For the `GeM` module (**MY**) for the \\s^{th}\\ species, change initial
values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'GeM'
change_MY_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** model object

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes
