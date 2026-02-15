# Update Initial Values for `basicL` from a state vector `y`

Extract the values of the variable \\L\\ from a state vector `y` and use
them to set the initial value for \\L\\

## Usage

``` r
# S3 method for basicL
update_Linits(pars, y, s)
```

## Arguments

- pars:

  an **`xds`** object

- y:

  the state variables

- s:

  the species index

## Value

an **`xds`** object
