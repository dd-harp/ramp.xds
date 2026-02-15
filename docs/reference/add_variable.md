# Add Variable

This is a generic method to add new dependent state variables that are
computed by `dVdt.`

The model object for the new variable should fully define an other
variables module, including methods to set up initial values.

The description for each method should include the equations.

## Usage

``` r
add_variable(var_object, xds_obj)
```

## Arguments

- var_object:

  an object to dispatch and compute a variable

- xds_obj:

  an **`xds`** model object

## Value

the **`xds`** model object with a new variable
