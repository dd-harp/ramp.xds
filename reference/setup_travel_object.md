# Setup the Travel Object

Setup the object to handle travel

- `F_travel` should have the form \\F(t,V)\\ where \\V\\ is returned by
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  dispatching on `class(F_travel)`

- `F_travel_eir` should have the form \\F(t,V)\\ where \\V\\ is returned
  by
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  dispatching on `class(F_travel)`

## Usage

``` r
setup_travel_object(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object
