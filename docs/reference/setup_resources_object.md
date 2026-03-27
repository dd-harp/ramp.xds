# Setup Resources Object

Resources is a junction. When set to its default class (`none`), all
resource parameters (blood hosts, habitat weights, traps, sugar) retain
their static zero-initialized defaults. Dynamic port machinery is
provided by satellite packages (e.g., `ramp.forcing`).

## Usage

``` r
setup_resources_object(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object
