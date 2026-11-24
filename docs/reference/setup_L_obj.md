# Set up the module object (**L**)

Set up the selected module and its interface ports for the \\(s^{th}\\)
vector species in the **L** component.

- Pass `membership`, `xds_obj`, `options`, and `s` to the selected setup
  method.

- Let that method construct the module object and configure its initial
  values and habitat interface as appropriate for the module.

- Return the updated **`xds`** model object.

The function is typically called by
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
but it is also used to add vector species.

## Usage

``` r
setup_L_obj(Lname, membership, xds_obj, options = list(), s = 1)
```

## Arguments

- Lname:

  the class name of the **L** module

- membership:

  the habitats' patch membership vector

- xds_obj:

  an **`xds`** model object

- options:

  a named list to configure **`L_obj`**

- s:

  the species index

## Value

an **`xds`** model object

## Note

The module object is stored at `xds_obj$L_obj[[s]]`. Dispatch is on
`Lname`, whose class is set to the supplied module name, not on an
existing module object.

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[change_L_pars](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md),
[setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md),
[setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md)
