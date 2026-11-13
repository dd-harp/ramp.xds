# Set up an **L** model object

This setup creates an **L** object for the \\(s^{th}\\) species and sets
up the interface ports:

- calls `make_L_obj_`*Lname* to set up the dynamics, including
  parameters

- calls
  [setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md)
  to set initial values

- calls
  [setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md)

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

an **`xds`** object

## Note

The model object is stored as `xds_obj$L_obj[[s]]`

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[change_L_pars](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md),
[setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md),
[setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md)
