# Set up an **MY** model object

Set up creates an **MY** object for the \\(s^{th}\\) species and sets up
the interface ports:

- calls `make_MY_obj_`*MYname* to set up the dynamics, including
  parameters

- calls
  [setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md)
  to set initial values

- calls
  [setup_F_circadian](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.md)
  to set up a flat circadian function (no effect)

- calls
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
  to set up a zero matrix

It also sets up computation for module-specific objects.

The function is part of basic setup, but it can also be used to add
vector species.

## Usage

``` r
setup_MY_obj(MYname, xds_obj, options = list(), s = 1)
```

## Arguments

- MYname:

  the name of the model

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

## Value

an **`xds`** object

## Note

The object is stored as `xds_obj$MY_obj[[s]]`

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md),
[setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md),
[setup_F_circadian](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.md),
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
