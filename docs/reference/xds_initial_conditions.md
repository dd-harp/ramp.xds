# Change Initial Conditions

In **`ramp.xds`**, initial values for all the dependent variables are
assigned default values and stored during basic setup. Values to
override defaults or replace existing values are passed by name to
`options` in a [list](https://rdrr.io/r/base/list.html). To learn the
variable names:

- Set up a model without setting `options` and then use:

  - [get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
    to examine the stored initial conditions for the **XH** module

  - [get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
    to examine the stored initial conditions for the **MY** module

  - [get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md)
    to examine the stored initial conditions for the **L** module

- To change the value of one or more variables, pass the new values by
  name in a list `options` to:

  - [change_XH_inits](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md)
    for the **XH** module variables

  - [change_MY_inits](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.md)
    for the **MY** module variables

  - [change_L_inits](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.md)
    for the **L** module variables

- Module setup functions replace the existing initial values (use with
  caution):

  - [setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)
    for the **XH** module

  - [setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md)
    for the **MY** module

  - [setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md)
    for the **L** module

- Read the module documentation

Two useful utilities for initial values are:

- [last_to_inits](https://dd-harp.github.io/ramp.xds/reference/last_to_inits.md)
  — after calling
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md),
  replace the initial values with the final values

- [burnin](https://dd-harp.github.io/ramp.xds/reference/burnin.md) —
  runs the model from `t_neg_inf` to 0 and then calls
  [last_to_inits](https://dd-harp.github.io/ramp.xds/reference/last_to_inits.md)

## See also

[XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md),
[MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md),
[L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md),
[xds_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md)
