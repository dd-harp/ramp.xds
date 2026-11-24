# Change Parameters

In **`ramp.xds`**, parameter values for the dynamical components can be
set during basic setup or changed after basic setup. Values to override
defaults or replace existing values are passed by name in `options` (a
[list](https://rdrr.io/r/base/list.html)). To learn the parameter names:

- Set up a model without setting `options` and then use:

  - [get_XH_pars](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)
    to examine the **XH** module parameter names and defaults

  - [get_MY_pars](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.md)
    to examine the **MY** module parameter names and defaults

  - [get_L_pars](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.md)
    to examine the **L** module parameter names and defaults

- To change parameter values, pass the new values in a named list as
  `options` to:

  - [change_XH_pars](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)
    for **XH** module parameters

  - [change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
    for **MY** module parameters

  - [change_L_pars](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md)
    for **L** module parameters

- The module setup functions reinitialize an existing module and may
  overwrite parameter changes (use with caution):

  - [setup_XH_obj](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.md)
    for the **XH** module

  - [setup_MY_obj](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.md)
    for the **MY** module

  - [setup_L_obj](https://dd-harp.github.io/ramp.xds/reference/setup_L_obj.md)
    for the **L** module

- Read the module documentation

## See also

[XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md),
[MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md),
[L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md),
[xds_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md)
