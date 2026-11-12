# **`ramp.xds`**: Change Parameters

In **`ramp.xds`**, the parameter in one of the dynamical components can
can be set during basic setup or changed after basic setup. These
functions require setting up *options*, a named list. There are several
ways to learn the names of the parameters for one of the modules:

- Set up a model without passing any arguments and then use:

  - [get_XH_pars](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md)
    to examine the **XH** module parameter names and defaults

  - [get_MY_pars](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.md)
    to examine the **MY** module parameter names and defaults

  - [get_L_pars](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.md)
    to examine the **L** module parameter names and defaults

- Read the module documentation and follow the links to the module
  documentation: see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)
  or
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md)
  or
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)

- To change the value of a parameter, pass the new values of the
  parameter in a named list to:

  - [change_XH_pars](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md)
    for **XH** module parameters

  - [change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
    for **MY** module parameters

  - [change_L_pars](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.md)
    for **L** module parameters

- The built-in functions that set up modules can also be used to change
  parameters. In general, these functions should not be used after basic
  setup because they will overwrite other changes, including biting
  weights and the time spent matrix:

  - [setup_XH_obj](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.md)
    for the **XH** module

  - [setup_MY_obj](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.md)
    for the **MY** module

  - [setup_L_obj](https://dd-harp.github.io/ramp.xds/reference/setup_L_obj.md)
    for the **L** module
