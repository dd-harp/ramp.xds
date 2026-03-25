# Dynamical Components

In basic setup, modules are selected by passing the name as a character
string:

- `Xname`:

  **X** Component module name: see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)

- `MYname`:

  **MY** Component module name: see
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md)

- `Lname`:

  **L** Component module name: see
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md)

## Modules

Dynamical systems for malaria and other mosquito-borne diseases are made
up of three core **dynamical components** that describe five core
processes. There is also a generic interface to add other variables:

- **XH** - a system of equations describing two inextricably linked
  processes:

  - **X** - the dynamics of host infection and immunity

  - **H** - human / host demography and behavior

- **MY** - a system of equations describing two inextricably linked
  processes:

  - **M** - adult mosquito ecology

  - **Y** - parasite / pathogen infection dynamics in mosquitoes

- **L** - a system of equations describing immature mosquito ecology

- **V** - other dynamical components: see
  [Other_State_Variables](https://dd-harp.github.io/ramp.xds/reference/Other_State_Variables.md)

## See also

[xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)
