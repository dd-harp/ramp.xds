# Set up the module object (**MY**)

Set up the selected module and its interface ports for the \\s^{th}\\
vector species in the **MY** component.

- Pass `xds_obj`, `options`, and `s` to the selected setup method.

- Let that method construct the module object and configure its initial
  values and interface ports as appropriate for the module.

- Return the updated **`xds`** model object.

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

an **`xds`** model object

## Note

The object is stored as `xds_obj$MY_obj[[s]]` Dispatch is on `MYname`,
whose class is set to the supplied module name, not on an existing
module object.

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md),
[setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md),
[setup_F_circadian](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.md),
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
