# Get the skill set (**L**)

Return the selected module's capabilities and compatibilities as a list.
Non-trivial modules include logical `<parameter>_is_port` flags for
`psi`, `phi`, `xi`, and `theta`. A flag is `TRUE` when the module
implements that parameter as a port, and `FALSE` when it is a direct
value or is absent.

## Usage

``` r
get_skillset_L(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object (or the module name)

- s:

  the species index

## Value

the **L** module skill set, as a list

## Note

Dispatch is on the class of `xds_obj$L_obj[[s]]`. If a module name is
passed instead of a model object, that name selects the method.
