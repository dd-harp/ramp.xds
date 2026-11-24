# Get the skill set (**MY**)

Return the selected module's capabilities and compatibilities as a list.
Non-trivial modules include logical `<parameter>_is_port` flags for `f`,
`q`, `g`, `sigma`, `sigma_b`, `sigma_q`, `mu`, `nu`, `eip`, and
`K_matrix`. A flag is `TRUE` when the module implements that parameter
as a port, and `FALSE` when the parameter is a direct value or is
absent.

## Usage

``` r
get_skillset_MY(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object (or the module name)

- s:

  the species index

## Value

the **MY** module skill set, as a list

## Note

Dispatch is on the class of `xds_obj$MY_obj[[s]]`. If a module name is
passed instead of a model object, that name selects the method.
