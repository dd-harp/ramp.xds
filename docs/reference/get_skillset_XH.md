# Get the skill set (**XH**)

Return the selected module's capabilities and compatibilities as a list.

## Usage

``` r
get_skillset_XH(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object (or the module name)

- i:

  the species index

## Value

the **XH** module skill set, as a list

## Note

Dispatch is on the class of `xds_obj$XH_obj[[i]]`. If a module name is
passed instead of a model object, that name selects the method.
