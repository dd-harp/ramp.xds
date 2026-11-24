# Get the skill set for `hMoI` (**XH**)

Return the capabilities and compatibilities of the `hMoI` module as a
list.

## Usage

``` r
# S3 method for class 'hMoI'
get_skillset_XH(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object (or the module name)

- i:

  the species index

## Value

the **XH** module skill set, as a list

## Details

The `hMoI` module is an example of a model that is not extensible. There
is no way to add either dynamic human population density or mass
treatment without violating the model assumptions.

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
