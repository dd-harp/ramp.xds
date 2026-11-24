# Get the skill set for `trivialXH` (**XH**)

Return the capabilities and compatibilities of the `trivialXH` module as
a list.

## Usage

``` r
# S3 method for class 'trivialXH'
get_skillset_XH(xds_obj, i)

# S3 method for class 'trivial'
get_skillset_XH(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object (or the module name)

- i:

  the species index

## Value

the **XH** module skill set, as a list

## Note

The compatibility name `"trivial"` returns the same skill set as
`"trivialXH"`.
