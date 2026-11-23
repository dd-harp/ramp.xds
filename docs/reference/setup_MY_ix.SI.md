# Set up variable indices for `SI` (**MY**)

Implements
[setup_MY_ix](https://dd-harp.github.io/ramp.xds/reference/setup_MY_ix.md)
for the `SI` model.

## Usage

``` r
# S3 method for class 'SI'
setup_MY_ix(xds_obj, s)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md)
