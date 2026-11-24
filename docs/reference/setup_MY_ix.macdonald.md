# Set up variable indices for `macdonald` (**MY**)

Implements
[setup_MY_ix](https://dd-harp.github.io/ramp.xds/reference/setup_MY_ix.md)
for the macdonald model.

## Usage

``` r
# S3 method for class 'macdonald'
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

- `Z`:

  density of infectious adult mosquitoes

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md)
