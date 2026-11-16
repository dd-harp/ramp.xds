# Change initial values for `hMoI` (**XH**)

For the `hMoI` module (**XH**) for the \\(i^{th}\\) species, change
initial values named in `options` leaving other values unchanged.

It must be true that \\(m_1 \> m_2\\), so this condition is checked
before changing the initial conditions, but some pairs of values would
not arise from any history of exposure.

## Usage

``` r
# S3 method for class 'hMoI'
change_XH_inits(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- i:

  the host species index

## Value

an **`xds`** object

## State Variables

- `m1` or \\(m_1\\):

  mean MoI for all infections

- `m2` or \\(m_2\\):

  mean MoI for apparent (detectable) infections

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
