# Parse outputs for `GeM` (**MY**)

Implements
[parse_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/parse_MY_orbits.md)
for the GeM model

## Usage

``` r
# S3 method for class 'GeM'
parse_MY_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of outputs from deSolve

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Infective Biting

The human biting density, `fqM`, is the number of human bites per patch
per day; the infective biting density, `fqZ`, is the number of
infectious human bites per patch per day.
[F_fqM.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqM.GeM.md)
and
[F_fqZ.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.GeM.md)
compute these quantities as \\f q M\\ and \\f q Z\\, respectively.
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
combines infective biting density with the biting distribution matrix
and resident-bite fraction to compute stratum-level `EIR`.
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) uses
`fqM`, the same matrix, and resident-bite fraction to compute
stratum-level `HBR`.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[get_MY_vars.GeM](https://dd-harp.github.io/ramp.xds/reference/get_MY_vars.GeM.md)
