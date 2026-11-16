# Infective mosquito biting rate for `macdonald`

Implements
[F_fqZ](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.md) for the
`macdonald` model.

## Usage

``` r
# S3 method for class 'macdonald'
F_fqZ(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Infective Biting

Infective biting density is the number of infectious bites by mosquitoes
on humans in each patch per day. The density of infectious mosquitoes
\\(Z\\) is computed from \\(Y\\) after survival and dispersal through
the EIP.

- `fqZ`:

  infective biting density, the number of infectious bites per patch per
  day

- `fqM`:

  human biting density, the number of bites on humans per patch per day

F_fqZ.macdonald and
[F_fqM.macdonald](https://dd-harp.github.io/ramp.xds/reference/F_fqM.macdonald.md)
compute these patch-level quantities.
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
combines `fqZ` with the biting distribution matrix and resident-bite
fraction to compute stratum-level EIR;
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) computes
HBR from `fqM` in the same way. The resulting `EIR` and `HBR` orbits are
attached to the XH output during parsing.

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[dMYdt.macdonald](https://dd-harp.github.io/ramp.xds/reference/dMYdt.macdonald.md)
