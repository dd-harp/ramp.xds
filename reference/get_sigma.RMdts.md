# Get the patch emigration rate for `RMdts` (**MY**)

Return the stored realized patch emigration rate per patch.

## Usage

``` r
# S3 method for class 'RMdts'
get_sigma(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Details

Read `sigma` without evaluating ports or applying effect sizes.

## Parameters

- `f`:

  blood feeding rate

- `q`:

  human blood fraction

- `eip`:

  extrinsic incubation period (time steps)

- `g`:

  mosquito mortality rate

- `sigma`:

  emigration rate

- `mu`:

  emigration loss rate

- `K`:

  mosquito dispersal matrix

- `Omega`:

  adult mosquito demographic matrix (mortality + migration)

- `Upsilon`:

  survival and dispersal through the eip

## See also

[RMdts](https://dd-harp.github.io/ramp.xds/reference/RMdts.md)
