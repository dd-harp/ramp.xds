# Make random EIP configuration

Build a configuration object for random EIP values.

## Usage

``` r
make_F_eip_random(
  nPatches,
  options = list(),
  f_rand = stats::rgamma,
  p1 = 12,
  p2 = 1
)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overwrites the defaults

- f_rand:

  a positive random number generator

- p1:

  first distribution parameter (Gamma shape by default)

- p2:

  second distribution parameter (Gamma rate by default)

## Value

a configuration object for
[F_eip.random](https://dd-harp.github.io/ramp.xds/reference/F_eip.random.md)
