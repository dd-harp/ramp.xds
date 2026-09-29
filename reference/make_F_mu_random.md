# Make random emigration-loss fractions

Build a configuration object for random emigration-loss fractions.

## Usage

``` r
make_F_mu_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 8,
  p2 = 1000
)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overwrites the defaults

- f_rand:

  a random number generator

- p1:

  argument for f_rand

- p2:

  argument for f_rand

## Value

a configuration object for
[F_mu.random](https://dd-harp.github.io/ramp.xds/reference/F_mu.random.md)
