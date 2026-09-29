# Make random patch emigration rates

Set up a random vector

## Usage

``` r
make_F_sigma_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 100,
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
[F_sigma.random](https://dd-harp.github.io/ramp.xds/reference/F_sigma.random.md)
