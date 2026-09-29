# Make random blood feeding rate object

Build a configuration object for random blood feeding rates.

## Usage

``` r
make_F_f_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 280,
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

  argument for `f_rand`

- p2:

  argument for f_rand

## Value

a configuration object for
[F_f.random](https://dd-harp.github.io/ramp.xds/reference/F_f.random.md)
