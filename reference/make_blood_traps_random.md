# Make random blood trap availability

Generate a random vector of blood trap availability.

## Usage

``` r
make_blood_traps_random(
  nPatches,
  options = list(),
  f_rand = stats::rlnorm,
  p1 = 0,
  p2 = 0.5
)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overrides the defaults

- f_rand:

  a random number generator

- p1:

  argument for f_rand

- p2:

  argument for f_rand

## Value

a numeric vector
