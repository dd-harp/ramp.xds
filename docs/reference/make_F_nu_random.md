# Make random laying rates

Set up a random vector

## Usage

``` r
make_F_nu_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 980,
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

  argument 1 for f_rand

- p2:

  argument 2 for f_rand

## Value

an object to set up egg laying
