# Make random values for vis_kappa

Set up a random vector

## Usage

``` r
make_vis_kappa_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 20,
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

a [matrix](https://rdrr.io/r/base/matrix.html)
