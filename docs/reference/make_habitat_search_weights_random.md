# Make habitat search weights

Set up a random vector of habitat search weights

## Usage

``` r
make_habitat_search_weights_random(
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

## See also

[setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md)
