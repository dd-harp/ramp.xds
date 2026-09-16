# Random ovitrap availability

Set up a random vector describing ovitrap availability

## Usage

``` r
make_ovitraps_random(
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

  is a set of options that overwrites the defaults

- f_rand:

  a random number generator

- p1:

  argument for f_rand

- p2:

  argument for f_rand

## Value

a numeric vector
