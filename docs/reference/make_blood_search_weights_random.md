# Make blood search weights

Set up a random vector

## Usage

``` r
make_blood_search_weights_random(
  nStrata,
  options = list(),
  f_rand = stats::rlnorm,
  p1 = 0,
  p2 = 0.5
)
```

## Arguments

- nStrata:

  is the number of population strata

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
