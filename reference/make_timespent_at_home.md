# Make a time spent matrix, called timespent

Make a time spent matrix, called timespent

## Usage

``` r
make_timespent_at_home(
  nPatches,
  residence,
  options = list(),
  at_home = 1,
  not_at_risk = 0
)
```

## Arguments

- nPatches:

  is the number of patches

- residence:

  is the home patch for each stratum

- options:

  is a set of options that overwrites the defaults

- at_home:

  is the fraction of time spent at home

- not_at_risk:

  is the fraction of time not at risk

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
