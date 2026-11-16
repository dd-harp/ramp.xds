# Make a time spent matrix, called timespent

Make a time spent matrix, called timespent

## Usage

``` r
make_timespent_athome(
  nPatches,
  residence,
  options = list(),
  atHome = 1,
  travel = 0
)
```

## Arguments

- nPatches:

  is the number of patches

- residence:

  is the home patch for each stratum

- options:

  is a set of options that overwrites the defaults

- atHome:

  is the fraction of time spent at home

- travel:

  is the fraction of time spent traveling

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
