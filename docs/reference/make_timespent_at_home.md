# Make a time spent matrix

Construct a time spent matrix, where a fraction of time `at_home` is
spent at home.

The remaining time is divided equally among the other patches.

An optional argument allows for some time to be spent `not_at_risk`

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

  is a set of options that overrides the defaults

- at_home:

  is the fraction of time spent at home

- not_at_risk:

  is the fraction of time not at risk

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)

## See also

[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md)
