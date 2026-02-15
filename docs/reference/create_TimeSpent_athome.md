# Make a mosquito dispersal matrix, called TimeSpent

Make a mosquito dispersal matrix, called TimeSpent

## Usage

``` r
create_TimeSpent_athome(
  nPatches,
  residence,
  opts = list(),
  atHome = 1,
  travel = 0
)
```

## Arguments

- nPatches:

  is the number of patches

- residence:

  is the home patch for each stratum

- opts:

  is a set of options that overwrites the defaults

- atHome:

  is the fraction of time spent at home

- travel:

  is the fraction of time spent traveling

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
