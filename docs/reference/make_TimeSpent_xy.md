# Make a mosquito dispersal matrix, called TimeSpent

Make a mosquito dispersal matrix, called TimeSpent

## Usage

``` r
make_TimeSpent_xy(xy, residence, kern, stay, travel)
```

## Arguments

- xy:

  is the xy-locations of the patches

- residence:

  is the home patch for each stratum

- kern:

  is a function that gives weight by distance

- stay:

  is the fraction of time spent at home

- travel:

  is the fraction of time spent traveling

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
