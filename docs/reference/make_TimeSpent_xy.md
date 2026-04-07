# Make a time spent matrix, called timespent

Make a time spent matrix, called timespent

## Usage

``` r
make_timespent_xy(xy, residence, kern, stay, travel)
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
