# Make a time spent matrix, called timespent

Make a time spent matrix, called timespent

## Usage

``` r
make_timespent_xy(xds_obj, xy, residence, stay, kern, kopts = list())
```

## Arguments

- xds_obj:

  the **`xds`** model object

- xy:

  is the xy-locations of the patches

- residence:

  is the home patch for each stratum

- stay:

  is the fraction of time spent at home

- kern:

  is a function to compute time spent away from home

- kopts:

  options to pass to the kernel

## Value

a [matrix](https://rdrr.io/r/base/matrix.html)
