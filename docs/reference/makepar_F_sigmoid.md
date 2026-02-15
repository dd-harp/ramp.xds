# Make Parameters for a Sigmoidal Function

Return an object to configure a function
[make_function.sigmoid](https://dd-harp.github.io/ramp.xds/reference/make_function.sigmoid.md)

## Usage

``` r
makepar_F_sigmoid(k = 1/7, D = 100, Tl = 0, N = 1)
```

## Arguments

- k:

  the rate parameter

- D:

  the half-saturation day

- Tl:

  length of interval to normalize over

- N:

  the length of the vector to return

## Value

a sigmoidal function
