# Make Parameters for a sharkbite Function

Return an object for
[make_function.sharkbite](https://dd-harp.github.io/ramp.xds/reference/make_function.sharkbite.md)

## Usage

``` r
makepar_F_sharkbite(
  D = 100,
  L = 180,
  uk = 1/7,
  dk = 1/40,
  pw = 1,
  mx = 1,
  N = 1
)
```

## Arguments

- D:

  the half-saturation day for scale-up

- L:

  the half-saturation day for decay

- uk:

  shape parameter for scale-up

- dk:

  shape parameter for decay

- pw:

  shape parameter, power

- mx:

  a maximum value

- N:

  the length of the vector to return

## Value

a function F_season
