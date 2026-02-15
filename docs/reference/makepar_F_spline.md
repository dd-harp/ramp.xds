# Make Parameters for a Spline

Return an object for
[make_function.splinef](https://dd-harp.github.io/ramp.xds/reference/make_function.splinef.md)
or
[make_function.splineX](https://dd-harp.github.io/ramp.xds/reference/make_function.splineX.md)

## Usage

``` r
makepar_F_spline(tt, yy, X = FALSE)
```

## Arguments

- tt:

  the nodes

- yy:

  the y values

- X:

  a switch to configure for splinef or splineX

## Value

parameters to configure the `splinef` or `splineX` case of
`make_function`
