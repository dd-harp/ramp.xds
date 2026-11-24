# Make random patch emigration rates (**MY**)

Draw patch emigration rates once during setup. Subsequent evaluations
reuse these values.

## Usage

``` r
make_F_sigma_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 100,
  p2 = 1000
)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overrides the defaults

- f_rand:

  a random number generator called as `f_rand(nPatches, p1, p2)` that
  returns a numeric vector of length `nPatches`

- p1:

  argument for f_rand

- p2:

  argument for f_rand

## Value

a static port object for
[F_sigma.static](https://dd-harp.github.io/ramp.xds/reference/F_sigma.static.md)

## Note

The values are stored in a static port object.
