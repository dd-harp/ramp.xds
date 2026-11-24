# Make random human fractions (**MY**)

Draw human fractions once during setup. Subsequent evaluations reuse
these values.

## Usage

``` r
make_F_q_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 960,
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
[F_q.static](https://dd-harp.github.io/ramp.xds/reference/F_q.static.md)

## Note

The values are stored in a static port object.
