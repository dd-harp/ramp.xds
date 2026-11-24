# Make random oviposition rates (**MY**)

Draw oviposition rates once during setup. Subsequent evaluations reuse
these values.

## Usage

``` r
make_F_nu_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 980,
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

  argument 1 for `f_rand`

- p2:

  argument 2 for `f_rand`

## Value

a static port object for
[F_nu.static](https://dd-harp.github.io/ramp.xds/reference/F_nu.static.md)

## Note

The values are stored in a static port object.
