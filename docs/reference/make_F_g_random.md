# Make random mosquito mortality rates (**MY**)

Draw mosquito mortality rates once during setup and store them in a
static port object. Subsequent evaluations reuse these values.

## Usage

``` r
make_F_g_random(
  nPatches,
  options = list(),
  f_rand = stats::rbeta,
  p1 = 80,
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
[F_g.static](https://dd-harp.github.io/ramp.xds/reference/F_g.static.md)
