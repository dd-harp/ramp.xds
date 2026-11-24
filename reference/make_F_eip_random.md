# Make random EIP configuration (**MY**)

Draw extrinsic incubation periods once during setup. Subsequent
evaluations reuse these values.

## Usage

``` r
make_F_eip_random(
  nPatches,
  options = list(),
  f_rand = stats::rgamma,
  p1 = 12,
  p2 = 1
)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overrides the defaults

- f_rand:

  a random number generator called as `f_rand(nPatches, p1, p2)` that
  returns a positive numeric vector of length `nPatches`

- p1:

  first distribution parameter (Gamma shape by default)

- p2:

  second distribution parameter (Gamma rate by default)

## Value

a static port object for
[F_eip.static](https://dd-harp.github.io/ramp.xds/reference/F_eip.static.md)

## Note

The values are stored in a static port object. The stored time
derivative is zero.
