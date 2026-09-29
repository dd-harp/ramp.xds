# Make a patch emigration configuration

Make an object to model patch emigration as a type 2 functional response

## Usage

``` r
make_F_sigma_type2(
  nPatches,
  options = list(),
  sigX = 0.3,
  sigB = 0.1,
  sigQ = 0.1,
  sigS = 0.1
)
```

## Arguments

- nPatches:

  is the number of patches

- options:

  is a set of options that overwrites the defaults

- sigX:

  the emigration rate from a patch with no resources

- sigB:

  the shape parameter for blood hosts

- sigQ:

  the shape parameter for aquatic habitats

- sigS:

  the shape parameter for sugar

## Value

a configuration object for
[F_sigma.type2](https://dd-harp.github.io/ramp.xds/reference/F_sigma.type2.md)

## See also

[F_sigma.type2](https://dd-harp.github.io/ramp.xds/reference/F_sigma.type2.md)
