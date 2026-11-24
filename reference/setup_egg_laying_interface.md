# Setup the Habitat Interface for Egg Laying and Emergence

Set up a part of the `xds` object that defines the interface for egg
laying

## Usage

``` r
setup_egg_laying_interface(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Details

This implements a model for egg laying described by Wu SL, *et al.*,
(2023).

Modular computation in **`ramp.xds`** requires a rigid interface to
guarantee mathematical consistency for egg laying and emergence. This
function initializes lists in `xds_obj$terms` for the first vector
species:

- `G`: eggs laid in each patch

- `eta`: eggs laid in each habitat

- `alpha`: emergence from each habitat

- `Lambda`: emergence in each patch

- `Q` and `Qall`: habitat availability in each patch, initialized to one

The egg-laying and emergence terms are initialized to zero. In
`xds_obj$patches`, it initializes `bad_habitats` and `ovitraps` to zero
in each patch, with static port objects.

This function is called by
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md).
Habitat membership is configured separately by
[setup_habitats](https://dd-harp.github.io/ramp.xds/reference/setup_habitats.md).

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
. [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

The habitat membership matrix is created by
[`make_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
