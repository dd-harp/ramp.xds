# Setup Egg Laying

Set up a part of the `xds` object that defines the interface for egg
laying

## Usage

``` r
setup_EGG_LAYING(pars, membership)
```

## Arguments

- pars:

  an `xds` object

- membership:

  is the habitat membership vector

## Value

an `xds` object

## Details

This implements a model for egg laying described by Wu SL, *et al.*,
(2023).

Modular computation in **`ramp.xds`** requires a rigid interface to
guarantee mathematical consistency for egg laying and emergence. The
interface is defined by an object called `EGGpar` that is attached to
the `xds` object `pars` as `pars$EGGpar`. The interface includes

- a habitat membership matrix, \\N\\ made by
  [create_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/create_habitat_matrix.md)

- the habitat search weights

- a quantity that is motivated by mosquito searching for resources,
  called habitat availability \\Q\\ made by
  [compute_Q](https://dd-harp.github.io/ramp.xds/reference/compute_Q.md);

- the availability of ovitraps

- the availability of unsuitable habitats

- the availability of anything that is like a habitat, but that is an
  aquatic habitat in the model, including ovitraps and unsuitable
  habitats, \\Q\_{tot}\\ made by
  [compute_Qtot](https://dd-harp.github.io/ramp.xds/reference/compute_Qtot.md);

- the egg distribution matrix \\U\\, made by
  [compute_Umatrix](https://dd-harp.github.io/ramp.xds/reference/compute_Umatrix.md)

- a vector that stores eggs laid

This function is called by `make_xds_object_template` to set up `EGGpar`
and the variables and parameters with all the variables it might depend
on.

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

For a discussion of habitat availability, see
[`compute_Q()`](https://dd-harp.github.io/ramp.xds/reference/compute_Q.md)

The habitat membership matrix is created by
[`create_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/create_habitat_matrix.md)
