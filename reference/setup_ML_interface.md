# Setup the Habitat Interface for Egg Laying and Emergence

Set up a part of the `xds` object that defines the interface for egg
laying

## Usage

``` r
setup_ML_interface(xds_obj, membership)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- membership:

  is the habitat membership vector

## Value

an `xds` object

## Details

This implements a model for egg laying described by Wu SL, *et al.*,
(2023).

Modular computation in **`ramp.xds`** requires a rigid interface to
guarantee mathematical consistency for egg laying and emergence. The
interface is defined by an object called `ML_interface` that is attached
to the `xds` object `xds_obj` as `xds_obj$ML_interface`. The interface
includes

- a habitat membership matrix, \\N\\ made by
  [make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)

- the habitat search weights

- a quantity that is motivated by mosquito searching for resources,
  called habitat availability \\Q\\, computed by
  [F_Q](https://dd-harp.github.io/ramp.xds/reference/F_q.md);

- the availability of ovitraps

- the availability of unsuitable habitats

- the availability of anything that attracts egg laying mosquitoes,
  including ovitraps and unsuitable habitats

- the egg distribution matrix \\O\\, made by
  [make_O_matrix](https://dd-harp.github.io/ramp.xds/reference/make_O_matrix.md)

- a vector that stores eggs laid

This function is called by `compute_xds_object_template` to set up
`ML_interface` and the variables and parameters with all the variables
it might depend on.

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

The habitat membership matrix is created by
[`make_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
