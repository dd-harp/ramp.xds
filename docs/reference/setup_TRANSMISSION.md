# Setup the interface for parasite / pathogen transmission

Sets up the object that defines the transmission interface for an `xds`
model object.

## Usage

``` r
setup_transmission(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Details

This implements a model for egg laying described by Wu SL, *et al.*,
(2023).

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)

[setup_XY_interface](https://dd-harp.github.io/ramp.xds/reference/setup_XY_interface.md)
