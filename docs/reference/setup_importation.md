# Setup importation

This function, called by
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
sets up terms and objects associated with malaria importation.

## Usage

``` r
setup_importation(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Details

Initialize the importation junction with a static object. Visitor
availability and visitor infectiousness are initialized to zero in each
patch for the first host species, with static port objects. Time away
and travel EIR are configured separately on the **XH** object.

**Multi-Host Models**

In models with multiple host species, the availability of visitors might
vary by host.

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[setup_transmission](https://dd-harp.github.io/ramp.xds/reference/setup_transmission.md)
