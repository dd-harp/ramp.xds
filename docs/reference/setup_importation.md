# Setup importation

This function, called by
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
sets up terms and objects associated with malaria importation

## Usage

``` r
setup_importation(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object

## Details

This implements a framework to model blood feeding described by Wu SL,
*et al.*, (2023).

Modular computation in **`ramp.xds`** requires a rigid interface to
guarantee mathematical consistency in computing quantites related to
blood feeding and transmission.

Several terms were developed to model importation by traveling humans
and visitors. The terms associated with time away and the travel EIR are
human activities, so they are set up on the **XH** object. This sets up
visitors and infectiousness.

**Mulit-Host Models**

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
