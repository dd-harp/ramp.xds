# Setup the Blood Feeding Interface

This function, called by
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
initializes storage for blood-feeding terms and patch-level resources.

## Usage

``` r
setup_blood_feeding_interface(xds_obj, residence)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- residence:

  a residence vector for the first host species

## Value

an **`xds`** model object

## Details

This implements a framework to model blood feeding described by Wu SL,
*et al.*, (2023).

The initializer creates `xds_obj$blood_feeding_obj`, initializes the
first time-at-risk and host-availability term slots, and creates
zero-valued visitor, other-host, and blood-trap vectors with static port
objects. Time-spent matrices, circadian functions, and blood search
weights are configured separately. These terms are used by
[setup_transmission](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md).

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
. [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[setup_transmission](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md),
[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md),
[time_at_risk](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md),
[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md)
