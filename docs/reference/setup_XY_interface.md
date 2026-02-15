# Setup the Blood Feeding Interface

This function, called by
[make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md),
sets up the blood feeding interface, `xds_obj$XY_interface.`

## Usage

``` r
setup_XY_interface(xds_obj, residency)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- residency:

  the residency vector

## Value

a **`xds`** model object template with a blood feeding object

## Details

This implements a framework to model blood feeding described by Wu SL,
*et al.*, (2023).

Modular computation in **`ramp.xds`** requires a rigid interface to
guarantee mathematical consistency in computing quantites related to
blood feeding and transmission.

The interface for blood feeding is defined by an object called
`XY_interface`, attached to the **`xds`** model object as
`xds_obj$XY_interface.`

The blood feeding interface sets up several objects:

- the residency matrix \\J\\ (see
  [make_residency_matrix](https://dd-harp.github.io/ramp.xds/reference/make_residency_matrix.md))

- a time spent (TiSp) matrix \\\Theta\\ (see
  [setup_TimeSpent](https://dd-harp.github.io/ramp.xds/reference/setup_TimeSpent.md))

- a circadian function `F_circadian` for each vector species

- a time at risk (TaR) matrix \\\Psi,\\ the product of the TiSp matrix
  and the circadian function

- blood feeding search weights \\\omega\\

- a vector describing the available of humans (or hosts), \\W\\ (see
  [F_W_available](https://dd-harp.github.io/ramp.xds/reference/F_W_available.md))

- a vector describing the available of visitors, \\V\\

- a vector describing the available of other blood hosts, \\O\\

- a vector describing the total available of blood hosts, \\B\\ (see
  [F_B_available](https://dd-harp.github.io/ramp.xds/reference/F_B_available.md))

These quantities are used to compute the transmission matrix, \\\beta\\,
to models transmission (see
[`setup_transmission()`](https://dd-harp.github.io/ramp.xds/reference/setup_transmission.md)).

Mosquito bionomic parameters *ought* to be constrained. If bionomic
parameters are assigned, there's no guarantee they are internally
mathematically consistent or sensible. To guarantee internal
consistency, the the concept of resource available should be used to
compute the blood feeding rates (\\f\\) using *functional responses.*
The human fraction ought to be \\q=W/B\\. available can also be used to
model mosquito movement.

**Mulit-Host Models**

In models with multiple host species, let \\W_i\\ denote the available
of the \\i^{th}\\ host species. Total availablity of blood hosts is
\$\$B = \sum_i W_i + O + V,\$\$ so the the fraction of bites on each
host species is \\W_i/B\\.

In models with multiple vector species and one host species, each vector
species could have different search habits and preferences. so blood
feeding available is computed for each species, denoted \\B_s\\ and
\\W\_{s}\\.

In models with multiple vector and multiple host species, \\W\_{i,s}\\
is the available of the \\i^{th}\\ host species to the \\s^{th}\\ vector
species.

For hosts, available is based on *time spent* in each patch, and *time
at risk,* or time spent by time of day weighted by mosquito
species-specific *search weights* reflecting different preferences and a
circadian function describing relative mosquito blood feeding rates by
time of day.

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[setup_transmission](https://dd-harp.github.io/ramp.xds/reference/setup_transmission.md)
