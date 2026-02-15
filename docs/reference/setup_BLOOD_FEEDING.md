# Set up Blood Feeding

Set up a part of the **`xds`** object that defines the interface for
blood feeding

## Usage

``` r
setup_BLOOD_FEEDING(pars)
```

## Arguments

- pars:

  an **`xds`** object

## Value

a modified **`xds`** object

## Details

This implements a blood feeding model described by Wu SL, *et al.*,
(2023).

Modular computation in ramp.xds requires a rigid interface to guarantee
mathematical consistency for blood feeding and transmission. The
interface is for blood feeding is defined by an object called `BFpar`
that is attached to the **`xds`** object pars as `pars$BFpar`. The blood
feeding interface

- the residency matrix \\J\\

- a time spent (TiSp) matrix \\\Theta\\

- a circadian function `F_circadian` for each vector species

- a time at risk (TaR) matrix \\\Psi\\ that is the product the TiSp
  matrix and the circadian function

- blood feeding search weights

- a vector describing \\W\\, the availability the population strata for
  blood feeding: the availability of the parasite/pathogen's hosts

- a vector describing the availability of visitors

- a vector describing the availability of other blood hosts

- a vector describing \\B\\, the total availability of all vertebrate
  blood hosts for blood feeding (see
  [`compute_B()`](https://dd-harp.github.io/ramp.xds/reference/compute_B.md))

These quantities are used to model transmission (see
[`setup_TRANSMISSION()`](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md)).

Mosquito bionomic parameters *ought* to be constrained. If bionomic
parameters are assigned, there's no guarantee they are internally
mathematically consistent or sensible. To guarantee internal
consistency, the the concept of resource availability should be used to
compute the blood feeding rates (\\f\\) using *functional responses.*
The human fraction ought to be \\q=W/B\\. Availability can also be used
to model mosquito movement.

**Mulit-Host Models** - In models with multiple host species, with
availability \\W_i\\, the fraction on each host species would be
\\W_i/B\\. In models with multiple vector species, each species could
have different search habits and preferences, so blood feeding
availability is indexed for each species: \\B_s\\ and \\W\_{s}\\. In
models with multiple host species, \\W\_{i,s}\\ is the availability of
the \\i^{th}\\ host species to the \\s^{th}\\ vector species. For hosts,
availability is based on *time spent* in each patch, and *time at risk,*
or time spent by time of day weighted by mosquito species-specific
*search weights* reflecting different preferences and a circadian
function describing relative mosquito blood feeding rates by time of
day.

## References

Wu SL, Henry JM, Citron DT, Ssebuliba DM, Nsumba JN, C HMS, Brady OJ,
Guerra CA, García GA, Carter AR, Ferguson HM, Afolabi BE, Hay SI, Jr
RCR, Kiware S, Smith DL (2023). “Spatial dynamics of malaria
transmission.” *PLoS Computational Biology*, **19**(6), e1010684.
[doi:10.1371/journal.pcbi.1010684](https://doi.org/10.1371/journal.pcbi.1010684)
, [2023-06-27](https://dd-harp.github.io/ramp.xds/reference/2023-06-27).

## See also

[setup_TRANSMISSION](https://dd-harp.github.io/ramp.xds/reference/setup_TRANSMISSION.md)
