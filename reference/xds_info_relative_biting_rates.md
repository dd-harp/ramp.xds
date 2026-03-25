# Relative Biting Rate

In **`ramp.xds`**, heterogeneous biting rates are handled by assigning
*search weights* to various population strata. The *relative biting
rate* for a stratum is a value \\\omega\\ such that if the population
average daily EIR is \\E\\, then the daily EIR for the stratum is
\\\omega E\\.

Relative biting rates are handled in two ways, depending on the frame:

- In model that define multiple human population strata, the RBRs are
  computed from the stratum specific EIR: \$\$\omega_i = E_i / E\$\$

- In models that are forced by the EIR, the user configures a function
  to compute the population average EIR over time, and another function
  to compute the relative biting rate by age.

## See also

[xds_info_search_weights_blood](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights_blood.md)
\|
[xds_info_heterogeneous_biting_rates](https://dd-harp.github.io/ramp.xds/reference/xds_info_heterogeneous_biting_rates.md)
\|
[xds_info_heterogeneity](https://dd-harp.github.io/ramp.xds/reference/xds_info_heterogeneity.md)
