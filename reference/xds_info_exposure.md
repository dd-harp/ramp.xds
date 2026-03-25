# Exposure

The EIR computed by
[xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md)
is a measure of *local* risk: it is the expected number of infected
bites, per person, and it is computed for every population stratum. In
**exposure,** the daily local EIR is computed into a measure of the
population FoI in three steps:

- the total EIR is the weighted sum of local exposure and travel
  exposure, using the concept of time here *vs.* time away;

- the distribution of the number of bites per person is computed under a
  model of environmental heterogeneity ;

- the number of infections is computed under a model of partial
  immunity.

## Environmental Heterogeneity

While EIR is an expected value, the expected value can have a
distribution in a human population stratum. For example, if the
expectation has a Gamma distribution, then the expected number of bites
per person would be negatively binomially distributed. For more, see
[xds_info_environmental_heterogeneity](https://dd-harp.github.io/ramp.xds/reference/xds_info_environmental_heterogeneity.md).

## Travel Malaria

Travel malaria is defined by exposure to malaria while spending time
outside of the spatial domain represented by the patches. See
[xds_info_travel_malaria](https://dd-harp.github.io/ramp.xds/reference/xds_info_travel_malaria.md)

## Pre-Erythrocytic Immunity

Depending on the X module, a parameter called \\b\\ describes the
probability of infection per infectious bite.

## See also

[xds_info_environmental_heterogeneity](https://dd-harp.github.io/ramp.xds/reference/xds_info_environmental_heterogeneity.md)
\|
[xds_info_travel_malaria](https://dd-harp.github.io/ramp.xds/reference/xds_info_travel_malaria.md)
