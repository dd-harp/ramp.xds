# Population Strata

A human (or host) population is sub-divided into *strata.* The human
population (or other host population) and its structure, size and
behavior are critical aspects of transmission.

- `residence`:

  a vector: the patch index where each human stratum lives

- `nStrata`:

  `nStrata = length(residence)`

Human / host populations are heterogeneous, and some of that
heterogeneity is important for accurately describing the epidemiology of
malaria and other mosquito-borne pathogens. **`ramp.xds`** was thus
designed with the capability of segmenting a population into as many
strata as needed to achieve a degree of accuracy. In designing a study,
decisions about how to construct a model are translated into a set of
population strata. Information about the number of strata is configured
through the `residence` vector.

## Residency and Time Spent

In designing **`ramp.xds`**, the first concern was how to handle spatial
dynamics. Since mosquitoes and humans are very different, a
metapopulation that is set up for adult mosquito ecology might not work
as well for the humans, whose behaviors follow very different rules.
Humans are enumerated by where they live, and most people spend most of
their time at risk in or around home.

The human population is thus stratified around the concept of patch
residency – the patch where home is found. Exposure is based on the
notions of *time spent* (see
[xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md))
and *time at risk* (see
[xds_info_time_at_risk](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_at_risk.md)).
The model is configured by passing a vector, called `residence.` The
residence vector is the index of the patch where each human population
resides, and multiple strata can reside in the same patch. Conversely,
there might be some patches where no one resides, such as the mosquito
habitats in a buffer around the places where humans live.

## Residency Matrix

The residence vector is used to construct a residency matrix, \\J\\,
that is the same shape as the time spent and time at risk matrices. It
can be used to sum quantities by patch. For example, if \\X\\ is the
density of infected individuals, then \\J \cdot X\\ is the number of
infected individuals by patch, so true prevalence would be \$\$\frac{J
\cdot X}{J \cdot H}\$\$.

## Demography

Utilities to handle host age and *cohort dynamics* are handled in
**`ramp.demog`**. Similarly, the functionality to handle

## Principled Stratification

Principled stratification is the algorithm:

- interventions
