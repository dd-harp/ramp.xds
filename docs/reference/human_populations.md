# The human population

A human (or host) population is sub-divided into *strata.* The human
population (or other host population) and its structure, size and
behavior are critical aspects of transmission.

- `residence`:

  a vector: the patch index where each human stratum lives

- `nStrata`:

  `nStrata = length(residence)`

- `HPop`:

  initial human population size: `length(HPop) = nStrata`

## Human Population Size, \\H\\

The human (or host) population size plays a key role in the blood
feeding and transmission interface. In most **XH** modules, it is
handled as a variable. Setup requires having some assigning a value to
H, so it is passed at setup as `HPop.`

## The Residency Matrix

The mathematical framework was designed to model the spatial dynamics of
malaria transmission in patches. The patch structure is set up around
mosquito population ecology. Humans spend time among the patches, and
most time at risk is likely to be at home. The human population is thus
also stratified by patch location.The residence vector is the index of
the patch where each human population resides, and it is possible for
some patches to have multiple strata. The residence vector is used to
construct a residency matrix, \\J\\, that is the same shape as the time
spent and time at risk matrices. It can be used to sum quantities by
patch. For example, if \\X\\ is the density of infected individuals,
then \\J \cdot X\\ is the number of infected individuals by patch, so
true prevalence would be \$\$\frac{J \cdot X}{J \cdot H}\$\$.
