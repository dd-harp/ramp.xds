# The residence matrix

- \\(\mathbf J\\):

  the residence matrix

## Residence Matrix

The residence matrix \\((\mathbf J)\\) is an `nPatches` \\(\times\\)
`nStrata` matrix constructed from the
[residence_vector](https://dd-harp.github.io/ramp.xds/reference/residence_vector.md)
for a host species.

If the \\(i^{th}\\) stratum resides in the \\(j^{th}\\) patch, then
\\(J\_{j,i}=1\\). Otherwise, \\(J\_{j,i}=0\\).

If \\(x\\) is any vector describing a quantity in population strata
(*i.e.*, \\(\left\|x\right\|=\\)`nStrata`), then \$\$X = \mathbf J \cdot
x\$\$ sums \\(x\\) by patch of residence, and
\\(\left\|X\right\|=\\)`nPatches`. In particular, \\(\mathbf J \cdot
H\\) gives the resident population in each patch from the stratum
population densities \\(H\\).

The residence matrix initializes the time spent matrix during basic
setup. Time spent and time at risk can subsequently differ from
residence.

## See also

[residence_vector](https://dd-harp.github.io/ramp.xds/reference/residence_vector.md),
[make_residence_matrix](https://dd-harp.github.io/ramp.xds/reference/make_residence_matrix.md),
[get_residence_matrix](https://dd-harp.github.io/ramp.xds/reference/get_residence_matrix.md),
[xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md),
[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md)
