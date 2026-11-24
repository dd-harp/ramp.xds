# Get the residence matrix

Get the residence matrix for host species `i`.

## Usage

``` r
get_residence_matrix(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a `nPatches` \\(\times\\) `nStrata` matrix

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

[residence_matrix](https://dd-harp.github.io/ramp.xds/reference/residence_matrix.md),
[get_residence](https://dd-harp.github.io/ramp.xds/reference/get_residence.md),
[setup_blood_feeding_interface](https://dd-harp.github.io/ramp.xds/reference/setup_blood_feeding_interface.md)
