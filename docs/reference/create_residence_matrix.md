# Create the Residency Matrix

The residency matrix, \\J\\, holds information about residency for each
human population stratum. It is the template for the time spent and time
at risk matrices, making it possible to compute mosquito parameters
describing blood feeding, the mixing matrix, and terms describing
transmission.

## Usage

``` r
create_residence_matrix(nPatches, residence)
```

## Arguments

- nPatches:

  the number of patches

- residence:

  a vector describing the patch index for each habitat

## Value

the residence [matrix](https://rdrr.io/r/base/matrix.html), denoted
\\J\\ where \\\left\|J\right\|= n_p \times n_h\\

## Details

Information about residence in a patch location for each stratum is
passed as the residence vector, an ordered list of patch locations. If
the \\i^{th}\\ stratum lives in the \\j^{th}\\ patch, then
\\{J}\_{j,i}=1.\\ Otherwise, \\{J}\_{j,i}=0.\\

Since \\J\\ is a matrix, it is readily used for computation. Let:

- \\n_h = \\ `nStrata`, the number of population strata;

- \\n_p = \\ `nPatches`, the number of patches.

If \\w\\ is any vector describing a quantity in strata (*i.e.*,
\\\left\|w\right\|=n_h\\), then \$\$W={J}\cdot w\$\$ is a vector that
has summed \\w\\ by residency for the strata, and \\\left\|W\right\|=
n_p\\.

## See also

see
[setup_BLOOD_FEEDING](https://dd-harp.github.io/ramp.xds/reference/setup_BLOOD_FEEDING.md)

see
[view_residence_matrix](https://dd-harp.github.io/ramp.xds/reference/view_residence_matrix.md)
