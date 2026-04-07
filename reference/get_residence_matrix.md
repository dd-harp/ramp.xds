# Create the residence Matrix

This function creates the residence matrix that is used to sum
quantities describing human (or host) populations at the level of a
patch.

It is created from the residence vector (`residence`), an ordered list
of the patch index for each stratum.

The structural parameter `nPatches` to handle cases where some patches
have no residents.

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

a `nPatches` \\\times\\ `nStrata` matrix

## Details

The residence matrix, herein denoted \\J\\, holds information about
residence for each human (or host) population stratum.

Information about residence in a patch location for each stratum is
passed as the residence vector, an ordered list of patch locations. If
the \\i^{th}\\ stratum lives in the \\j^{th}\\ patch, then
\\{J}\_{j,i}=1.\\ Otherwise, \\{J}\_{j,i}=0.\\

Let:

- \\N_h = \\ `nStrata`, the number of population strata;

- \\N_p = \\ `nPatches`, the number of patches.

\\J\\ is an \\N_p \times N_h\\ matrix that is used to map information
about human (or host) populations onto patches.

If \\w\\ is any vector describing a quantity in strata (*i.e.*,
\\\left\|w\right\|=N_h\\), then \$\$W={J}\cdot w\$\$ computes a vector
that sums \\w\\ by residence for the strata, and \\\left\|W\right\|=
N_p\\.

It is a template for the time spent and time at risk matrices, making it
possible to compute mosquito parameters describing blood feeding, the
mixing matrix, and terms describing transmission.

## See also

see
[setup_XY_interface](https://dd-harp.github.io/ramp.xds/reference/setup_XY_interface.md)
