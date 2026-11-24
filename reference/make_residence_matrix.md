# Make the residence matrix

Create the residence matrix (see
[residence_matrix](https://dd-harp.github.io/ramp.xds/reference/residence_matrix.md)).

## Usage

``` r
make_residence_matrix(nPatches, residence)
```

## Arguments

- nPatches:

  the number of patches

- residence:

  a residence vector of patch indices, one per population stratum

## Value

a `nPatches` \\\times\\ `nStrata` matrix
