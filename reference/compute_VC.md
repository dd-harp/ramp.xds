# Compute the VC Matrix

Compute the \\N_p \times N_p\\ matrix \\\mathcal{V}\\ whose columns
describe the number of infective bites arising in each patch from all
the mosquitoes biting a single human on a single day in each patch.

This creates a new model object to compute VC then

- sets the initial condition to be the number of mosquitoes biting a
  single infectious human on a single day, \\fqM/W\\

- sets mean forcing to zero

- adds a variable that tracks infective biting, \\fqZ\\

- runs the model until the expected number of living mosquitoes from
  that cohort is one in a million

- saves the

## Usage

``` r
compute_VC(xds_obj, s = 1, tol = 1e-05)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- s:

  the vector species index

- tol:

  a tolerance parameter

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
