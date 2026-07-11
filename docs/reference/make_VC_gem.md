# Make the VC Matrix

Compute the \\N_p \times N_p\\ matrix \\\mathcal{V}\\ whose columns
describe the number of infective bites arising in each patch from all
the mosquitoes biting a single human on a single day in each patch.

Compute the VC matrix for models that use the parameter set for the
generalized Macdonald model

## Usage

``` r
# S3 method for class 'macdonald'
make_VC(xds_obj, s = 1)

make_VC_gem(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  a **`ramp.xds`** model object

- s:

  the vector species index

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)

a numeric [matrix](https://rdrr.io/r/base/matrix.html)

## Note

This is called for the models `macdonald`, `GeM`, and `SI`
