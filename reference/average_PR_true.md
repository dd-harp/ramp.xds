# Compute the average True PR

This is a generic way of computing the average PR for some subset(s) of
the population. The object `members` is a membership matrix, like the
residence matrix (see
[make_residency_matrix](https://dd-harp.github.io/ramp.xds/reference/make_residency_matrix.md)).
If the residence matrix were passed to members, average_PR_true returns
the average PR for each patch by residency.

The default, `members=NULL,` creates a \\1 \times\\ `nStrata` matrix,
which takes the matrix over the whole population. If `members` is a
matrix with 1 column and `nStrata` rows, with only some elements set to
one, then it returns the average over those strata.

## Usage

``` r
average_PR_true(xds_obj, i = 1, members = NULL)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the human species index

- members:

  a membership matrix

## Value

a PfPR
