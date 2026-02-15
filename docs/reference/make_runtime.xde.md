# Set up run-time time step support for `xde` models

Continuous time models don't use run-time support, so this returns the
`xds` object without modification

## Usage

``` r
# S3 method for xde
make_runtime(pars, Xday, MYZday, Lday, Lname)
```

## Arguments

- pars:

  an `xds` object

- Xday:

  is the run-time time step for X component (in days): integer or
  1/integer

- MYZday:

  is the run-time time step for MYZ component (in days): integer or
  1/integer

- Lday:

  is the run-time time step for L component (in days): integer or
  1/integer

- Lname:

  is the S3 class of the L model

## Value

the unmodified `xds` object
