# Compute the prevalence of infection by RDT

A function that translates the state variables into the predicted *Pf*PR
by rapid diagnostic test (RDT)

## Usage

``` r
F_pfpr_by_rdt(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for human

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
