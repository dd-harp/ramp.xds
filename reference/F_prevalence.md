# Compute the *true* prevalence of infection / parasite rate

A function that translates the state variables into the "true" *Pf*PR

## Usage

``` r
F_prevalence(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for human

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
