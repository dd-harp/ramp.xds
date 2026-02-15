# Compute infection prevalence by PCR

A function that translates the state variables into the predicted *Pf*PR
by PCR

## Usage

``` r
F_pr_by_pcr(vars, Xpar)
```

## Arguments

- vars:

  a list with the variables attached by name

- Xpar:

  a list defining a model for human

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## Note

This method dispatches on the type of `pars$Xpar[[i]]`.
