# Net infectiousness of human population to mosquitoes

Compute the net infectiousness of humans in each patch at time `t`

## Usage

``` r
F_kappa(Wi, W, beta, X)
```

## Arguments

- Wi:

  availability of this host

- W:

  availability of all hosts

- beta:

  the mixing matrix

- X:

  the infectious density of the strata

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
