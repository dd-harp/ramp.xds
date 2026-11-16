# Make initial values for `hMoI` (**XH**)

MoI stands for Multiplicity of Infection, and refers to malarial
superinfection.

## Usage

``` r
make_XH_inits_hMoI(nStrata, options = list(), m1 = 2, m2 = 1)
```

## Arguments

- nStrata:

  the number of population strata

- options:

  a [list](https://rdrr.io/r/base/list.html) that overrides default
  values

- m1:

  mean MoI among inapparent human infections

- m2:

  mean MoI among patent human infections

## Value

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `m1` or \\m_1\\:

  mean MoI for all infections

- `m2` or \\m_2\\:

  mean MoI for apparent (detectable) infections

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
