# Make initial values for `hMoI` (**XH**)

Create a named list of initial values for the `hMoI` module. Values in
`options` override the defaults. MoI stands for Multiplicity of
Infection, and refers to malarial superinfection.

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

  mean MoI for all infections

- m2:

  mean MoI for apparent infections

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial values must be numeric scalars or have length `nStrata`. Scalars
are expanded to that length.

## State Variables

- `m1` or \\(m_1\\):

  mean MoI for all infections

- `m2` or \\(m_2\\):

  mean MoI for apparent (detectable) infections

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)

[setup_XH_inits.hMoI](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.hMoI.md),
[change_XH_inits.hMoI](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.hMoI.md)
