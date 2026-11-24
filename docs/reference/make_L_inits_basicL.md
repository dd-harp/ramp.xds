# Make initial values for `basicL` (**L**)

Create a named list of initial values for the `basicL` module. Values in
`options` override the defaults. Initial values of the variable \\(L\\)
can be set by passing `L` in `options`.

## Usage

``` r
make_L_inits_basicL(nHabitats, options = list(), L = 1)
```

## Arguments

- nHabitats:

  the number of habitats in the model

- options:

  a [list](https://rdrr.io/r/base/list.html) that overrides default
  values

- L:

  initial conditions

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial values must be numeric scalars or have length `nHabitats`.
Scalars are expanded to that length.

## See also

[setup_L_inits.basicL](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.basicL.md),
[change_L_inits.basicL](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.basicL.md)
