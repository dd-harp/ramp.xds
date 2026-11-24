# Change the delayed maturation parameter (**L**)

Change the stored delayed maturation parameter per habitat for a static
`basicL` model.

## Usage

``` r
change_xi(xi, xds_obj, s)
```

## Arguments

- xi:

  a numeric delayed maturation parameter for all habitats or a vector of
  length `nHabitats`

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

Store the supplied values at `xds_obj$L_obj[[s]]$xi`. When `xi_obj` is
present, also update `xi_t` and `xi_obj$xi` on that module object. The
port class and `es_xi` are unchanged; this function does not evaluate
ports or apply effect sizes. Inputs must be numeric, with length 1 or
`nHabitats`; scalar values are repeated across habitats. Invalid types
or lengths cause an error.

## Parameters

- `psi` or \\(\psi\\):

  maturation rate

- `xi` or \\(\xi\\):

  delayed maturation parameter in response to mean crowding

- `phi` or \\(\phi\\):

  density-independent mortality rate

- `theta` or \\(\theta\\):

  slope of density-dependent mortality

## See also

[get_xi](https://dd-harp.github.io/ramp.xds/reference/get_xi.md)
