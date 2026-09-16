# Setup mass treatment

Add mass treatment ports to modules.

## Usage

``` r
setup_mass_treatment(
  xds_obj,
  mda = F_zero,
  msat = F_zero,
  options = list(),
  i = 1
)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- mda:

  a function of the form \\F(t,V)\\

- msat:

  a function of the form \\F(t,V)\\

- options:

  options list (overrides command line arguments)

- i:

  the species index

## Value

an **`xds`** object
