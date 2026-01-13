# Make an SIS **XH** model object

Make an SIS **XH** model object

## Usage

``` r
make_XH_obj_SIS(
  nStrata,
  options = list(),
  b = 0.55,
  r = 1/180,
  c = 0.15,
  d_lm = 0.8,
  d_rdt = 0.8,
  d_pcr = 0.9
)
```

## Arguments

- nStrata:

  is the number of population strata

- options:

  a named list with parameter values to overwrite defaults

- b:

  transmission probability (efficiency) from mosquito to human

- r:

  recovery rate

- c:

  transmission probability (efficiency) from human to mosquito

- d_lm:

  detection by light microscopy

- d_rdt:

  detection by RDT

- d_pcr:

  detection by pcr

## Value

an **XH** model object
