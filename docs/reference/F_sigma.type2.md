# Patch emigration rate

Implements a type 2 functional response to the availability of three
resources: blood hosts (\\B\\), aquatic habitats (\\Q\\), and sugar
(\\S\\).

\$\$\sigma = \sigma_X \left( \frac{\sigma_Q Q}{1+\sigma_Q Q} +
\frac{\sigma_B B}{1+\sigma_B B} + \frac{\sigma_S S}{1+\sigma_S S}
\right)\$\$

## Usage

``` r
# S3 method for class 'type2'
F_sigma(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\\sigma\\, the patch emigration rate
