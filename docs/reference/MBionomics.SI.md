# Macdonald-style adult mosquito bionomics

Reset the effect SIzes for static models

When modules are added to compute effect SIzes from baseline parameters,
those functions store an effect SIze. The total effect SIze is the
product of the effect SIzes for each intervention. SInce coverage could
be changing dynamically, these must be reset each time the derivatives
are computed.

## Usage

``` r
# S3 method for class 'SI'
MBionomics(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

the model as a [list](https://rdrr.io/r/base/list.html)
