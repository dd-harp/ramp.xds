# Macdonald-style adult mosquito bionomics

Reset the effect trivialzes for static models

When modules are added to compute effect sizes from baseline parameters,
those functions store an effect size. The total effect size is the
product of the effect sizes for each intervention. since coverage could
be changing dynamically, these must be reset each time the derivatives
are computed.

## Usage

``` r
# S3 method for class 'trivial'
MBaseline(t, y, xds_obj, s)
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
