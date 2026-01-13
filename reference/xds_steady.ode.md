# Solve for the steady state of a system of equations using [rootSolve::steady](https://rdrr.io/pkg/rootSolve/man/steady.html)

Solve for the steady state of a system of equations using
[rootSolve::steady](https://rdrr.io/pkg/rootSolve/man/steady.html)

## Usage

``` r
# S3 method for class 'ode'
xds_steady(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object

## Note

This method dispatches on `class(dlay)`
