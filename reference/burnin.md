# Burn In

Run the model from some time in the remote past (\\t\_{-\infty}\\) up to
the present (\\t=0\\), and then reset the initial conditions.

## Usage

``` r
burnin(xds_obj, t_neg_inf = -3650)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- t_neg_inf:

  days before the present, \\t\_{-\infty}\\

## Value

a **`ramp.xds`** xds_obj object

## Note

The algorithm sets \\t\_{-\infty}\\ to \\-\|\\`t_neg_inf`\\\|\\, the
negative absolute value of `t_neg_inf`
