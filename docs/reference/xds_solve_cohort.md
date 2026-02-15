# Cohort dynamics for a human / host model

Compute the states for a model \\\cal X\\ in a cohort of humans / hosts
as it ages, up to age \\A\\ years of age

## Usage

``` r
xds_solve_cohort(xds_obj, bday = 0, A = 10, da = 10)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- bday:

  the cohort birthday

- A:

  the maximum age to compute (in years)

- da:

  the output interval (age, in days)

## Value

an **`xds`** object

## Details

This method substitutes age for time: a model \$\$\cal X(t)\$\$ is
solved with respect to age \\a\\: \$\$\cal X(a),\$\$ where the daily EIR
is computed by a *trace* function with four elements:

- \\\bar E\\ or `eir`, the mean daily EIR,

- \\\omega(a)\\ or `F_age,` a function of age

- \\S(t)\\ or `F_season,` a function of time of year

- \\T(t)\\ or `F_trend,` a function describing a trend

For a cohort born on day \\B\\, the function creates a mesh on age /
time, where time and age are related by the formula: \$\$t = B + a\$\$
and the trace function is: \$\$E(a, t) = \hat E \\ \omega(a) \\ S(t)\\
T(t) \$\$ The output is returned as `xds_obj$outputs$cohort`
