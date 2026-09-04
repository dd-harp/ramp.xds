# Solve Cohort Dynamics

Given a trace function describing the average EIR in a population over
time, simulate malaria in a cohort as it ages: the independent variable
is age \\(a)\\ and not time \\(t)\\.

Relative biting rates by age are set by \\F\_\omega(a)\\ or `F_age` with
associated parameters `age_par`.

To solve, the argument `birthday` sets the birthday for a cohort,
\\(d)\\. The user ether passes `ages` or `Amax` and `da` to configure a
vector of ages at which output is wanted \\(a_i)\\, at times
\\t_i=a_i+d\\.

In effect, since \\a = t-d\\, the function solves the solves the system
for for an exposure function with the pattern: \$\$F_w(a) \times
F_S(t-d) \times F_T(t-d) \times F_K(t-d)\$\$

## Usage

``` r
xds_solve_cohort(xds_obj, birthday = 0, Amax = 365, da = 1, ages = NULL)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- birthday:

  a cohort's birthday

- Amax:

  the oldest year, run from 0...Amax

- da:

  the age interval

- ages:

  a set of ages

## Note

Use `xds_setup_eir` to set up a model for cohort dynamics.

During setup, the variable `xds_obj$EIR_obj$bday` is set to 0, and
`F_age` is set to `F_one`.
