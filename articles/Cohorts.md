# Cohort Dynamics

``` r
library(ramp.xds)
```

In **`ramp.xds`,** there are three different ways of simulating malaria
epidemiology in isolation. These are set up using different setup
functions:

- `xds_setup_human` sets up a full **X** component model, including a
  time spent matrix. The `trivial` **MYZ** module is calibrates the
  model `F_fqZ,` and the model computes the dEIR as an output.

- `xds_setup_eir` sets up an **X** component for a single human
  population stratum. The user configures a function `F_eir.` Solutions
  to these models uses `xds_solve.eir`

- `xds_setup_cohort` sets up an **X** component for a single human
  population birth cohort as it ages. The user configures a function
  `F_eir` that computes the average EIR in the population with respect
  to time. The model for exposure translates population EIR into the
  specific pattern of exposure experienced by the cohort born on day
  \\\tau\\ using the identity \\t = a + \tau,\\ and adding an
  age-specific frailty \\\omega(a).\\ Solutions use `xds_solve_cohort,`
  which accepts the cohort’s birthday as an optional argument. If
  \\\omega(a) = 1\\, and \\\tau=0,\\ then `xds_solve.eir` and
  `xds_solve_cohort` would give the same answers.

## Example

To illustrate, we set up the same model using three different setup
functions.

``` r
Sp <- makepar_F_sin()
F_s <- make_function(Sp)
Tp <- makepar_F_spline(tt=365*c(0:5), yy=c(1,1,1.6,.3,.7,1))
F_t <- make_function(Tp)
```

``` r
tt = seq(0, 5*365, by = 10)
plot(tt, F_s(tt), type ="l", ylab = "Seasonality, Trend", xlab = "Time (in Days)")
lines(tt, F_t(tt), type ="l")
```

![](Cohorts_files/figure-html/unnamed-chunk-4-1.png)

``` r
opts = list(Z=2000/365, season_par = Sp, trend_par = Tp)
mod_human <- xds_setup_human(HPop=1000, XHoptions = opts)
mod_human <- xds_solve(mod_human, Tmax=5*365, dt=10)
xds_plot_PR(mod_human)
```

![](Cohorts_files/figure-html/unnamed-chunk-5-1.png)

With this value of \\Z\\ and a population size of \\H=1000\\, the
scaling function gives us an annual EIR of 2.

``` r
mod_eir <- xds_setup_eir(eir=2/365, season_par = Sp, trend_par = Tp)
mod_eir <- xds_solve(mod_eir, Tmax=365*5, dt=10)
xds_plot_PR(mod_eir)
```

![](Cohorts_files/figure-html/unnamed-chunk-6-1.png)
