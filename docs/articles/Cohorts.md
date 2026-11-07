# Cohort Dynamics

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ramp.xds`](https://dd-harp.github.io/ramp.xds/)`)`

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
  \\(\tau\\) using the identity \\(t = a + \tau,\\) and adding an
  age-specific frailty \\(\omega(a).\\) Solutions use
  `xds_solve_cohort,` which accepts the cohort’s birthday as an optional
  argument. If \\(\omega(a) = 1\\), and \\(\tau=0,\\) then
  `xds_solve.eir` and `xds_solve_cohort` would give the same answers.

## Example

To illustrate, we set up the same model using three different setup
functions.

\
`Sp`` ``<-`` `[`makepar_F_sin`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sin.md)`(``)`\
`F_s`` ``<-`` `[`make_function`](https://dd-harp.github.io/ramp.xds/reference/make_function.md)`(``Sp``)`\
`Tp`` ``<-`` `[`makepar_F_spline`](https://dd-harp.github.io/ramp.xds/reference/makepar_F_spline.md)`(``tt``=``365``*`[`c`](https://rdrr.io/r/base/c.html)`(``0``:``5``)``, yy``=`[`c`](https://rdrr.io/r/base/c.html)`(``1``,``1``,``1.6``,``.3``,``.7``,``1``)``)`\
`F_t`` ``<-`` `[`make_function`](https://dd-harp.github.io/ramp.xds/reference/make_function.md)`(``Tp``)`

\
`tt`` ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``0``, ``5``*``365``, by ``=`` ``10``)`\
[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``tt``, ``F_s``(``tt``)``, type ``=``"l"``, ylab ``=`` ``"Seasonality, Trend"``, xlab ``=`` ``"Time (in Days)"``)`\
[`lines`](https://rdrr.io/r/graphics/lines.html)`(``tt``, ``F_t``(``tt``)``, type ``=``"l"``)`

![](Cohorts_files/figure-html/unnamed-chunk-4-1.png)

\
`opts`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``Z``=``2000``/``365``, season_par ``=`` ``Sp``, trend_par ``=`` ``Tp``)`\
`mod_human`` ``<-`` `[`xds_setup_human`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)`(``HPop``=``1000``, XHoptions ``=`` ``opts``)`\
`mod_human`` ``<-`` `[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``mod_human``, Tmax``=``5``*``365``, dt``=``10``)`\
[`xds_plot_PR`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_PR.md)`(``mod_human``)`

![](Cohorts_files/figure-html/unnamed-chunk-5-1.png)

With this value of \\(Z\\) and a population size of \\(H=1000\\), the
scaling function gives us an annual EIR of 2.

\
`mod_eir`` ``<-`` `[`xds_setup_eir`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_eir.md)`(``eir``=``2``/``365``, season_par ``=`` ``Sp``, trend_par ``=`` ``Tp``)`\
`mod_eir`` ``<-`` `[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``mod_eir``, Tmax``=``365``*``5``, dt``=``10``)`\
[`xds_plot_PR`](https://dd-harp.github.io/ramp.xds/reference/xds_plot_PR.md)`(``mod_eir``)`

![](Cohorts_files/figure-html/unnamed-chunk-6-1.png)
