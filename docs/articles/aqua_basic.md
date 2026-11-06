# Basic L

The basicL_xde competition aquatic mosquito model fulfills the generic
interface of the aquatic mosquito component. It has a single compartment
“larvae” for each aquatic habitat, and mosquitoes in that aquatic
habitat suffer density-independent and dependent mortality, and mature
at some rate \\(\psi\\).

## Differential Equations

Given \\(\Lambda\\) and some egg laying rate from the adult mosquito
population we could formulate and solve a dynamical model of aquatic
mosquitoes to give that emergence rate. However, in the example here we
will simply use a trivial-based (forced) emergence model, so that
\\(\Lambda\\) completely specifies the aquatic mosquitoes.

The simplest model of aquatic (immature) mosquito dynamics with negative
feedback (density dependence) is:

\\\[\dot{L} = \eta - (\psi+\phi+\theta L)L\\\]

Because the equations allow the number of larval habitats \\(l\\) to
differ from \\(p\\), in general the emergence rate is given by:

\\\[\Lambda = N \cdot \alpha\\\]

Where \\(N\\) is a \\(p\times l\\) matrix and \\(\alpha\\) is a length
\\(l\\) column vector given as:

\\\[\alpha = \psi L\\\]

## Equilibrium solutions

In general, if we know the value of \\(\Lambda\\) at equilibrium we can
solve for \\(L\\) directly by using the above two equations. Then we can
consider \\(\theta\\), the strength of density dependence to be unknown
and solve such that:

\\\[\theta = (\eta - \psi L - \phi L) / L^2\\\]

## Example

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ramp.xds`](https://dd-harp.github.io/ramp.xds/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`deSolve`](https://github.com/tpetzoldt/deSolve/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`data.table`](https://r-datatable.com)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`

### The long way

Here we run a simple example with 3 aquatic habitats at equilibrium. We
use `ramp.xds::make_parameters_L_basicL_xde` to set up parameters.
Please note that this only runs the aquatic mosquito component and that
most users should read [our fully worked
example](https://dd-harp.github.io/ramp.xds/articles/ex_534.md) to run a
full simulation.

\
`nHabitats`` ``<-`` ``3`\
`nPatches`` ``<-`` ``nHabitats`\
`membership`` ``<-`` ``1``:``nPatches`\
`params`` ``<-`` `[`make_xds_object_template`](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)`(``"ode"``, ``"aquatic"``, ``nPatches``,  ``membership``, ``1``)`

\
`alpha`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``50``, ``20``)`\
`eta`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``250``, ``500``, ``170``)`\
`psi`` ``<-`` ``1``/``10`\
`phi`` ``<-`` ``1``/``12`\
`L`` ``<-`` ``alpha``/``psi`\
`theta`` ``<-`` ``(``eta`` ``-`` ``psi``*``L`` ``-`` ``phi``*``L``)``/``(``L``^``2``)`\
`Lo`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``eta``=``eta``, psi``=``psi``, phi``=``phi``, theta``=``theta``, L``=``L``)`\
`MYZo`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``MYZm`` ``<-`` ``eta``)`` `

\
`params``$``eggs_laid`` ``=`` ``eta`` `\
`F_eta`` ``=`` ``function``(``t``, ``pars``)``{`\
`  ``pars``$``eggs_laid`\
`}`\
\
`params`` ``=`` `[`setup_L_obj`](https://dd-harp.github.io/ramp.xds/reference/setup_L_obj.md)`(``"basicL"``, ``membership``, ``params``, ``1``, ``Lo``)`\
`params`` ``=`` `[`setup_MY_obj`](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.md)`(``"trivial"``, ``params``, ``1``, ``MYZo``)`\
\
`params`` ``=`` `[`make_indices`](https://dd-harp.github.io/ramp.xds/reference/make_indices.md)`(``params``)`

\
`xDE_aquatic`` ``=`` ``function``(``t``, ``y``, ``pars``, ``F_eta``)`` ``{`\
`  ``pars``$``terms``$``eta``[[``1``]``]`` ``<-`` `[`F_eta`](https://dd-harp.github.io/ramp.xds/reference/F_eta.md)`(``t``, ``pars``)`\
`  ``dL`` ``<-`` `[`dLdt`](https://dd-harp.github.io/ramp.xds/reference/dLdt.md)`(``t``, ``y``, ``pars``, ``1``)`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(`[`list`](https://rdrr.io/r/base/list.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``dL``)``)``)`\
`}`

\
`y0`` ``<-`` `[`get_inits`](https://dd-harp.github.io/ramp.xds/reference/get_inits.md)`(``params``, flatten``=``TRUE``)`` `\
\
`out`` ``<-`` ``deSolve``::`[`ode`](https://rdrr.io/pkg/deSolve/man/ode.html)`(``y ``=`` `[`as.vector`](https://rdrr.io/r/base/vector.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``y0``)``)``, times ``=`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``0``,``50``,by``=``10``)``, ``xDE_aquatic``, parms ``=`` ``params``, method ``=`` ``'lsoda'``, F_eta ``=`` ``F_eta``)`` `\
`out1`` ``<-`` ``out`

\
[`colnames`](https://rdrr.io/r/base/colnames.html)`(``out``)``[``params``$``L_obj``[[``1``]``]``$``ix``$``L_ix``+``1``]`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``'L_'``, ``1``:``params``$``nHabitats``)`\
\
`out`` ``<-`` `[`as.data.table`](https://rdrr.io/pkg/data.table/man/as.data.table.html)`(``out``)`\
`out`` ``<-`` `[`melt`](https://rdrr.io/pkg/data.table/man/melt.data.table.html)`(``out``, id.vars ``=`` ``'time'``)`\
`out``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"Component"``, ``"Patch"``)`` ``:=`` `[`tstrsplit`](https://rdrr.io/pkg/data.table/man/tstrsplit.html)`(``variable``, ``'_'``, fixed ``=`` ``TRUE``)``]`\
`out``[``, ``variable`` ``:=`` ``NULL``]`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``out``, mapping ``=`` `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``time``, y ``=`` ``value``, color ``=`` ``Patch``)``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(``)`` ``+`\
`  `[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

![](aqua_basic_files/figure-html/unnamed-chunk-8-1.png)

### Using Setup

The function `xds_setup_aquatic` sets up a model that includes only
aquatic dynamics, and it is solved using `xds_solve.aqua.` The setup
functions are simpler than `xds_setup` and come with constrained
choices. The user can configure any aquatic model (`trivial` wouldn’t
make much sense), and it uses `trivial` to force egg laying.

We configure the aquatic model: `Lo` is a list with the parameter values
attached.

\
`Lo`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  psi ``=`` ``1``/``10``, `\
`  phi ``=`` ``1``/``12`\
`)`\
`alpha`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``50``, ``20``)`\
`Lo``$``L`` ``=`` `[`with`](https://rdrr.io/r/base/with.html)`(``Lo``, ``alpha``/``psi``)`` `\
`Lo``$``theta`` ``=`` `[`with`](https://rdrr.io/r/base/with.html)`(``Lo``, ``(``eta`` ``-`` ``psi``*``L`` ``-`` ``phi``*``L``)``/``(``L``^``2``)``)`

We use the MYZ model `trivial` to configure egg laying.

\
`Mo`` ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``MYZm ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``250``, ``500``, ``170``)``)`

\
[`xds_setup_aquatic`](https://dd-harp.github.io/ramp.xds/reference/xds_setup_aquatic.md)`(``nHabitats``=``3``, Lname ``=`` ``"basicL"``, Loptions ``=`` ``Lo``, MYoptions ``=`` ``Mo``)`` ``->`` ``aqbasicL_xde`\
[`xds_solve`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)`(``aqbasicL_xde``, Tmax``=``50``, dt``=``10``)``$``output``$``orbits``$``deout`` ``->`` ``out2`\
[`sum`](https://rdrr.io/r/base/sum.html)`(`[`abs`](https://rdrr.io/r/base/MathFun.html)`(``out1``-``out2``)``)`` ``==`` ``0`

    ## [1] TRUE
