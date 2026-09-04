# Time Spent

The risk of exposure to mosquito-borne pathogens is related to time
spent in places where vectors are blood feeding. Each human / host
population resides in a patch. Let \\N_p\\ denote the number of patches
and \\N_h\\ the number of strata. The time spent matrix, \\\Theta\\, is
an \\N_p \times N_h\\ matrix with the form: \$\$ {\Theta} = \left\[
\begin{array}{ccccc} j=1&j=2&j=3&\cdots&j=N_h \\ \boxed{
\begin{array}{c} {\theta}\_{1,1} \\ {\theta}\_{2,1} \\ {\theta}\_{3,1}
\\ \vdots \\ {\theta}\_{N_p,1} \\ \end{array}} & \boxed{
\begin{array}{c} {\theta}\_{1,2} \\ {\theta}\_{2,2} \\ {\theta}\_{3,2}
\\ \vdots \\ {\theta}\_{N_p,2} \\ \end{array}} & \boxed{
\begin{array}{c} {\theta}\_{1,3} \\ {\theta}\_{2,3} \\ {\theta}\_{3,3}
\\ \vdots \\ {\theta}\_{N_p,3} \\ \end{array}} & \boxed{
\begin{array}{c} \cdots \\ \cdots \\ \cdots \\ \ddots \\ \cdots
\end{array}} & \boxed{ \begin{array}{c} {\theta}\_{1,N_h} \\
{\theta}\_{2,N_h} \\ {\theta}\_{3,N_h} \\ \vdots \\ {\theta}\_{N_p,N_h}
\\ \end{array}} \end{array} \right\] \$\$

Each columns describes the fraction of time spent by a single population
stratum in each patch *when not traveling,* so \\\sum_i
\theta\_{i,j}\leq 1\\. Time away - or time spent outside of the spatial
domain - is handled separately (see
[xds_port_time_away](https://dd-harp.github.io/ramp.xds/reference/xds_port_time_away.md)).
If the columns do not sum up to one, then it implies that when a person
is not traveling, some time is spent in places within the spatial domain
where they are not at risk.

It is expected that most time is spent in the patch where the stratum
resides.

In **`ramp.xds`,** the time spent matrix is static. In
**`xds.forcing`**, time spent can have a daily pattern, and time at risk
weights time spent by a function describing mosquito daily activity
rates (see
[xds_info_time_at_risk](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_at_risk.md)).
