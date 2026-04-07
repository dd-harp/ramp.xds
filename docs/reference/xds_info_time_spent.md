# time spent

The risk of exposure to mosquito-borne pathogens is related to time
spent in places where vectors are blood feeding. Each human / host
population resides in a patch. Let \\N_p\\ denote the number of patches
and \\N_h\\ the number of strata. The time spent matrix, \\\Theta\\, is
an \\N_p \times N_h\\ matrix: each columns describes the fraction of
time spent by a single population stratum in each patch. It is expected
that most time is spent in the patch where the stratum resides.

In **`ramp.xds`,** the time spent matrix is static. In
**`xds.forcing`**, time spent can have a daily pattern, and time at risk
weights time spent by a function describing mosquito daily activity
rates (see
[xds_info_time_at_risk](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_at_risk.md)).
