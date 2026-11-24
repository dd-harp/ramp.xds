# Time Away

- \\\delta\\:

  time away

Time away is defined by travel, or time spent outside the patches. Time
away is used to model travel malaria in
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md),
and it is used to compute host availability.

**Travel Malaria**

The risk of malaria while traveling is computed in
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md) as
the product of time away and the travel EIR \\(E\_\delta)\\. Daily
travel risk is \$\$\delta E\_\delta\$\$

**Host Availability**

If human population density is \\H\\, but since a fraction \\\delta\\ is
traveling, the expected density of humans at any point in time is
\$\$(1-\delta)H.\$\$

## See also

[setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
[human_availability](https://dd-harp.github.io/ramp.xds/reference/human_availability.md)
