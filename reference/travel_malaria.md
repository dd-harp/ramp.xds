# Travel Malaria

Travel malaria is defined as exposure to malaria acquired outside the
area. The user can configure two ports:

- [time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md)
  or \\\delta\\:

  the fraction of time spent outside the area

- [travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md)
  or \\E\_\delta\\:

  the average daily EIR experienced during travel

The rate of exposure to malaria for each population stratum while
traveling is: \$\$\delta E\_\delta\$\$

The terms are integrated into the interfaces that compute blood feeding,
exposure, and transmission.

## See also

[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[xds_info_exposure](https://dd-harp.github.io/ramp.xds/reference/xds_info_exposure.md)
