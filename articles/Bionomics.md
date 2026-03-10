# Mosquito Bionomics

In **`ramp.xds,`** some mosquito bionomic parameters are handled as
*ports.* This includes some adult mosquito modules (**MY** Component)
and some aquatic modules (**L** Component). These ports share some
common features:

- During *basic setup,* these parameters are set to constant values;
  basic setup and the port are set up in **`ramp.xds`**

- Forcing is an *advanced setup* option: the value of these parameters
  can vary over time in response to other variables, such as
  temperature, wind, or soil moisture levels. Advanced setup options are
  in **`ramp.forcing`**

- Various modes of vector control are handled as another set of
  *advanced setup* options: advanced setup options for vector control
  are in **`ramp.control.`** Vector control effects can be handled in
  one of two ways:

  - vector control can affect the availability of resources, with
    effects that are handled through

  - vector control effect sizes can be computed.

The module `macdonald` is an exception: it was not set up to handle
either forcing or vector control. We would say that it is a strictly
autonomous model: those capabilities are not in its *skill set.* To
handle forcing and vector control, we developed `GeRM.`

The flexible implementation
