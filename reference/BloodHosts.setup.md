# Time Spent F_blood_hosts

This function sets a static value for the parameter describing time
spent F_blood_hosts, resets the class to `static` and then triggers an
update to the blood feeding model. It is computed in
[compute_TaR](https://dd-harp.github.io/ramp.xds/reference/compute_TaR.md)
and used to compute availability. It is also used in
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)

## Usage

``` r
# S3 method for class 'setup'
BloodHosts(t, y, xds_obj)
```

## Arguments

- t:

  current time

- y:

  the state variable vector

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object
