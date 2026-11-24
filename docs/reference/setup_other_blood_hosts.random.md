# Set up other blood hosts

Generate random other blood host availability using
[make_other_blood_hosts_random](https://dd-harp.github.io/ramp.xds/reference/make_other_blood_hosts_random.md).

## Usage

``` r
# S3 method for class 'random'
setup_other_blood_hosts(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a setup method name or a numeric vector of availability values

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** model object
