# Set up other blood hosts

Pass a pre-configured other_blood_hosts. If it passes the checks, it
replaces the current other_blood_hosts.

If called with `name = "as_is"`, provide the vector in
`options$other_blood_hosts_asis`.

## Usage

``` r
# S3 method for class 'as_is'
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
