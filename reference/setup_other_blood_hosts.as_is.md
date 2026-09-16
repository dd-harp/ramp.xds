# Set up a other_blood_hosts

Pass a pre-configured other_blood_hosts. If it passes the checks, it
replaces the current other_blood_hosts.

If called with `name = "as_is"`, the other_blood_hosts must be at
`options$other_blood_hosts`

## Usage

``` r
# S3 method for class 'as_is'
setup_other_blood_hosts(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the vector species index

## Value

an **`xds`** object
