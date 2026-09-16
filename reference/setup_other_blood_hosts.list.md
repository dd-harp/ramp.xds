# Set up a other_blood_hosts

If an options list is passed as the first argument, then set

- `OBHname = name$name`

- `options = name` and call
  `setup_other_blood_hosts(OBHname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
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
