# Set up blood traps

If an options list is passed as the first argument, then set

- `BTname = name$name`

- `options = name` and call
  `setup_blood_traps(BTname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_blood_traps(name, xds_obj, options = list(), s = 1)
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
