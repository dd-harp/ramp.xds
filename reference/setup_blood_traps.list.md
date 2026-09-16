# Set up a blood_traps

If an options list is passed as the first argument, then set

- `OBHname = name$name`

- `options = name` and call
  `setup_blood_traps(OBHname, xds_obj, options, s)`

## Usage

``` r
# S3 method for class 'list'
setup_blood_traps(name, xds_obj, options = list(), s = 1)
```

## Arguments

- name:

  a or setup function name

- xds_obj:

  an **`xds`** model object

- options:

  configuration options

- s:

  the host species index

## Value

an **xds** object
