# Set up `L_obj` for **L** Component modules

Each instance of `setup_L_obj.*` calls a function `create_L_obj_*` that
creates an object **`L_obj`.** It is attached the **`xds`** object as
`xds_obj$L_obj[[s]].` Each instance of `create_L_obj_*` should assign
default parameter values that will be over-written by `options`

## Usage

``` r
setup_L_obj(Lname, xds_obj, s, options = list())
```

## Arguments

- Lname:

  the class name of the **L** module

- xds_obj:

  an **`xds`** model object

- s:

  the species index

- options:

  a named list to configure **`L_obj`**

## Value

an **`xds`** object

## Note

This method assigns `Lname` to class(`Lname`) and dispatches on `Lname`.
