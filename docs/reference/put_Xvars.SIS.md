# Return the SIS model variables as a list, returned from Update_Xt.SIS

This method dispatches on the type of `pars$Xpar`

## Usage

``` r
# S3 method for SIS
put_Xvars(Xvars, y, pars, i)
```

## Arguments

- Xvars:

  the X variables to insert into y

- y:

  the variables

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)
