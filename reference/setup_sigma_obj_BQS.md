# Setup Blood Feeding Bionomic Object

Set up an object to compute dynamic blood feeding rates as a type 2
functional response to availability of blood hosts, \\B\\

## Usage

``` r
setup_sigma_obj_BQS(
  MY_obj,
  options = list(),
  sigma_x = 1,
  sigma_B = 3,
  sigma_Q = 3,
  sigma_S = 3,
  sB = 1,
  sQ = 1,
  sS = 1
)
```

## Arguments

- MY_obj:

  an **`MY`** model object

- options:

  an **`MY`** model object

- sigma_x:

  a scaling parameter

- sigma_B:

  maximum rate, blood searching

- sigma_Q:

  maximum rate, habitat searching

- sigma_S:

  maximum rate, sugar searching

- sB:

  shape parameter, blood searching

- sQ:

  shape parameter, habitat searching

- sS:

  shape parameter, sugar searching

## Value

a **`MY`** model object
