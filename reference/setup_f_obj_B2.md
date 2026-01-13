# Setup Blood Feeding Bionomic Object

Set up an object to compute dynamic blood feeding rates as a type 2
functional response to availability of blood hosts,
\$\$f=F_feeding_rate(B) = \frac{f_x s_f B}{1+s_f B}\$\$

Set up an object to compute dynamic blood feeding rates as a type 2
functional response to availability of blood hosts, \\B\\

## Usage

``` r
setup_f_obj_B2(MY_obj, options = list(), fx = 0.35, sf = 1)

setup_f_obj_B2(MY_obj, options = list(), fx = 0.35, sf = 1)
```

## Arguments

- MY_obj:

  an **`MY`** model object

- options:

  a list of options

- fx:

  the maximum blood feeding rate

- sf:

  a shape parameter

## Value

a **`MY`** model object

a **`MY`** model object
