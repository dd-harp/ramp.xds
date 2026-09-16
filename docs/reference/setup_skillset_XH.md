# **XH** skill set

The **XH** skill set is a list of a module's capabilities:

- **`H_dynamics`** - if FALSE, \\H\\ is static. The module doesn't have
  a port for demographic change

- **`mda`**: if FALSE, the module doesn't have an `mda` port for mass
  drug administration

- **`msat`**: if FALSE, the module doesn't have an `msat` port for mass

- **`malaria`**: if FALSE, the model probably shouldn't be used for
  malaria

- **`pr_obs`**: if TRUE, the model outputs a value for observed PR

- **`pf_rdt`**: if TRUE, the model outputs a value for prevalence by RDT

- **`pf_lm`**: if TRUE, the model outputs a value for prevalence by
  light microscopy

- **`pf_pcr`**: if TRUE, the model outputs a value for prevalence by PCR

## Usage

``` r
setup_skillset_XH(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the species index

## Value

*XH* module skill set, as a list
