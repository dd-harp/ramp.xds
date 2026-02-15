# **XH** Skill Set

The **XH** skill set is a list of an module's capabilities:

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

- **`pf_pcr`**: if FALSE, the model outputs a value for prevalence by
  PCR

## Usage

``` r
skill_set_XH(Xname)
```

## Arguments

- Xname:

  the **XH** module name

## Value

*XH* module skill set, as a list
