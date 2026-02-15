# Diagnostics, Detection, & Prevalence

All malaria modules developed for the **XH** component must define four
functions to compute malaria prevalence (traditionally called the
parasite rate). Several species of malaria parasites infect humans, so
we must be more specific and define *Plasmodium falciparum* Parasite
Rate or *Pf*PR.

- *true* - the true *Pf*PR

- *lm* - the *Pf*PR by light microscopy

- *rdt* - the PR by rapid diagnostic test

- *pcr* - the PR by PCR

## `get_PR`
