# Artzy-Randrup et al. (2015): model equations

Source: Artzy-Randrup Y, Dobson AP, Pascual M. *Synergistic and antagonistic
interactions between bednets and vaccines in the control of malaria.*
PNAS 112(10):3014-3019.
[DOI: 10.1073/pnas.1409467112](https://doi.org/10.1073/pnas.1409467112).

Local appendix: [pnas.1409467112.sapp.pdf](../pnas.1409467112.sapp.pdf).

The generalized model is on PDF page 4 of the appendix: eight human states per
age group, plus susceptible and infectious mosquitoes. The equations below
were checked against a rendered image of that page because automatic text
extraction scrambled fractions and subscripts.

This is an algebraically equivalent transcription using shorthand, not a
verbatim reproduction of the page's layout. Potentially questionable terms
have been kept as printed and are flagged afterward.

## Notation and transmission terms

For each age group \(i=1,\ldots,n\), let

\[
S_i,\ I_i,\ R_i,\ A_i
\]

denote fully susceptible, clinically infected, partially immune, and
asymptomatically infected hosts. Their vaccinated counterparts are

\[
V_{S,i},\ V_{I,i},\ V_{R,i},\ V_{A,i}.
\]

Define the mosquito-to-human infection pressure for host state \(X\) by

\[
\lambda_{X,i}
=
\frac{\beta}{N_H}(1-b+\varepsilon b)\,
\tau_{MH,X(i)}M_I,
\qquad X\in\{S,R,V_S,V_R\}.
\]

Define the human-to-mosquito infection pressure by

\[
\lambda_M
=
\frac{\beta}{N_H}(1-b+\varepsilon b)
\sum_{j\in\{I,A,V_I,V_A\}}
\sum_{i=1}^{n}
j_i\,\tau_{HM,j(i)},
\]

and the mosquito mortality term by

\[
d_M=\mu_M+\zeta\beta b(1-\varepsilon).
\]

Here, \(\mathcal K_{X,i}\) denotes the demographic and aging term defined below.

## Unvaccinated hosts

\[
\begin{aligned}
\dot S_i={}&
\mathcal K_{S,i}
-\lambda_{S,i}S_i
+(1-\pi_i)\sigma_{I(i)}I_i
-\nu_{S(i)}S_i
+\omega_{S(i)}V_{S,i}
+\delta_iR_i,
\\[3pt]
\dot I_i={}&
\mathcal K_{I,i}
+\lambda_{S,i}S_i
-\sigma_{I(i)}I_i,
\\[3pt]
\dot R_i={}&
\mathcal K_{R,i}
-\lambda_{R,i}R_i
+\pi_i\sigma_{I(i)}I_i
+\sigma_{A(i)}A_i
-\nu_{R(i)}R_i
+\omega_{R(i)}V_{R,i}
-\delta_iR_i,
\\[3pt]
\dot A_i={}&
\mathcal K_{A,i}
+\lambda_{R,i}R_i
-\sigma_{A(i)}A_i.
\end{aligned}
\]

The recovery terms imply that \(\pi_i\) is the fraction of clinically infected,
unvaccinated hosts who enter the partially immune state upon clearance; the
remainder return to full susceptibility.

## Vaccinated hosts

\[
\begin{aligned}
\dot V_{S,i}={}&
\mathcal K_{V_S,i}
-\lambda_{V_S,i}V_{S,i}
+\nu_{S(i)}S_i
-\omega_{S(i)}V_{S,i}
+\delta_iV_{R,i},
\\[3pt]
\dot V_{I,i}={}&
\mathcal K_{V_I,i}
+\lambda_{V_S,i}V_{S,i}
-\sigma_{I(i)}V_{I,i},
\\[3pt]
\dot V_{R,i}={}&
\mathcal K_{V_R,i}
-\lambda_{V_R,i}V_{R,i}
+\sigma_{I(i)}V_{I,i}
+\sigma_{A(i)}V_{A,i}
+\nu_{R(i)}R_i
-\omega_{R(i)}V_{R,i}
-\delta_iV_{R,i},
\\[3pt]
\dot V_{A,i}={}&
\mathcal K_{V_A,i}
+\lambda_{V_R,i}V_{R,i}
-\sigma_{A(i)}V_{A,i}.
\end{aligned}
\]

## Mosquitoes

\[
\begin{aligned}
\dot M_S &= B_M-d_MM_S-\lambda_MM_S,\\
\dot M_I &= \lambda_MM_S-d_MM_I.
\end{aligned}
\]

Thus, this formulation has no exposed mosquito compartment or explicit
extrinsic-incubation delay: infection moves mosquitoes directly from \(M_S\)
to \(M_I\).

## Demography and aging

For the youngest age group:

\[
\mathcal K_{S,1}
=
B_H-S_1\left(\kappa_{1,2}+\alpha_{S(1)}\right),
\]

and for every other host state \(j\),

\[
\mathcal K_{j,1}
=
-j_1\left(\kappa_{1,2}+\alpha_{j(1)}\right).
\]

For intermediate age groups \(i=2,\ldots,n-1\):

\[
\mathcal K_{j,i}
=
j_{i-1}\kappa_{i-1,i}
-j_i\left(\kappa_{i,i+1}+\alpha_{j(i)}\right).
\]

For the oldest age group, the appendix prints:

\[
\mathcal K_{j,n}
=
j_{n-1}\kappa_{n-1,n}
-j_n\alpha_{j(n-1)}.
\]

Here \(j\) ranges over all eight host states, except that the youngest
susceptible state has its separate birth-input expression.

## Important details to resolve before implementing

These are issues in the printed specification, not extraction failures.

### 1. Oldest-age mortality index

The final demographic expression uses \(\alpha_{j(n-1)}\), rather than the
apparently expected \(\alpha_{j(n)}\). It is preserved above; it looks like a
typographical error.

### 2. Different recovery routing for vaccinated hosts

Unvaccinated clinical infections split between \(S_i\) and \(R_i\) using
\(\pi_i\). Vaccinated clinical infections all recover into \(V_{R,i}\), with
no corresponding \(\pi_i\) split. This matters particularly for age groups
assumed unable to acquire protective immunity.

### 3. Vaccine waning occurs only in uninfected states

The printed equations include \(V_S\to S\) and \(V_R\to R\), but no waning
transitions out of \(V_I\) or \(V_A\). That is a substantive modeling choice
if implemented literally.

### 4. Insecticidal mortality notation varies

The equations use \(\zeta\), while the parameter table and bednet prose use
\(\xi\) for the probability of mosquito death following a net encounter.

### 5. The bednet prose and equation factors need reconciliation

Page 5 describes biting on an unprotected host using

\[
\frac{\beta}{N}\left(1+\frac{\varepsilon b}{1-b}\right),
\]

whereas the population equations use

\[
\frac{\beta}{N_H}(1-b+\varepsilon b).
\]

These are not identical quantities; translating between them requires being
explicit about whether the rate is conditional on being unprotected or
averaged over the population.

## Conclusion

The model can be transcribed reliably, but the specification details above
should be resolved before treating it as an unambiguous implementation recipe.
