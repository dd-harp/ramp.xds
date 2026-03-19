# Modular Forms

The modular structure for computation implemented in **`ramp.xds`** can
be understood by rewriting the equations in **modular form,** a stylized
way of presenting a dynamical system that emphasizes the biological
agents involved in the underlying process. Here, we rewrite a simple
dynamical system in modular form, so the equations mirror the
implementation in `ramp.xds.` For a longer discussion and examples, see
[Modularity](https://dd-harp.github.io/ramp.xds/articles/modularity.md)

## A Standard Form

A mathematical framework for building modular models of malaria dynamics
and control (and other mosquito-borne pathogens) was described in
[Spatial Dynamics of Malaria
Transmission](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684),
and that framework has been implemented in `ramp.xds.`

There is no *standard form* for writing down systems of differential
equations, but some forms are preferred by scientists or mathematicians,
depending on training and context.

We start by writing one version of a Ross-Macdonald model as a system of
equations that does not emphasize the modularity, or *not a modular
form* (Box 1).

------------------------------------------------------------------------

**Box 1: A Ross-Macdonald Model**

- Let the dependent variable \\I(t)\\ denote the density of infected
  humans, and the parameter \\H\\ the density of all humans

- Let the dependent variable \\M(t)\\ denote the density of mosquitoes,
  and \\Y(t)\\ the density of infectious mosquitoes.

- Human Infections:

  - \\b\\ denotes the fraction of bites by infectious mosquitoes that
    infect a human; and

  - \\r\\ denotes the rate that infections clear.

- Mosquito Ecology:

  - \\\Lambda\\ denotes the emergence rate of adult, female mosquitoes;

  - \\g\\ denotes the mosquito death rate;

- Mosquito Blood Feeding & Infection Dynamics:

  - \\c\\ denotes the fraction of blood meals on infectious humans that
    infect a mosquito;

  - \\f\\ denotes the overall blood feeding rate; and

  - \\q\\ denotes the human blood fraction.

In this set of equations, human infections occur at the rate \\fqbY/H,\\
and mosquito infections at the rate \\fqcI/H.\\

\\ \begin{array}{rl} dI/dt &= fq b \frac {Y}{H} (H-I) - r I \\ dM/dt &=
\Lambda - g M \\ dY/dt &= fq c \frac {I}{H} (M-Y) - g Y \end{array} \\
Written in this way, it is in a *standard form.*

------------------------------------------------------------------------

## …in a Modular Form

We illustrate by rewriting this model in its modular form (Figure 1). In
the modular form, we identify the terms in one equation that depend on
the other variable. First, we focus on the equation describing human
infection dynamics. We note that it depends on a term involving the
variable \\Y.\\ The term is called the force of infection (FoI, Ross
called it the *happenings* rate). We let \\h\\ denote the FoI:

\\h=fq b\frac {Y}{H}.\\

Now, the first equation can be rewritten:

\\ dI/dt = h (H-I) - r I \\ Next, we isolate the equation describing
mosquito infection dynamics. Mosquito infections depend on a term that
we will call the net infectiousness (NI).

\\\kappa = c\frac{I}{H}\\

We note that \\fq\kappa\\ is the FoI for mosquito infections, and now we
can rewrite the third equation for mosquitoes:

\\ dY/dt = fq\kappa\frac {I}{H} (M-Y) - g Y \\

\\ dM/dt = \Lambda - g M \\

With these four elements, the system has a modular form. In this modular
form, there are three components (the SIS human model; the SI model; and
mosquito population dynamics) and two dynamical terms (\\h\\ and
\\\kappa\\).

------------------------------------------------------------------------

![Figure 1 - Diagram of a Ross-Macdonald model in Box 1, rewritten in a
modular form.](RossMacdonaldFormal.png)

**Figure 1** - Diagram of a Ross-Macdonald model in Box 1, rewritten in
a modular form.

------------------------------------------------------------------------

## Dynamical Terms
