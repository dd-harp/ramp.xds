# **`ramp.xds`** - e**X**tensible **D**ynamical **S**ystems 
## *Nimble model building for malaria and other mosquito-borne pathogens*

<!-- badges: start -->
[![PLoS Computational Biology](https://img.shields.io/badge/doi%3A10.1371%2Fjournal.pcbi.1010684-cornflowerblue?style=for-the-badge&logoColor=cornflowerblue&label=PLoS%20Comp%20Bio&labelColor=slategrey&link=https%3A%2F%2Fjournals.plos.org%2Fploscompbiol%2Farticle%3Fid%3D10.1371%2Fjournal.pcbi.1010684)](https://doi.org/10.1371/journal.pcbi.1010684)

<!-- badges: end -->

## Install **`ramp.xds`** 

To install the latest version from GitHub, run the following lines of code in an R session.

```
library(devtools)
devtools::install_github("dd-harp/ramp.xds")
```

Also see the related vignette [*Getting Started*](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html).

## What is RAMP?

**`ramp.xds`** was developed to support RAMP. 

RAMP -- **R**obust **A**nalytics for **M**alaria **P**olicy -- describes bespoke inferential systems for malaria decision support and adaptive malaria control that go to great lengths to characterize, quantify, and propagate uncertainty. RAMP includes conventional analysis and simulation-based analytics.

## What is **`ramp.xds`**?

**`ramp.xds`** is an R software package that supports [nimble model building](https://dd-harp.github.io/ramp.xds/articles/Nimble.html) for simulation-based analytics and malaria research. The software was designed to help research scientists and policy analysts set up, analyze, solve, and use dynamical systems models describing the epidemiology, spatial transmission dynamics, and control of malaria and other mosquito-transmitted pathogens. The software also supports nimble model building and analysis for mosquito ecology, with the capability to handle forcing by weather and other exogenous variables. 

**`ramp.xds`** has been designed to serve the needs of malaria programs, where mathematical models are used for decision support and adaptive malaria control in a defined geographical area. Mechanistic models that have been fitted to data describing malaria in a place provide a synthesis of *malaria intelligence.* These models can facilitate complex analysis, extending our innate mental capabilities. By characterizing and quantifying uncertainty, and then propagating the uncertainty through the analysis, simulation-based analytics serve as a platform for giving robust policy advice and for adaptive malaria control. As the needs of a malaria program changes, the models can be modified -- simplified or extended -- to serve the tasks at hand.

**`ramp.xds`** was developed to apply malaria theory: it outputs standard, observable malaria metrics, and it easily solve dynamical systems, computes steady states and stable orbits, and computes the spatio-temporal vectorial capacity and the basic and adjusted reproductive numbers for malaria parasites. 

## A Software Ecosystem for Robust, Simulation-based Analytics  

**`ramp.xds`** is part of a suite of R packages developed to support RAMP: 

+ **`ramp.xds`** is the core computational engine for simulation-based analytics. It includes a basic set of models -- enough to design, verify, and demonstrate the basic features of modular software. 

+  [**`ramp.library`**](https://dd-harp.github.io/ramp.library/) is an extended library of stable code that has been tested and verified. It includes a large set of model families published in peer review that are not included in **`ramp.xds`** The ability to reuse code reduces the costs of replicating studies. Through this library, **`ramp.xds`** also supports nimble model building and analytics for other mosquito-borne pathogens. 
- [**`ramp.control`**](https://dd-harp.github.io/ramp.control/) is a collection of disease control models for **`ramp.xds`** 

+ [**`ramp.work`**](https://dd-harp.github.io/ramp.work/) includes algorithms to apply the framework, include code to fit models to data and to do constrained optimization 

+ [**`ramp.models`**](https://dd-harp.github.io/ramp.models/) includes a large set of models illustrating capabilities of **`ramp.xds`** 

**`ramp.xds`**  is under active development. It supersedes two other software packages, no longer under active development: [**`exDE`**](https://dd-harp.github.io/exDE/) and [**`MicroMoB`**](https://dd-harp.github.io/MicroMoB/). The history of development of RAMP simulation software has been memorialized in a [vignette](https://dd-harp.github.io/ramp.xds/articles/History.html).

## Capabilities

**`ramp.xds`** implements a rigorous mathematical framework for modular spatial dynamics, described in a recent publication:

> [**Spatial Dynamics of Malaria Transmission,** PLoS Computational Biology (2023).](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010684)^[Wu SL, Henry JM, Citron DT, Mbabazi Ssebuliba D, Nakakawa Nsumba J, Sánchez C. HM, et al. (2023) Spatial dynamics of malaria transmission. PLoS Comput Biol 19(6): e1010684. https://doi.org/10.1371/journal.pcbi.1010684] 

The computational framework has now been extended to cover *exogenous forcing* by weather and vector control. 

**`ramp.xds`** includes functions that make it easy to build, solve, and modify dynamical systems models for malaria & other mosquito-borne pathogens. A code library, in [**`ramp.library`**](https://dd-harp.github.io/ramp.library/), aims to become a comprehensive set of published models with code that has been verified and tested. It should be comparatively easy to replicate your study, or any study, using **`ramp.xds.`** If you're working on a new model family that is not already in **`ramp.library,`** you can write us and we'll help you [contribute](https://dd-harp.github.io/ramp.xds/articles/Contributing.html) to the growing collection. After your model has been added to the **`ramp.library`**, it will be much easier for others to replicate your study.

**`ramp.xds`** makes it easy to extend the capability of published models. Any model family from one dynamical component can be combined with any model from another dynamical component, and the rigid interface developed around blood feeding and egg laying make it possible to add structure and spatial dynamics. 

Some built in features that make **`ramp.xds`** easy to use (see [*Getting Started*](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html)): 

+ set up functions make it easy to build basic models;

+ functions make it easy to examine and change parameters and intial values;

+ after solving, the outputs are parsed so the variables can accessed by name;

+ the software automatically computes dynamical terms;  

+ most model families have built-in plotting functions to visualize basic outputs;

Additional capabilities of **`ramp.xds`** include support for: 

+ systems of ordinary and delay, differential and difference equations; 

+ autonomous and non-autonomous systems, including exogenous forcing by weather; 

+ mosquito spatial ecology and spatial transmission dynamics with algorithms to model mosquito dispersal and human mobility and travel;

+ structural flexibility for host populations, making it easy to stratify by epidemiological traits, such as heterogeneous exposure, vaccination, drug taking, net ownership, travel and mobility, and host demography;

+ realistic human / host demography with aging and cohort dynamics to model infection, immunity, disease, and infectiousness in host populations over by age and time; 

+ vector control;  

+ mass vaccination, mass drug administration, and other health interventions;  

+ heterogeneous biting and environmental heterogeneity in exposure;  

+ malaria connectivity, including importation through travel and visitors; 

+ multiple host species (or types) and multiple mosquito vector species (or types);

+ observational processes;

+ model fitting (see [**`ramp.work`**](dd-harp.github.io/ramp.work/)).
    
+ support for analysis, including computation of reproductive numbers 

## Nimble Model Building 

**`ramp.xds`** supports nimble model building through a mathematical framework and accompanying software that is modular, flexible, and extensible. Dynamical systems describing mosquitoes and malaria are naturally modular. The modular nature of these equations is emphasized by rewriting equations using [*modular forms*](modular_forms.html). Models include five core dynamical components in three chunks connected by two rigid interfaces:

***
<center>
![**Caligraphic Roman Letters Describe Major Components**](vignettes/notation1.png)
</center> 
***

Mosquito ecology and parasite infection dynamics are inextricably linked, as are the models describing human demography and parasite infection dynamics and immunity in human populations. 

Modularity was achieved through a combination of methods: the functions that compute derivatives (for differential equations) or that update state variables (for difference equations) use a generic method (using R's `S3` system) that dispatches on parameter lists for each component which is used to compute the full set of differential equations; the interfaces for blood feeding and egg laying are biologically realistic but rigidly defined to guarantee mathematical consistency; and a large set of additional features have been implemented with careful attention to detail to guarantee consistency and avoid conflicts. Each dynamical component also includes a null model, but where its outputs can be computed by any function. The function `ramp.xds::xde_derivatives` computes the derivatives of all state variables from those modular components and can be used with the excellent solvers in [deSolve](http://desolve.r-forge.r-project.org/). The functions `ramp.xds::dts_update` updates the state variables in discrete-time systems with support for mismatched run-time time steps. 

Beyond the modularity, the mathematical framework and software worked hard to leave most decisions up to the model builder. For example, the blood feeding and transmission interfaces compute the mean daily EIR by strata, but it is possible to model heterogeneous transmission. Heterogeneous biting can be modeled by assigning each stratum a frailty term. Environmental heterogeneity is handled through a function that allows the mean to have an arbitrary distribution, such as a gamma distribution, to model exposure as a negative binomial process. In some cases, this required developing new, generalized forms for old equations. 

Extensibility was a bigger challenge, especially with regard to exogenous forcing by weather and mosquito resource dynamics, vector control and health systems; and to human demographic processes, including aging and cohort dynamics. We believe **`ramp.xds`** is highly extensible, but we will find out as new models are developed that put this to the test. The full system looks something like this diagram:

***

<center>
![Modularity, as implemented in **`ramp.xds`**](vignettes/ModularGeneralized.png)
</center> 

***

To get started, we recommend the vignette [Getting Started](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html). A vignette that walks through `xds_setup` and sets up a simple system with 5 habitats, 3 patches, and 4 population strata (see the [5-3-4 Model Vignette](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)). Please browse the "Articles" or explore the well-documented "Functions."

## Contributing

**ramp.xds** aims to provide stable, reliable, reusable code for the study of mosquito-borne pathogen dynamics and control using dynamical systems. For information about how to contribute to the development of ramp.xds, please read our article on [Contributing](https://dd-harp.github.io/ramp.xds/articles/Contributing.html). 

If you have any questions, comments, bug reports, or suggestions, the first point of contact with the development team is through [GitHub Issues](https://github.com/dd-harp/ramp.xds/issues). If you are specifically submitting a bug report, please check out our [bug reporting guide](https://dd-harp.github.io/ramp.xds/articles/Contributing.html#sec8). If you are interested in collaborating in extensive model development (e.g. new mosquito model), please do not hesitate to contact the authors, whose email addresses can be found in the `DESCRIPTION` file.

We welcome issues and encourage contribution regardless of experience; the length of the contributing guide is not intended to be intimidating, merely complete. It is the responsibility of the package maintainers to help new contributors understand our conventions and guide contributions to a successful conclusion.

## Acknowledgements

+ This project has been supported by a grant from the Bill and Melinda Gates Foundation, **Modeling for Adaptive Malaria Control** (INV 030600, PI = David L Smith, University of Washington).

+ Adaptive Vector Control is funded by grant **Spatial Targeting and Adaptive Vector Control for Residual Transmission and Malaria Elimination in Urban African Settings** (R01 AI163398, PI = David L Smith), from US National Institute of Allergies and Infectious Diseases (NIAID).

+ Development of the software was supported through a collaboration with the [**Bioko Island Malaria Elimination Program**](https://www.mcd.org/focus-areas/featured-projects/bioko-island-malaria-elimination-project) 

+ Development of the software was supported through a collaboration with Uganda's **National Malaria Control Division** and **Department of Health Information** in the [Uganda Ministry of Health](https://www.health.go.ug/)

+ Development of this software benefited from funding and collaboration with the NIAID grant **Program for Resistance, Immunology, Surveillance & Modeling of Malaria in Uganda** (PRISM) (2U19AI089674, PIs = Grant Dorsey, University of California San Francisco; and Moses Kamya, Infectious Diseases Research Collaboration), which was part of the **International Centers of Excellence in Malaria Research** (ICEMR) program.

+ Funding to develop models of West Nile Virus to support Harris County Public Health was funded by the NSF as part of a project, Computing the Biome (PI = Janos Sztipanovits). The project was part of the Convergence Accelerator program of the National Science Foundation, Directorate for Technology, Innovation, and Partnerships (TIP) ([NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2040688) and [NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2134862), PI=Janos Sztipanovits, Vanderbilt University).

+ We acknowledge the important formative role played by the mosquito working groups of **RAPIDD** (Research and Policy for Infectious Disease Dynamics), which was sponsored by the Fogarty International Center, NIH, and the Department of Homeland Security. The mosquito working groups were led by Professor Thomas Scott. RAPIDD was led by F. Ellis McKenzie. 




