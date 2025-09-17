---
header-includes:
- \usepackage{mathjaxr}
- \usepackage{euscript}
- \usepackage{mathscr}
--- 

# **`ramp.xds`** - e**X**tensible **D**ynamical **S**ystems 
## *Nimble model building and analytics for malaria and other mosquito-borne pathogens*


<!-- badges: start -->
[![PLoS Computational Biology](https://img.shields.io/badge/doi%3A10.1371%2Fjournal.pcbi.1010684-cornflowerblue?style=for-the-badge&logoColor=cornflowerblue&label=PLoS%20Comp%20Bio&labelColor=slategrey&link=https%3A%2F%2Fjournals.plos.org%2Fploscompbiol%2Farticle%3Fid%3D10.1371%2Fjournal.pcbi.1010684)](https://doi.org/10.1371/journal.pcbi.1010684)
<!-- badges: end -->

## Install **`ramp.xds`** 

To install the latest version from GitHub, run the following lines of code in an R session.

```
library(devtools)
devtools::install_github("dd-harp/ramp.xds")
```

To get started, see the vignette [*Getting Started*](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html), and the [**SimBA** Project](https://faculty.washington.edu/smitdave/simba/index.html) website.

***

## What is RAMP?


**`ramp.xds`** is an R software package that was developed to support **Sim**ulation-**B**ased **A**nalytics ([**SimBA**](https://faculty.washington.edu/smitdave/simba/index.html)) and **R**obust **A**nalytics for **M**alaria **P**olicy, or [**RAMP**](https://faculty.washington.edu/smitdave/ramp/index.html). 

**`ramp.xds`** supports nimble model building for simulation-based malaria analytics and research. 
The software was designed to lower the costs of setting up, solving, analyzing, and applying dynamical systems to model the epidemiology, spatial transmission dynamics, and control of malaria and other mosquito-transmitted pathogens. 
The software also supports nimble model building and analysis for mosquito ecology and parasite/pathogen transmission by mosquitoes as a *changing baseline* (using [**`ramp.forcing`**](https://dd-harp.github.io/ramp.forcing/) to model the effects of weather, hydrology, ...) that is *modified by vector control* (using [**`ramp.control`**](https://dd-harp.github.io/ramp.control/)).


**RAMP** is shorthand for the bespoke inferential systems developed for malaria decision support and adaptive malaria control that go to great lengths to characterize, quantify, and propagate uncertainty. 
RAMP systems combine elements of data science, conventional statistical analysis, and simulation-based analytics. 

The [**SimBA** project](https://faculty.washington.edu/smitdave/simba/index.html) (**Sim**ultation-**B**ased **A**nalytics) is a
software development project to support malaria analytics and development of robust policy advice for
malaria and other mosquito-transmitted pathogens. SimBA includes **`ramp.xds`** and all its satellite packages (below). 

**`ramp.xds`** has been designed to serve the needs of malaria programs, where mathematical models are used for decision support and adaptive malaria control in a defined geographical area. Mechanistic models that have been fitted to data describing malaria in a place provide a synthesis of *malaria intelligence.* These models can facilitate complex analysis, extending our innate mental capabilities. By characterizing and quantifying uncertainty, and then propagating the uncertainty through the analysis, simulation-based analytics serve as a platform for giving robust policy advice and for adaptive malaria control. As the needs of a malaria program changes, the models can be modified -- simplified or extended -- to serve the tasks at hand.

**`ramp.xds`** was developed to apply malaria theory: the software solves dynamical systems and outputs the predicted values of standard, observable malaria metrics. 
It also provides some computational support for qualitative analysis: it computes steady states, stable orbits and reproductive numbers. In [**`ramp.work`**](https://dd-harp.github.io/ramp.work/), we developed algorithms that fit models and evaluate vector control; that develope short term forecasts; and that enable scenario planning and strategic planning.

## **`ramp.xds`** and SimBA

Originally, **`ramp.xds`** was a single program, but it made sense to split the software into several R packages. 
When it split, we started using calling the software development project **SimBA**. 
In the narrow sense, **SimBA** software includes six distinct R packages: 

1. **`ramp.xds`** handles setup, solving, plotting, and some analysis. 
It was developed to build and solve dynamical systems models for the epidemiology, transmission dynamics, and control of malaria and other mosquito-transmitted pathogens based on a well-defined mathematical framework.
It includes a basic set of models -- enough to design, verify, and demonstrate the basic features of modular software. 

2. [**`ramp.library`**](https://dd-harp.github.io/ramp.library/) is an extended library of models -- stable code that has been tested and verified. It includes a large set of model families published in peer review that are not included in **`ramp.xds`** The ability to reuse code reduces the costs of replicating studies. Through this library, **`ramp.xds`** also supports nimble model building and analytics for other mosquito-borne pathogens. 

3. [**`ramp.control`**](https://dd-harp.github.io/ramp.control/) is a collection of disease control models for **`ramp.xds`** 

4. [**`ramp.forcing`**](https://dd-harp.github.io/ramp.forcing/) is a collection of utilities to model exogenous forcing in models for **`ramp.xds`** 

5. [**`ramp.demog`**](ramp.demog.html) is is a supplementary code library for **`ramp.xds`** that handles human demography and stratification, including vital dynamics and age structure.

6. [**`ramp.work`**](https://dd-harp.github.io/ramp.work/) includes algorithms to apply the framework, include code to fit models to data and to do constrained optimization 

**`ramp.xds`**  is under active development. It supersedes two other software packages, no longer under active development: [**`exDE`**](https://dd-harp.github.io/exDE/) and [**`MicroMoB`**](https://dd-harp.github.io/MicroMoB/). The history of development of RAMP simulation software has been memorialized in a [vignette](https://dd-harp.github.io/ramp.xds/articles/History.html).

The material in this website supports **`ramp.xds`** development and a basic introduction. 


## Contributing

**ramp.xds** aims to provide stable, reliable, reusable code for the study of mosquito-borne pathogen dynamics and control using dynamical systems. For information about how to contribute to the development of ramp.xds, please read our article on [Contributing](https://dd-harp.github.io/ramp.xds/articles/Contributing.html). 

If you have any questions, comments, bug reports, or suggestions, the first point of contact with the development team is through [GitHub Issues](https://github.com/dd-harp/ramp.xds/issues). If you are specifically submitting a bug report, please check out our [bug reporting guide](https://dd-harp.github.io/ramp.xds/articles/Contributing.html#sec8). If you are interested in collaborating in extensive model development (e.g. new mosquito model), please do not hesitate to contact the authors, whose email addresses can be found in the `DESCRIPTION` file.

We welcome issues and encourage contribution regardless of experience; the length of the contributing guide is not intended to be intimidating, merely complete. It is the responsibility of the package maintainers to help new contributors understand our conventions and guide contributions to a successful conclusion.

## Acknowledgements

+ This project was supported by a grant from the Bill and Melinda Gates Foundation, **Modeling for Adaptive Malaria Control** (INV 030600, PI = David L Smith, University of Washington).

+ Support for *Adaptive Vector Control* is funded by grant **Spatial Targeting and Adaptive Vector Control for Residual Transmission and Malaria Elimination in Urban African Settings** (R01 AI163398, PI = David L Smith), from US National Institute of Allergies and Infectious Diseases (NIAID).

+ Development of SimBA, RAMP and Adaptive Malaria Control was supported through collaboration with the [**Bioko Island Malaria Elimination Program**](https://www.mcd.org/focus-areas/featured-projects/bioko-island-malaria-elimination-project) and
Uganda's **National Malaria Control Division** and **Department of Health Information** in the [Uganda Ministry of Health](https://www.health.go.ug/)

+ Development of this software benefited from funding and collaboration with the NIAID grant **Program for Resistance, Immunology, Surveillance & Modeling of Malaria in Uganda** (PRISM) (2U19AI089674, PIs = Grant Dorsey, University of California San Francisco; and Moses Kamya, Infectious Diseases Research Collaboration), which was part of the **International Centers of Excellence in Malaria Research** (ICEMR) program.

+ Funding to develop models of West Nile Virus to support Harris County Public Health was funded by the NSF as part of a project, Computing the Biome (PI = Janos Sztipanovits). The project was part of the Convergence Accelerator program of the National Science Foundation, Directorate for Technology, Innovation, and Partnerships (TIP) ([NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2040688) and [NSF 2040688](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2134862), PI=Janos Sztipanovits, Vanderbilt University).

+ We also acknowledge the importance of the mosquito working groups of **RAPIDD** (Research and Policy for Infectious Disease Dynamics), which was sponsored by the Fogarty International Center, NIH, and the Department of Homeland Security. The mosquito working groups were led by Professor Thomas Scott. RAPIDD was directed by F. Ellis McKenzie. 



