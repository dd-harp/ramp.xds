#' @title XH modules
#' 
#' @description
#' A List of Modules for the **XH**-Component 
#' 
#' @section In **`ramp.xds`**: 
#'  
#' + [`trivialXH`] - the trivial module
#' 
#' + [`SIS`] --- *ode* or *dts* --- a generalized *S-I-S* compartmental model 
#' 
#' + [`hMoI`] --- *ode* --- a hybrid model for infection and detection
#'
#' @section In **`ramp.library`**: 
#'  
#' + **Malaria** 
#' 
#'      + `garki` - an implementation of the Garki Model for malaria
#'      
#'      + `SIP`   - a compartmental model with treatment and chemoprotection 
#'      
#'      + `SIPd`  - an implementation of the Garki Model for malaria
#'
#' + **Compartmental Models** 
#' 
#'      + `SIR` - an **SIR** compartmental model 
#'      + `SIRS` - an **SIRS** compartmental model 
#'      + `SEIR` - an **SEIR** compartmental model 
#'      + `SEIRV` - an **SEIRV** compartmental model 
#'      + `SEIS` - an **SEIS** compartmental model 
#'      + `SEISd` - an **SEIS** compartmental model, as a delay differential equation
#' 
#' 
#'  **`ramp.library`** --- [source code](https://github.com/dd-harp/ramp.library) & [github.io](https://dd-harp.github.io/ramp.library)
#' 
#' @name XH_module_list 
NULL
