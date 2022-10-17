# Mapping female sex worker prevalence (aged 15-49 years) in sub-Saharan Africa

This repository contains most of the files to model female sex worker (FSW) size (aged 15-49 years) in sub-Saharan Africa. The project is divided into four folders with brief summaries of their main purposes:
* Data Preparation 01
  * Geolocates entries to nest cities within districts with countries
  * Converts population size estimates to prevalences
* Covariates 02
  * Processes DHS covariates and other auxiliary covariates
  * Performs leave-one-out cross-validation and imputation study for DHS covariates
  * Nightlight handling can be found at [David Chen's Github](https://github.com/TheDavidChen/NL_Africa)
* Modeling 03
  * Prepares the data for fitting
  * Performs leave-one-out cross-validation for model selection
  * Fits the FSW prevalence models and predicts prevalence to all sub-Saharan African countries
* Plotting 04
  * Plots results from the fitted model
  * Plots results from gender subdivision across countries
  * Trims plots for publication

