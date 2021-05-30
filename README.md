# Mapping female sex worker prevalence (aged 15-49 years) in sub-Saharan Africa

This repository contains most of the files to model female sex worker (FSW) size (aged 15-49 years) in sub-Saharan Africa. The project is divided into three folders with brief summaries of their main purposes:
* Data Preparation
  * Geolocates entries to nest cities within districts with countries
  * Converts population size estimates to prevalences
* Covariates
  * Processes DHS covariates, LandCover, and Facebook Social Connectedness
  * Nightlight handling can be found at [David Chen's Github](https://github.com/TheDavidChen/NL_Africa)
* Modelling
  * Fits the FSW prevalence models and predicts prevalence to all sub-Saharan African countries
  * Rstan files detailing the models


## Order of Operations

The starting point for this repo is a PSE spreadsheet with information extracted from the literature. Some entries have prevalence, but most do not. The order of operations from this PSE spreadsheet to final FSW prevalence estimates is the following:

1. Standardize prevalence
    * In the Excel sheet, create a new column called *Prevalence_std*. This column contains the information in the *Prevalence* column extracted from the literature, but converts it to an actual prevalence value. This is necessary because some prevalences are reported as percents or ranges.
2. Add census genpop, census years, and standardize area
    * In the Excel sheet, new columns for census populations and census years of the region are extracted from [citypopulation.de](https://www.citypopulation.de/). This is done by hand.
    * While extracting the census populations and years, the *area* is also standardized, converting the original area designations to one of *Sub-city*, *City*, *District*, or *National*.
3. Calculate FSW prevalence from FSW PSE
    * This is done in R using [WorldPop](https://www.worldpop.org/) data.
4. Map each entry to a GADM 2.8 district.
    * This is first done in R using Google `geocode()`
    * Validation of `geocode()` and missing values are done by hand
5. Process covariates
    * As mentioned earlier, Nightlight is processed by [David Chen's Github](https://github.com/TheDavidChen/NL_Africa)
6. Model the data


## Technologies

These files were run using `R` v3.6.3 and the following packages:

+ `sp` v1.4.4
+ `raster` v3.4.5
+ `rstan` v2.21.2
+ `dplyr` v1.0.2
+ `rdhs` v0.7.1
+ `tidyverse` v1.3.0 <!-- + `janitor` v2.0.1 -->
+ `labelled` 2.7.0
+ `stringr` 1.4.0

## Detailed file instructions
* Data Preparation
  * `Method_Standardization.R` converts the original methods extracted from the papers to a smaller number of methods. This is done using the ```case_match()``` function in the **dplyr** package.
  * `EDA_PSE.R` is purely for explaratory data analysis. This file does not alter the data in any lasting way and is not required for final analysis.

* Covariates
  * `automateDHS_household_general.Rmd` processes the DHS covariates. The procedure is to download the datasets for all available countries, map each cluster to an administrative one unit in the shapefile, and summarize the data for each administrative one unit. Numerical columns are summarized using mean, median, and standard deviation while categorical columns are summarized using percent of each response.
  * `PSE_Covariate_Matching.R` processes the rest remaining covariates. It currently calculates the prevalence, but this will be moved to a script in the Data Preparation folder. After calculating the prevalence, it reads in the shapefiles with the nightlight values already stored, processes and adds the LandCover covariate, adds the social connectedness, where available (**TODO**: Add social connectedness correlations as well), and adds the female child-bearing age population and ratio to the general population.
  * **TODO**: `Admin_matching.R` matches the city within a district within a country using geocode. After these values are added using geocode, a new spreadsheet is created to correct and add additional matchings.

* Modeling
  * `District_PSE_stan.stan` contains the stan model for fitting the district-informed-random-effect model.
  * `SSA_Modeling.R` fits the model, predicts FSW prevalence to all sub-Saharan Africa administrative one units, and visualizes the estimates.

