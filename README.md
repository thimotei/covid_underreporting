# Reconstructing the global dynamics of unreported COVID-19 cases and infections
sing reported data on COVID-19 cases and fatalities globally, we estimated the proportion of symptomatic cases that were reported in 210 countries and territories. We then use these estimates to attempt to reconstruct the pandemic.

## Contents in the repository
The repo contains all the functions and scripts required to reproduce the results of (this paper)[https://cmmid.github.io/topics/covid19/Under-Reporting.html]. Specifically, the functions and scripts in this repo are for:
* downloading the required data from the ECDC [here](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)
* importing the estimates from the bayesian inference model which forms the basis for the paper, the code and estimates for which can be found [here](https://github.com/thimotei/CFR_calculation)
* combining the data and estimates to adjust the case curves
* producing all figures in the manuscript 

To download the code, clone this repository using the command

```sh
git clone https://github.com/thimotei/covid_underreporting
```

To reproduce all of the figures in the manuscript, run the script 
```r
scripts/main_script.R
```
in which all of the data required for the figures is pulled using the `R/data_helper_functions.R` and the plots are made using both the `R/plotting_functions.R` and `R/plotting_helper_functions.R`.
