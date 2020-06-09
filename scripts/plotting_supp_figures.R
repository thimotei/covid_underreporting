# ------- setting working directory -------#
here::here() %>% setwd()

# plotting supplementary figures


# get all test data, case data and underreporting estimates in single tibble=
source("covid_underreporting/R/get_underreporting_and_testing_data.R")
allDataTogether <- getUnderReportingAndTestingData()

# sourcing the functions to make the plots
source("covid_underreporting/R/plotting_functions.R")

#----------- 

png("covid_underreporting/figures/figure_1_all_countries_4.png", width = 1000, height = 1000, res = 100) 
#par(mar = c(3, 3, 3, 3))
#par(oma = c(2, 2, 2, 2))
par(mfrow = c(5, 3))
for(countryArg in allCountries[46:60])
{
  tryCatch(
    {
      figure1Supp(allDataTogether, countryArg, 0, 0)
    },
    
    error=function(e){})
}
dev.off()


#----------- making scatterplot figure -------------#
source("covid_underreporting/R/plotting_functions.R")
figureSuppFun()

ggplot2::ggsave("covid_underreporting/figures/figure_3_selected_countries.png",
                p3,
                #width = 10,
                #height = 30,
                limitsize = FALSE)
