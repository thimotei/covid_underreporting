# ------- script to make all three figures ------- #

# ------- setting working directory -------#
here::here() %>% setwd()

library(patchwork)
source("covid_underreporting/R/data_helper_functions.R")
source("covid_underreporting/R/plotting_helper_functions.R")
source("covid_underreporting/R/plotting_functions.R")

# get all test data, case data and underreporting estimates in single tibble=

allUnderReportingAndTestingData <- getUnderReportingCaseDeathPopulationAndTestingData()
allAdjustedCaseData <- getAdjustedCaseDataNational()


#----------- making temporal variation under-reporting and testing effort figure -------------#

# calling the plotting function to make figure 1 style plots for every country in one large .pdf

countries_of_interest <- c("Austria", "Bangladesh",  "Belgium", 
                           "Denmark", "Italy", "South Korea",
                           "Switzerland", "United Kingdom", "USA") %>% sort()


#png("covid_underreporting/figures/figure_1.png", width = 10, height = 8, units = "in", res = 100)
plotData <- allUnderReportingAndTestingData %>%
  dplyr::mutate(country = dplyr::case_when(country == "Korea (the Republic of)" ~ "South Korea",
                                           country == "United States of America (the)" ~ "USA",
                                           country != "Korea (the Republic of)" | country != "United States of America (the)" ~ country)) %>% 
  dplyr::filter(country %in% countries_of_interest) %>%
  dplyr::mutate(testing_effort = zoo::rollmean(new_tests/new_cases, k = 7, fill = NA)) %>%
  dplyr::mutate(testing_effort = dplyr::na_if(testing_effort, "Inf")) %>%
  dplyr::select(-new_tests.x, -new_tests.y) %>%
  tidyr::drop_na()
fontsize1 <- 1
par(mar = c(3, 3, 3, 3))
par(oma = c(2, 2, 2, 2))
par(mfrow = c(3, 3))
for(countryArg in countries_of_interest)
{
  tryCatch(
    {
      figure1Fun(plotData, countryArg, 0, 0)
    },
  error=function(e){})
  #allPlots[[countryArg]] = p
}
mtext("Date",
      side = 1,
      col = "black",
      line = 0,
      outer = TRUE,
      cex = fontsize1)
mtext("Moving average of new tests per new cases",
      side = 2,
      line = 0,
      outer = TRUE,
      cex = fontsize1)
mtext("Percentage of symptomatic cases reported",
      side = 4,
      col = "black",
      line = 0,
      outer = TRUE,
      cex = fontsize1)
dev.off()

#----------- making serology figure -------------#

figure2 <- figure2Fun()
ggplot2::ggsave(
  "covid_underreporting/figures/figure_2.png",
  plot = figure2, 
  width = 10,
  height = 5,
  units = "in")

#----------- making adjusted case curves figure -------------#

options(scipen = 999)
figure3 <- figure3Fun()

ggplot2::ggsave("covid_underreporting/figures/figure_3.png",
                plot = figure3, 
                width = 10,
                height = 5,
                units = "in")


#------------ making a map of the prevalence of COVID -----------------#

figure4 <- figure4Fun()

ggplot2::ggsave("covid_underreporting/figures/figure_4.png",
                plot = figure4, 
                width = 11,
                height = 5,
                units = "in")

