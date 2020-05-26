# ------- script to make all three figures ------- #

# ------- setting working directory -------#
here::here() %>% setwd()

library(patchwork)
source("covid_underreporting/R/data_helper_functions.R")
source("covid_underreporting/R/plotting_helper_functions.R")
source("covid_underreporting/R/plotting_functions.R")

# get all test data, case data and underreporting estimates in single tibble=

allUnderReportingAndTestingData <- getUnderReportingAndTestingData()
allAdjustedCaseData <- getAdjustedCaseData()


#----------- making temporal variation under-reporting and testing effort figure -------------#

# calling the plotting function to make figure 1 style plots for every country in one large .pdf

allCountries <- allUnderReportingAndTestingData$country %>% unique()
countriesOfInterest <- c("Canada",  "Bangladesh", "Switzerland", 
                         "Belgium", "Italy",  "United Kingdom")

potentialCountriesOfInterest <- c("Austria", "Belgium", "Canada",
                                  "Denmark", "Hungary", "Indonesia",
                                  "Italy", "South Korea", "Turkey")


#pdf("covid_underreporting/figures/figure_1_all_countries.pdf") 

png("covid_underreporting/figures/figure_1_countries_of_interest.png", width = 1024, height = 768)
fontsize1 <- 1
par(mar = c(3, 3, 3, 3))
par(oma = c(2, 2, 2, 2))
par(mfrow = c(3, 3))
for(countryArg in potentialCountriesOfInterest)
{
  tryCatch(
    {
      figure1Fun(allUnderReportingAndTestingData, countryArg, 0, 0)
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

allNationalCumulativeIncidenceEstimates <- getNationalCumulativeIncidenceEstimates() %>%
  dplyr::select(date, country, cumulativeIncidenceMid, cumulativeIncidenceLow, cumulativeIncidenceHigh)


allRegionalCumulativeIncidenceEstimates <- getRegionalCumulativeIncidenceEstimates() %>%
  dplyr::mutate(country = dplyr::case_when(countryCode == "GEN" ~ "Switzerland",
                                           countryCode == "WUH" ~ "China",
                                           countryCode == "NYC" ~ "United States of America")) %>%
  dplyr::select(date, country, cumulativeIncidenceMid, cumulativeIncidenceLow, cumulativeIncidenceHigh)


observedEstimatesNational <- dplyr::tibble(country = c("Andorra" ,"Brazil", "Denmark", "Luxembourg", "Spain", "Sweden"),
                                   dateStart = as.Date(c("2020-05-04","2020-05-09", "2020-04-20", "2020-04-15", "2020-04-27", "2020-04-27")),
                                   dateEnd = as.Date(c("2020-05-13", "2020-05-11", "2020-04-27", "2020-05-05", "2020-05-04", "2020-05-03")),
                                   dateMid = as.Date(c("2020-05-09", "2020-05-10", "2020-04-24", "2020-04-25", "2020-05-01", "2020-04-30")),
                                   observedEstimateMid = c(0.0904, 0.0022, 0.011, 0.019, 0.055, 0.055),
                                   observedEstimateLow = c(0.0876, 0.00107, 0.00579, 0.0134, 0.0483, 0.073),
                                   observedEstimateHigh = c(0.0932, 0.00408,  0.01943589, 0.0266, 0.0518, 0.037))


observedEstimatesRegional <- dplyr::tibble(country = c("Switzerland", "China", "United States of America"),
                                           dateStart = as.Date(c("2020-04-21", "2020-04-03", "2020-04-20")),
                                           dateEnd = as.Date(c("2020-04-26", "2020-04-15 ", "2020-05-01")),
                                           dateMid = as.Date(c("2020-04-24", "2020-04-09", "2020-04-26")),
                                           observedEstimateMid  = c(0.097, 0.096, 0.123),
                                           observedEstimateLow  = c(0.0727, 0.0786, 0.118),
                                           observedEstimateHigh = c(0.122, 0.116, 0.128))


observedEstimatesRegionalTmp <- dplyr::tibble(country = c("Switzerland", "United States of America"),
                                           dateStart = as.Date(c("2020-04-21", "2020-04-20")),
                                           dateEnd = as.Date(c("2020-04-26", "2020-05-01")),
                                           dateMid = as.Date(c("2020-04-24", "2020-04-26")),
                                           observedEstimateMid  = c(0.097, 0.123),
                                           observedEstimateLow  = c(0.0727, 0.118),
                                           observedEstimateHigh = c(0.122, 0.128))


plotPrevalenceNational <- figure2Fun(allNationalCumulativeIncidenceEstimates, observedEstimatesNational)
plotPrevalenceRegional <- figure2Fun(allRegionalCumulativeIncidenceEstimates, observedEstimatesRegionalTmp)

ggplot2::ggsave(
  "covid_underreporting/figures/prevalenceEstimatesAllCountries.png",
  plot = plotPrevalence, 
  width = 20,
  height = 20,
  units = "in")

#----------- making adjusted case curves figure -------------#

options(scipen = 999)
allAdjustedCaseData %>% figure3Fun()

ggplot2::ggsave("covid_underreporting/figures/figure_3.png")


#------------ making a map of the prevalence of COVID -----------------#

wholeMapPlot <- figure4Fun()

ggplot2::ggsave("covid_underreporting/figures/prevalenceTwoPanels.png",
                wholeMapPlot, 
                width  = 500,
                height = 240,
                units = "mm")

