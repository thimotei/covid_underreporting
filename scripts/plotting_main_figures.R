# ------- script to make all three figures ------- #

# ------- setting working directory -------#
here::here() %>% setwd()

library(patchwork)
source("covid_underreporting/R/data_helper_functions.R")
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

allCumulativeIncidenceEstimates <- getCumulativeIncidenceEstimates()

# just for own reference
#potentialCountriesOfInterest <- c("Switzerland", "Iran", "China",
#                                  "Spain", "Luxembourg", "United States of America",
#                                  "Sweden", "Brazil", "Denmark")

observedEstimates <- dplyr::tibble(country = c("Brazil", "Denmark", "Luxembourg", "Spain", "Sweden", "Switzerland"),
                                   dateStart = as.Date(c("2020-05-09", "2020-04-20", "2020-04-15", "2020-04-27", "2020-04-27", "2020-04-21")),
                                   dateEnd = as.Date(c("2020-05-11", "2020-04-27", "2020-05-05", "2020-05-04", "2020-05-03", "2020-04-26")),
                                   dateMid = as.Date(c("2020-05-10", "2020-04-24", "2020-04-25", "2020-05-01", "2020-04-30", "2020-04-24")),
                                   observedEstimate = c(0.0022, 0.011, 0.019, 0.05, 0.073, 0.097))


figure2Fun(allCumulativeIncidenceEstimates, observedEstimates)

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

cumulativeIncidenceEstimatesWorldMap <- getIncidenceUpToDateData(dateInput = NULL)
cumulativeIncidenceEstimatesEuropeMap1 <- getIncidenceUpToDateData(dateInput = "2020-03-30")
cumulativeIncidenceEstimatesEuropeMap2 <- getIncidenceUpToDateData(dateInput = "2020-04-20")
cumulativeIncidenceEstimatesEuropeMap3 <- getIncidenceUpToDateData(dateInput = "2020-05-10")

europeanCountries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium",
                       "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus",
                       "Czech Republic", "Denmark", "Estonia", "Finland", "France",
                       "Germany", "Greece", "Hungary","Ireland", "Isle of Man",
                       "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Moldova", 
                       "Netherlands", "Norway", "Macedonia", "Poland", "Portugal", "Romania", 
                       "Russia", "San Marino", "Slovakia", "Slovenia", "Spain", "Sweden", 
                       "Switzerland", "Turkey", "Ukraine", "UK")



worldMapData <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesWorldMap)
europeMapData1 <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesEuropeMap1)
europeMapData2 <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesEuropeMap2)
europeMapData3 <- combineMapAndIncidenceData(cumulativeIncidenceEstimatesEuropeMap3)





worldMapPlot <- mapPlottingFunction(worldMapData, europe = FALSE)
europeMapPlot1 <- mapPlottingFunction(europeMapData1, europe = TRUE)
europeMapPlot2 <- mapPlottingFunction(europeMapData2, europe = TRUE)
europeMapPlot3 <- mapPlottingFunction(europeMapData3, europe = TRUE)




wholePlot <- worldMapPlot + (EuropeMapPlot1/EuropeMapPlot2/EuropeMapPlot3) +  plot_layout(widths = c(6, 1))


ggplot2::ggsave("covid_underreporting/figures/prevalenceTwoPanels.png",
                wholePlot, 
                width  = 500,
                height = 240,
                units = "mm")




