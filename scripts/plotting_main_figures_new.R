here::here() %>% setwd()

source("covid_underreporting/R/data_helper_functions.R")
source("covid_underreporting/R/plotting_functions.R")
source("covid_underreporting/R/plotting_helper_functions.R")

#----------- making temporal variation under-reporting and testing effort figure -------------#

# calling the plotting function to make figure 1 style plots for every country in one large .pdf

allUnderReportingAndTestingData <- getUnderReportingCaseDeathPopulationAndTestingData()
allAdjustedCaseData <- getAdjustedCaseData()

countriesOfInterestTesting <- c("Austria", "South Korea", "Switzerland", 
                         "Belgium", "Italy", "Sweden")

countriesOfInterestNoTesting <- c("Brazil", "United Kingdom", "United States of America")

#pdf("covid_underreporting/figures/figure_1_all_countries.pdf") 

#png("covid_underreporting/figures/figure_1_countries_of_interest.png", width = 1024, height = 768)
plotDataTesting <- allUnderReportingAndTestingData %>%
  dplyr::mutate(country = dplyr::case_when(country == "Korea (the Republic of)" ~ "South Korea",
                                           country != "Korea (the Republic of)" ~ country)) %>%
  dplyr::filter(country %in% countriesOfInterestTesting) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date >= "2020-03-10") %>%
  dplyr::mutate(estimate = estimate*100,
                lower    = lower*100,
                upper    = upper*100)

plotDataNoTesting <- allAdjustedCaseData %>%
  dplyr::ungroup(country) %>%
  dplyr::filter(country %in% countriesOfInterestNoTesting) %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date >= "2020-03-10")  %>%
  dplyr::mutate(estimate = estimate*100,
                lower    = lower*100,
                upper    = upper*100)

fontsize1 <- 1
par(mar = c(3, 3, 3, 3))
par(oma = c(2, 2, 2, 2))
par(mfrow = c(3, 3))
for(countryArgTest in countriesOfInterestTesting)
{
  tryCatch(
    {
      figure1Fun(plotDataTesting, countryArgTest, 0, 0)
    },
    error=function(e){})
}
for(countryArgNoTest in countriesOfInterestNoTesting)
{
  tryCatch(
    {
      figure1FunNoTesting(plotDataNoTesting, countryArgNoTest, 0, 0)
    },
    error=function(e){})
}
mtext("Date",
      side = 1,
      col = "black",
      line = 0,
      outer = TRUE,
      cex = fontsize1)
mtext("Testing effort (seven day moving average of tests per thousand cases)",
      side = 2,
      line = 0,
      outer = TRUE,
      cex = fontsize1)
mtext("Estimated symptomatic cases (%)",
      side = 4,
      col = "black",
      line = 0,
      outer = TRUE,
      cex = fontsize1)
dev.off()


#----------- making seroprevalence comparison figure -------------#

tmp <- readr::read_csv("~/Downloads/full-list-covid-19-tests-per-day.csv") %>%
  dplyr::mutate(Date = lubridate::mdy(Date)) %>%
  dplyr::rename(country = Entity, iso_code = Code, date = Date, new_tests = `Daily change in cumulative total tests`) %>%
  dplyr::left_join(allUnderReportingAndTestingData, by = c("date", "iso_code"))

#----------- making adjusted case curves figure -------------#


figure3 <- figure3Fun()


#----------- making map of cumulative incidence -------------#

figure4 <- figure4Fun()

figure4
