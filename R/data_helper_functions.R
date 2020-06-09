#--- simple function which gets all 2 and 3 letter country codes
#--- from a .csv file saved on my laptop, taken from online
getCountryCodes <- function()
{
  
  httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
  allDatRaw <- readr::read_csv(tf) %>%
    dplyr::rename(date = dateRep, 
                  country = countriesAndTerritories,
                  countryCode = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::dmy(date))
  
  countryCodesLookUp <- allDatRaw %>%
    dplyr::select(country, 
                  countryCode) %>% 
    unique()
  
  return(countryCodesLookUp)
  
}

#--- pulls our national under-reporting estimates from the shared Dropbox 
#--- pulls testing data from the Our Wold In Data website
#--- munges data together
# getUnderReportingAndTestingData <- function()
# {
#   
#   #---------- reading in the ECDC data ----------#
#   
#   httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
#   allDatRaw <- readr::read_csv(tf) %>%
#     dplyr::rename(date = dateRep, 
#                   country = countriesAndTerritories,
#                   countryCode = countryterritoryCode) %>%
#     dplyr::mutate(date = lubridate::dmy(date))
#   
#   
#   countryCodesLookUp <- allDatRaw %>%
#     dplyr::select(country,
#                   countryCode) %>%
#     unique()
#   
#   # DEPRACATED
#   # dateRange <- allDatRaw %>%
#   #   dplyr::group_by(countryCode) %>%
#   #   dplyr::summarise(minDate = min(date),
#   #                    maxDate = max(date))
#   
#   #---------- reading in the under-reporting estimates from the shared dropbox ----------#
#   
#   data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted/"
#   files <- dir(path = data_path,
#                pattern = "*.rds")
#   
#   data <- dplyr::tibble(countryCode = files) %>% 
#     dplyr::mutate(file_contents = purrr::map(countryCode, 
#                                              ~ readRDS(file.path(data_path, .)))
#                   
#     ) %>% 
#     tidyr::unnest(cols = c(file_contents)) %>%
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
#     #dplyr::left_join(dateRange) %>%
#     dplyr::group_by(countryCode) %>%
#     #dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>% # DEPRACATED
#     dplyr::select(date, everything()) %>%
#     dplyr::left_join(countryCodesLookUp) %>%
#     dplyr::select(date, country, countryCode, everything()) %>%
#     dplyr::group_by(countryCode) %>%
#     dplyr::ungroup()
#   
#   #------ combining the case & death data from ECDC with our under-reporting estimates
#   #------ into a single tibble 
#   
#   dataCases <- data %>% 
#     dplyr::left_join(allDatRaw, by = c("date", "country", "countryCode"), copy = FALSE) %>%
#     dplyr::select(date, country, countryCode, cases, deaths) %>%
#     tidyr::drop_na()
#   
#   dataCasesAndEstimates <- dataCases %>%
#     dplyr::right_join(data, by = c("country", "date", "countryCode"), copy = FALSE) %>%
#     tidyr::drop_na() %>% 
#     dplyr::mutate(country = stringr::str_replace_all(country, "_", " "))
#   
#   #---------- reading in the OWID data and munging it together with the other data frame ----------#
#   owidData <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")
#   
#   
#   owidData <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
#     dplyr::select(date, 
#                   countryCode = iso_code,
#                   country = location,
#                   newTests = new_tests,
#                   newCases = new_cases) %>% 
#     tidyr::drop_na() %>%
#     dplyr::mutate(country = dplyr::case_when(country == "United States" ~ "United States of America",
#                                              country != "United States" ~ country))
#   
#   allDataTogether <- owidData %>%
#     dplyr::right_join(dataCasesAndEstimates, by = c("date", "country")) %>%
#     tidyr::drop_na() %>%
#     dplyr::select(date, country, cases, deaths, newTests, estimate, lower, upper) %>%
#     dplyr::arrange(country, date) %>%
#     unique() %>% 
#     dplyr::mutate(testsPerCase = newTests/cases) %>% 
#     dplyr::group_by(country) %>%
#     dplyr::select(date, upper, lower, estimate, testsPerCase) %>%
#     dplyr::na_if(Inf) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(upper = upper*100, lower = lower*100, estimate = estimate*100,
#                   testsPerCaseMA = imputeTS::na_ma(testsPerCase, k = 4, weighting = "linear")) %>%
#     dplyr::mutate(testsPerCaseMA = forecast::ma(testsPerCaseMA, order = 4, centre = TRUE)) %>%
#     dplyr::filter(country != "Argentina")
#   
# }


# up to date version of the function
getUnderReportingCaseDeathPopulationAndTestingData <- function()
{
  
  owidData <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
  
  underReportingPath <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted/"
  files <- dir(path = underReportingPath,
               pattern = "*.rds")
  
  underReportingRawData <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(underReportingPath, .)))) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  underReportingAndTestingData <- owidData %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, "iso3c", destination = 'iso.name.en')) %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths, total_cases, total_deaths, new_tests_smoothed_per_thousand, population, estimate, lower, upper) %>%
    tidyr::drop_na()
  
}


#--- pulls the confirmed case and death time-series data from ECDC then
#--- adjusts the curves based on our under-reporting estimates and
#--- the best available estimates for the proportion of asymptomatic infections
# getAdjustedCaseData <- function()
# {
#   
#   httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
#   allDatRaw <- readr::read_csv(tf) %>%
#     dplyr::rename(date = dateRep, 
#                   country = countriesAndTerritories,
#                   countryCode = countryterritoryCode) %>%
#     dplyr::mutate(date = lubridate::dmy(date))
#   
#   countryCodesLookUp <- allDatRaw %>%
#     dplyr::select(country, 
#                   countryCode) %>% 
#     unique()
#   
#   deathSummaryData <- allDatRaw %>%
#     dplyr::group_by(country) %>%
#     dplyr::summarise(totalDeaths = sum(deaths)) %>% 
#     dplyr::mutate(country_order = rank(totalDeaths)) %>%
#     dplyr::arrange(desc(country_order))
#   
#   caseSummaryData <- allDatRaw %>%
#     dplyr::group_by(country) %>%
#     dplyr::summarise(totalCases = sum(cases))
#   
#   # DEPRACATED
#   # dateRange <- allDatRaw %>%
#   #   dplyr::group_by(countryCode) %>%
#   #   dplyr::summarise(minDate = min(date),
#   #                    maxDate = max(date)) 
#   
#   
#   data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted/"
#   files <- dir(path = data_path,
#                pattern = "*.rds")
#   
#   dataTmp <- dplyr::tibble(countryCode = files) %>% 
#     dplyr::mutate(file_contents = purrr::map(countryCode, 
#                                              ~ readRDS(file.path(data_path, .)))
#                   
#     ) %>% 
#     tidyr::unnest(cols = c(file_contents)) %>%
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
#     dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
#     # dplyr::left_join(dateRange) %>% DEPRACATED
#     dplyr::group_by(countryCode) %>%
#     # dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>% # DEPRACATED
#     dplyr::select(date, everything()) %>%
#     dplyr::left_join(countryCodesLookUp) %>%
#     dplyr::select(date, country, countryCode, everything()) %>%
#     dplyr::group_by(countryCode) %>%
#     dplyr::ungroup()
#   
#   underReportingData <- dataTmp %>% 
#     dplyr::left_join(deathSummaryData, by = c('country' = 'country')) %>% 
#     dplyr::left_join(caseSummaryData, by = c('country' = 'country')) %>% 
#     dplyr::arrange(desc(country_order), date) 
#   
#   
#   dataTrueCases <- allDatRaw %>%
#     dplyr::mutate(date = as.Date(date)) %>%
#     dplyr::left_join(underReportingData) %>%
#     dplyr::select(date, country, cases, deaths, estimate, lower, upper) %>%
#     dplyr::group_by(country) %>%
#     dplyr::arrange(date, .by_group = TRUE) %>%
#     tidyr::drop_na() %>%
#     dplyr::mutate(trueCasesMid  = smooth::sma(cases/estimate)$fitted,
#                   trueCasesLow  = smooth::sma(cases/upper)$fitted,
#                   trueCasesHigh = smooth::sma(cases/lower)$fitted) %>%
#     dplyr::ungroup(country) %>%
#     dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
#     dplyr::filter(!(country == "China" & date > "2020-03-15")) %>%
#     dplyr::filter(dplyr::n() > 20) 
#   
#   return(dataTrueCases)
#   
# }


# new version of the function, much more streamlined
getAdjustedCaseData <- function()
{
  
  asymptomatic_mid <- 0.5
  asymptomatic_low <- 0.1
  asymptomatic_high <- 0.7
  
  
  ecdcCaseData <- NCoVUtils::get_ecdc_cases() %>% 
    dplyr::rename(new_cases = cases,
                  new_deaths = deaths, 
                  iso_code = countryterritoryCode) %>%
    dplyr::filter(new_cases >= 0)
  
  underReportingPath <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted/"
  files <- dir(path = underReportingPath,
               pattern = "*.rds")
  
  underReportingRawData <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(underReportingPath, .)))) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::select(date, everything()) %>%
    dplyr::select(date, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::rename(iso_code = countryCode)
  
  
  underReportingAndCaseData <- ecdcCaseData %>% 
    dplyr::left_join(underReportingRawData) %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(country, date) %>%
    tidyr::drop_na() %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths, population_2018, estimate, lower, upper)
  
  
  dataOut <- underReportingAndCaseData %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_cases_smoothed             = zoo::rollmean(new_cases, k = 7, fill = NA),
                  new_cases_adjusted_mid         = new_cases/estimate,
                  new_cases_adjusted_low         = new_cases/upper,
                  new_cases_adjusted_high        = new_cases/lower,
                  new_cases_adjusted_smooth_mid  = zoo::rollmean(new_cases_adjusted_mid, k = 7, fill = NA),
                  new_cases_adjusted_smooth_low  = zoo::rollmean(new_cases_adjusted_low, k = 7, fill = NA),
                  new_cases_adjusted_smooth_high = zoo::rollmean(new_cases_adjusted_high, k = 7, fill = NA)) %>%
    dplyr::mutate(cumulative_incidence_mid  = cumsum(new_cases_adjusted_mid)/(population_2018*(1 - asymptomatic_mid)),
                  cumulative_incidence_low  = cumsum(new_cases_adjusted_low)/(population_2018*(1 - asymptomatic_low)),
                  cumulative_incidence_high = cumsum(new_cases_adjusted_high)/(population_2018*(1 - asymptomatic_high))) %>%
    dplyr::mutate(cumulative_incidence_mid = dplyr::case_when(cumulative_incidence_mid >= 1 ~ 1,
                                                              cumulative_incidence_mid <= 0 ~ 0,
                                                              cumulative_incidence_mid > 0 & cumulative_incidence_mid < 1 ~ cumulative_incidence_mid)) %>%
    dplyr::mutate(cumulative_incidence_low = dplyr::case_when(cumulative_incidence_low > 1 ~ 1,
                                                              cumulative_incidence_low < 0 ~ 0,
                                                              cumulative_incidence_low > 0 & cumulative_incidence_low < 1 ~ cumulative_incidence_low)) %>%
    dplyr::mutate(cumulative_incidence_high = dplyr::case_when(cumulative_incidence_high > 1 ~ 1,
                                                               cumulative_incidence_high < 0 ~ 0,
                                                               cumulative_incidence_high > 0 & cumulative_incidence_high < 1 ~ cumulative_incidence_high)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "USA",
                                             country != "United States of America" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                             country != "United Kingdom" ~ country))
  
  
  return(dataOut)
  
}

#--- simple function to combine the map data (i.e. coordinates of countries) and our estimates
combineMapAndIncidenceData <- function(dataToPlot)
{
  
  ggplot2::map_data("world") %>%
    dplyr::rename(country = region) %>%
    dplyr::filter(country != "Antarctica") %>%
    dplyr::left_join(dataToPlot, by = "country") %>%
    dplyr::select(country, lat, long, group, country, cumulative_incidence_mid)
  
}

#--- get estimates of cumulative incidence over time for every country we have estimates for
getNationalCumulativeIncidenceEstimates <- function()
{
  here::here() %>% setwd()
  
  allUnderReportingAndTestingData <- getUnderReportingAndTestingData()
  allAdjustedCaseData <- getAdjustedCaseData()
  
  worldPopulationEstimatesRaw <- readr::read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")
  
  worldPopulationEstimatesClean <- worldPopulationEstimatesRaw %>%
    dplyr::filter(Variant == "Medium" & Time == "2020") %>% 
    dplyr::select(country = Location, population = PopTotal) %>%
    dplyr::mutate(population = population*1000) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                             country != "Bolivia (Plurinational State of)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Iran (Islamic Republic of)" ~ "Iran",
                                             country != "Iran (Islamic Republic of)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Republic of Moldova" ~ "Moldova",
                                             country != "Republic of Moldova" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Russian Federation" ~ "Russia",
                                             country != "Russian Federation" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
                                             country != "Sint Maarten (Dutch part)" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Republic of Korea" ~ "South Korea",
                                             country != "Republic of Korea" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                             country != "Venezuela (Bolivarian Republic of)" ~ country)) %>%
    rbind(c("Kosovo", 1810366))
  
  
  asymptomaticPropMid <- 0.5
  asymptomaticPropLow <- 0.23
  asymptomaticPropHigh <- 0.7
  
  
  allIncidenceEstimates <- allAdjustedCaseData %>% 
    dplyr::left_join(worldPopulationEstimatesClean) %>% 
    dplyr::group_by(country) %>%
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::mutate(cumulativeIncidenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativeIncidenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativeIncidenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh))) %>%
    dplyr::mutate(cumulativeIncidenceMid = dplyr::case_when(cumulativeIncidenceMid >= 1 ~ 1,
                                                             cumulativeIncidenceMid <= 0 ~ 0,
                                                             cumulativeIncidenceMid > 0 & cumulativeIncidenceMid < 1 ~ cumulativeIncidenceMid)) %>%
    dplyr::mutate(cumulativeIncidenceLow = dplyr::case_when(cumulativeIncidenceLow > 1 ~ 1,
                                                             cumulativeIncidenceLow < 0 ~ 0,
                                                             cumulativeIncidenceLow > 0 & cumulativeIncidenceLow < 1 ~ cumulativeIncidenceLow)) %>%
    dplyr::mutate(cumulativeIncidenceHigh = dplyr::case_when(cumulativeIncidenceHigh > 1 ~ 1,
                                                             cumulativeIncidenceHigh < 0 ~ 0,
                                                             cumulativeIncidenceHigh > 0 & cumulativeIncidenceHigh < 1 ~ cumulativeIncidenceHigh)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()
  
  return(allIncidenceEstimates)
  
}


#--- pulls regional case and death time-series of interest from the various
#--- sources necessary depending on which regions are of interest
#--- current we use London, New York and Geneva and we use the data in the
#--- seroprevalence comparison in figure 3
getRegionalCaseDeathTimeSeries <- function()
{
  
  newYorkData <- NCoVUtils::get_us_regional_cases() %>%
    dplyr::filter(state == "New York") %>%
    dplyr::rename(iso_code = state,
                  new_cases = cases,
                  new_deaths = deaths) %>%
    dplyr::select(-fips) %>%
    dplyr::mutate(country_code = "NYC",
                  country = "NYC") %>%
    dplyr::select(date, country_code, country, new_cases, new_deaths)
  
  #---- DEPRECATED as time series wasn't long enough for useful comparison
  #---- Using London seroprevalence as comparison now instead
  # wuhanData <- getWuhanCaseDeathTimeSeries() %>%
  #   dplyr::select(date, country, new_cases, new_deaths) %>%
  #   dplyr::rename(iso_code = country) %>%
  #   dplyr::mutate(iso_code = "WUH") %>%
  #   dplyr::mutate(new_deaths = dplyr::case_when(new_deaths < 0 ~ 0,
  #                                               new_deaths >= 0 ~ new_deaths))
  # 
  
  londonData <- getRegionalUKCaseDeathData() 
  
  genevaData <- readr::read_csv("covid_19/COVID19_Fallzahlen_CH_total_v2.csv") %>%
    dplyr::arrange(abbreviation_canton_and_fl, date) %>%
    dplyr::group_by(abbreviation_canton_and_fl) %>%
    padr::pad(by = "date")  %>%
    dplyr::arrange(abbreviation_canton_and_fl, date) %>%
    dplyr::mutate(date,
                  country = abbreviation_canton_and_fl,
                  new_cases = ncumul_conf - dplyr::lag(ncumul_conf), 
                  new_deaths = ncumul_deceased - dplyr::lag(ncumul_deceased)) %>%
    dplyr::select(date, country, new_cases, new_deaths) %>%
    tidyr::drop_na() %>%
    dplyr::filter(country == "GE") %>% 
    dplyr::ungroup() %>%
    dplyr::select(-abbreviation_canton_and_fl) %>%
    dplyr::mutate(country_code = "GEN",
                  country = "GE") %>%
    dplyr::select(date, country_code, country, new_cases, new_deaths)
  
  regionalDataTogether <- rbind(newYorkData, genevaData, londonData) 
  
  return(regionalDataTogether)
  
}

#--- reads in and cleans the estimates computed on the HPC from shared Dropbox folder
regionalHPCBayesianData <- function(data_path)
{
  
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  data <- dplyr::tibble(country_code = files) %>% 
    dplyr::mutate(file_contents = purrr::map(country_code, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(country_code = stringr::str_remove(country_code, "result_")) %>% 
    dplyr::mutate(country_code = stringr::str_remove(country_code, ".rds")) 
  
  return(data)
  
}

#--- Get cumulative incidence estimates for each region of the UK.
#--- Due to lack of regional death data for both Midlands regions
#--- The midlands regions are aggregated in this whole analysis
getEnglandRegionalCumulativeIncidenceEstimates <- function()
{
  
  asymptomaticPropMid <- 0.5
  asymptomaticPropLow <- 0.23
  asymptomaticPropHigh <- 0.7
  
  ukRegionalCases <- NCoVUtils::get_uk_regional_cases() %>%
    dplyr::mutate(region = ifelse(region %in% c('West Midlands', 'East Midlands'), 'Midlands', region),
                  region = ifelse(region %in% c('North East', 'Yorkshire and The Humber'), 'North East And Yorkshire', region)) %>%
    dplyr::group_by(date, region) %>%
    dplyr::summarise(cases = sum(cases)) %>%
    dplyr::arrange(region, date) 
    
  #--- these codes are made up by myself, I wasn't aware of any official codes
  #--- we combine Yorkshire and the Humber & North East and East Midlands & West Midlands
  #--- we do so as this is how the deaths data is combined and therefore our estimates also
  ukRegionalPopulations <- dplyr::tibble(
    country_code = c("EOE", "LON", "MID", "NEY", "NOW", "SOE", "SOW"),
    region = c("East of England", "London", "Midlands", "North East And Yorkshire", "North West", "South East", "South West"),
    population = c(6201214, 8908081, 10704906, 8137524, 7292093, 9133625, 5599735))
  
  dateRange <- ukRegionalCases %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) %>%
    dplyr::left_join(ukRegionalPopulations) %>% 
    tidyr::drop_na()
  
  
  regionalUKUnderReportingAllData <- regionalHPCBayesianData("~/Dropbox/bayesian_underreporting_estimates/regional_data/uk_regional_results/") %>%
    dplyr::left_join(dateRange) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>%
    dplyr::select(date, country_code, region, estimate, lower, upper) %>%
    dplyr::group_by(region) %>%
    dplyr::arrange(region, date) %>%
    dplyr::left_join(ukRegionalCases)
  
  
  ukRegionalCasesScaled <- regionalUKUnderReportingAllData %>%
    dplyr::left_join(ukRegionalPopulations) %>%
    dplyr::mutate(trueCasesMid  = smooth::sma(cases/estimate)$fitted,
                  trueCasesLow  = smooth::sma(cases/upper)$fitted,
                  trueCasesHigh = smooth::sma(cases/lower)$fitted) %>%
    dplyr::mutate(cumulativeIncidenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativeIncidenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativeIncidenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh)))
  
  
  return(ukRegionalCasesScaled)
  
  
}

#--- get adjusted cumulative incidence curves for ad-hoc regional runs of under-reporting model
#--- for comparison against seroprevalence studies
#--- currently using Geneva, New York and London as study cities
getRegionalCumulativeIncidenceEstimates <- function()
{
  
  asymptomaticPropMid <- 0.5
  asymptomaticPropLow <- 0.23
  asymptomaticPropHigh <- 0.7
  
  
  populationData <- dplyr::tibble(country_code = c("GEN", "NYC", "LON"),
                                  population  = c(499480, 19453561, 8899375))
  
  
  regionalCaseDeathTS <- getRegionalCaseDeathTimeSeries() 
  
  regionalEstimates <- regionalHPCBayesianData("~/Dropbox/bayesian_underreporting_estimates/regional_data/results_for_paper/")
  
  dateRange <- regionalCaseDeathTS %>%
    dplyr::group_by(country_code) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) %>%
    dplyr::left_join(populationData)
  

  regionalUnderreportingAndRawData <- regionalEstimates %>%
    dplyr::left_join(dateRange) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>%
    dplyr::select(date, country_code, estimate, lower, upper, population) %>%
    dplyr::left_join(regionalCaseDeathTS) %>%
    dplyr::select(-country) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(trueCasesMid  = smooth::sma(new_cases/estimate)$fitted,
                  trueCasesLow  = smooth::sma(new_cases/upper)$fitted,
                  trueCasesHigh = smooth::sma(new_cases/lower)$fitted) %>%
    dplyr::mutate(cumulativeIncidenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativeIncidenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativeIncidenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh)))
  
  
  return(regionalUnderreportingAndRawData)
  
}

#--- merge UK death data from different sources - from Kath Sherratt
getRawUKRegionalDeathData <- function()
{
  
  # PHE Dashboard deaths = England, Wales, Scotland, N Ireland; UK
  dash_deaths <- readr::read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv")
  
  
  # Hospital deaths ---------------------------------------------------------
  
  # NHS Hospital deaths = England regions
  date <- paste0(format(Sys.Date() - 1, "%d"), "-", months(Sys.Date()), "-", format(Sys.Date(), "%Y"))
  hosp_deaths_path <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/", format(Sys.Date(), "%m"), "/COVID-19-total-announced-deaths-", date, ".xlsx")
  
  # Deaths in hospitals with covid test
  hosp_deaths_tested <- openxlsx::read.xlsx(hosp_deaths_path, sheet = 3, startRow = 16, detectDates = TRUE, colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  hosp_deaths_tested <- hosp_deaths_tested[,1:(ncol(hosp_deaths_tested) - 2)]
  hosp_deaths_tested <- setNames(data.frame(t(hosp_deaths_tested[,-1])), hosp_deaths_tested[,1])
  row.names(hosp_deaths_tested) <- NULL
  hosp_deaths_tested$date <- seq.Date(from = as.Date("2020-02-29"), by = "day", length.out = nrow(hosp_deaths_tested))
  
  # Deaths in hospitals without test but with covid on death certificate
  hosp_deaths_untested <- openxlsx::read.xlsx(hosp_deaths_path, sheet = 4, startRow = 16, detectDates = TRUE, colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  hosp_deaths_untested <- hosp_deaths_untested[,1:(ncol(hosp_deaths_untested) - 2)]
  hosp_deaths_untested <- setNames(data.frame(t(hosp_deaths_untested[,-1])), hosp_deaths_untested[,1])
  row.names(hosp_deaths_untested) <- NULL
  hosp_deaths_untested$date <- seq.Date(from = as.Date("2020-02-29"), by = "day", length.out = nrow(hosp_deaths_untested))
  
  # Deaths by trust (note dates stored in cols for now)
  hosp_deaths_bytrust <- openxlsx::read.xlsx(hosp_deaths_path, sheet = 6, startRow = 16, detectDates = TRUE, colNames = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE)
  hosp_deaths_bytrust <- hosp_deaths_bytrust[2:nrow(hosp_deaths_bytrust), 1:(ncol(hosp_deaths_bytrust) - 2)]
  colnames(hosp_deaths_bytrust) <- c("region", "code", "trust_name", format(seq.Date(from = as.Date("2020-02-29"), by = "day", length.out = ncol(hosp_deaths_bytrust)-3), "%D"))
  
  return(hosp_deaths_bytrust)
  
}

#--- get tibble of time-series of cases and deaths by region
getRegionalUKCaseDeathData <- function()
{
  
  # downloading and combining raw UK death data
  hospDeathsByTrust <- getRawUKRegionalDeathData()
  
  
  # cleaning data into simple time-series by region
  ukRegionalDeaths <- hospDeathsByTrust %>% 
    dplyr::tibble() %>%
    tidyr::pivot_longer(cols = ends_with("/20")) %>%
    dplyr::select(region, date = name, deaths = value) %>%
    dplyr::mutate(date = paste0(date, "20"),
                  region = dplyr::case_when(region == "London " ~ "London",
                                            region != "London " ~ region)) %>%
    dplyr::mutate(date = lubridate::mdy(date)) %>%
    dplyr::mutate(region = dplyr::case_when(region == "East Of England" ~ "East of England",
                                            region != "East Of England" ~ region)) %>%
    dplyr::group_by(date, region) %>%
    dplyr::summarise(deaths = sum(deaths)) %>%
    dplyr::arrange(region, date) 
  
  # downloading UK case data and cleaning into simple time-series by region
  ukRegionalCases <- NCoVUtils::get_uk_regional_cases() %>%
    dplyr::filter(region != "Scotland" & region != "Wales" & region != "Northern Ireland") %>%
    dplyr::select(-country) %>%
    dplyr::mutate(region = ifelse(region %in% c('West Midlands', 'East Midlands'), 'Midlands', region),
                  region = ifelse(region %in% c('North East', 'Yorkshire and The Humber'), 'North East And Yorkshire', region)) %>%
    dplyr::group_by(date, region) %>%
    dplyr::summarise(cases = sum(cases)) %>%
    dplyr::arrange(date, region) 
  
  
  ukRegionalData <- ukRegionalCases %>% 
    dplyr::right_join(ukRegionalDeaths, by = c("date", "region")) %>%
    dplyr::mutate(cases  = tidyr::replace_na(cases, 0),
                  deaths = tidyr::replace_na(deaths, 0)) %>% 
    dplyr::mutate(country_code = dplyr::case_when(region == "East of England" ~ "EOE",
                                              region == "London" ~ "LON",
                                              region == "Midlands" ~ "MID",
                                              region == "North East And Yorkshire" ~ "NEY",
                                              region == "North West" ~ "NOW",
                                              region == "South East" ~ "SOE",
                                              region == "South West" ~ "SOW")) %>%
    dplyr::mutate(country  = dplyr::case_when(region == "East of England" ~ "EOE",
                                              region == "London" ~ "LON",
                                              region == "Midlands" ~ "MID",
                                              region == "North East And Yorkshire" ~ "NEY",
                                              region == "North West" ~ "NOW",
                                              region == "South East" ~ "SOE",
                                              region == "South West" ~ "SOW")) %>%
    dplyr::select(date, country_code, country, new_cases = cases, new_deaths = deaths)
  

    return(ukRegionalData)
  
}



