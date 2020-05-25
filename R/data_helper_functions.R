getUnderReportingAndTestingData <- function()
{
  
  #---------- reading in the ECDC data ----------#
  
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
  
  #---------- reading in the under-reporting estimates from the shared dropbox ----------#
  
  data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates/"
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  data <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(date = seq(Sys.Date() - 13 - dplyr::n()  + 1, Sys.Date() - 13, 1)) %>% 
    dplyr::select(date, everything()) %>%
    dplyr::left_join(countryCodesLookUp) %>%
    dplyr::select(date, country, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup()
  
  #------ combining the case & death data from ECDC with our under-reporting estimates
  #------ into a single tibble 
  
  dataCases <- data %>% 
    dplyr::left_join(allDatRaw, by = c("date", "country", "countryCode"), copy = FALSE) %>%
    dplyr::select(date, country, countryCode, cases, deaths) %>%
    tidyr::drop_na()
  
  dataCasesAndEstimates <- dataCases %>%
    dplyr::right_join(data, by = c("country", "date", "countryCode"), copy = FALSE) %>%
    tidyr::drop_na() %>% 
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " "))
  
  #---------- reading in the OWID data and munging it together with the other data frame ----------#
  
  owidData <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
    dplyr::select(date, 
                  countryCode = iso_code,
                  country = location,
                  newTests = new_tests,
                  newCases = new_cases) %>% 
    tidyr::drop_na() 
  
  allDataTogether <- owidData %>%
    dplyr::right_join(dataCasesAndEstimates, by = c("date", "country")) %>%
    tidyr::drop_na() %>%
    dplyr::select(date, country, cases, deaths, newTests, estimate, lower, upper) %>%
    dplyr::arrange(country, date) %>%
    unique() %>% 
    dplyr::mutate(testsPerCase = newTests/cases) %>% 
    dplyr::group_by(country) %>%
    dplyr::select(date, upper, lower, estimate, testsPerCase) %>%
    dplyr::na_if(Inf) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(upper = upper*100, lower = lower*100, estimate = estimate*100,
                  testsPerCaseMA = imputeTS::na_ma(testsPerCase, k = 4, weighting = "linear")) %>%
    dplyr::mutate(testsPerCaseMA = forecast::ma(testsPerCaseMA, order = 4, centre = TRUE)) %>%
    dplyr::filter(country != "Argentina")
  
}

getAdjustedCaseData <- function()
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
  
  deathSummaryData <- allDatRaw %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalDeaths = sum(deaths)) %>% 
    dplyr::mutate(country_order = rank(totalDeaths)) %>%
    dplyr::arrange(desc(country_order))
  
  caseSummaryData <- allDatRaw %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalCases = sum(cases))
  
  
  data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates/"
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  dataTmp <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(date = seq(Sys.Date() - 13 - dplyr::n()  + 1, Sys.Date() - 13, 1)) %>% 
    dplyr::select(date, everything()) %>%
    dplyr::left_join(countryCodesLookUp) %>%
    dplyr::select(date, country, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup()
  
  underReportingData <- dataTmp %>% 
    dplyr::left_join(deathSummaryData, by = c('country' = 'country')) %>% 
    dplyr::left_join(caseSummaryData, by = c('country' = 'country')) %>% 
    dplyr::arrange(desc(country_order), date) 
  
  
  dataTrueCases <- allDatRaw %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::left_join(underReportingData) %>%
    dplyr::select(date, country, cases, deaths, estimate, lower, upper) %>%
    dplyr::group_by(country) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(trueCasesMid  = smooth::sma(cases/estimate)$fitted,
                  trueCasesLow  = smooth::sma(cases/upper)$fitted,
                  trueCasesHigh = smooth::sma(cases/lower)$fitted) %>%
    dplyr::ungroup(country) %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::filter(!(country == "China" & date > "2020-03-15")) %>%
    dplyr::filter(dplyr::n() > 20) 
  
  return(dataTrueCases)
  
}

getIncidenceUpToDateData <- function(data = allCumulativeIncidenceEstimates, dateInput = NA)
{
  data %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::group_by(country) %>%
    dplyr::filter(if(is.null(dateInput)){date == max(date)}else{date == dateInput}) %>%
    dplyr::ungroup(country) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United Kingdom" ~ "UK",
                                             country != "United Kingdom" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America" ~ "USA",
                                             country != "United States of America" ~ country)) %>%
    # dplyr::mutate(country = dplyr::case_when(country == "China" ~ "China",
    #                                          country != "China" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Congo" ~ "Republic of Congo",
                                             country != "Congo" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Czechia" ~ "Czech Republic",
                                             country != "Czechia" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "North Macedonia" ~ "Macedonia",
                                             country != "North Macedonia" ~ country)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United Republic of Tanzania" ~ "Tanzania",
                                             country != "United Republic of Tanzania" ~ country))
  
}


combineMapAndIncidenceData <- function(dataToPlot)
{
  
  ggplot2::map_data("world") %>%
    dplyr::rename(country = region) %>%
    dplyr::filter(country != "Antarctica") %>%
    dplyr::left_join(dataToPlot, by = "country") %>%
    dplyr::select(country, lat, long, group, country, cumulativePrevalenceMid)
  
}


getCumulativeIncidenceEstimates <- function()
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
  
  
  allPrevalenceEstimates <- allAdjustedCaseData %>% 
    dplyr::left_join(worldPopulationEstimatesClean) %>% 
    dplyr::group_by(country) %>%
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::mutate(cumulativePrevalenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativePrevalenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativePrevalenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh))) %>%
    dplyr::mutate(cumulativePrevalenceMid = dplyr::case_when(cumulativePrevalenceMid >= 1 ~ 1,
                                                             cumulativePrevalenceMid <= 0 ~ 0,
                                                             cumulativePrevalenceMid > 0 & cumulativePrevalenceMid < 1 ~ cumulativePrevalenceMid)) %>%
    dplyr::mutate(cumulativePrevalenceLow = dplyr::case_when(cumulativePrevalenceLow > 1 ~ 1,
                                                             cumulativePrevalenceLow < 0 ~ 0,
                                                             cumulativePrevalenceLow > 0 & cumulativePrevalenceLow < 1 ~ cumulativePrevalenceLow)) %>%
    dplyr::mutate(cumulativePrevalenceHigh = dplyr::case_when(cumulativePrevalenceHigh > 1 ~ 1,
                                                              cumulativePrevalenceHigh < 0 ~ 0,
                                                              cumulativePrevalenceHigh > 0 & cumulativePrevalenceHigh < 1 ~ cumulativePrevalenceHigh)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()
  
  return(allPrevalenceEstimates)
  
}

