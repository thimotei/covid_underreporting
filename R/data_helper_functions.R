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
  
  
  dateRange <- allDatRaw %>%
    dplyr::group_by(countryCode) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) 
  
  
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
    dplyr::left_join(dateRange) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>%
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

getRegionalCaseDeathTimeSeries <- function()
{
  
  newYorkData <- NCoVUtils::get_us_regional_cases() %>%
    dplyr::filter(state == "New York") %>%
    dplyr::rename(iso_code = state,
                  new_cases = cases,
                  new_deaths = deaths) %>%
    dplyr::select(-fips) %>%
    dplyr::mutate(iso_code = "NYC")
  
  
  wuhanData <- getWuhanCaseDeathTimeSeries() %>%
    dplyr::select(date, country, new_cases, new_deaths) %>%
    dplyr::rename(iso_code = country) %>%
    dplyr::mutate(iso_code = "WUH") %>%
    dplyr::mutate(new_deaths = dplyr::case_when(new_deaths < 0 ~ 0,
                                                new_deaths >= 0 ~ new_deaths))
  
  
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
    dplyr::rename(iso_code = country) %>%
    dplyr::mutate(iso_code = "GEN")
  
  regionalDataTogether <- rbind(newYorkData, genevaData, wuhanData) %>%
    dplyr::mutate(country = iso_code) %>%
    dplyr::select(date, iso_code, country, new_cases, new_deaths)
  
  return(regionalDataTogether)
  
}

# read in HPC output data neatly

regionalHPCBayesianData <- function()
{
  
  data_path <- "~/Dropbox/bayesian_underreporting_estimates/regional_data/results/"
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  data <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) 
  
  return(data)
  
}

getCumulativeIncidenceEstimates <- function()
{
  
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

getRegionalCumulativeIncidenceEstimates <- function()
{
  
  asymptomaticPropMid <- 0.5
  asymptomaticPropLow <- 0.23
  asymptomaticPropHigh <- 0.7
  
  
  populationData <- dplyr::tibble(countryCode = c("GEN", "NYC", "WUH"),
                                  population  = c(499480, 19453561, 8896900))
  
  
  regionalCaseDeathTS <- getRegionalCaseDeathTimeSeries() %>%
    dplyr::rename(countryCode = iso_code)
  
  regionalEstimates <- regionalHPCBayesianData()
  
  dateRange <- regionalCaseDeathTS %>%
    dplyr::group_by(countryCode) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) %>%
    dplyr::left_join(populationData)
  
  
  regionalUnderreportingAndRawData %>%
    dplyr::group_by(countryCode) %>%
    dplyr::summarise(minDate = min(date),
                     maxDate = max(date)) %>%
    dplyr::left_join(populationData)
  
  
  regionalUnderreportingAndRawData <- regionalEstimates %>%
    dplyr::left_join(dateRange) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(date = seq(unique(maxDate) - 13 - dplyr::n() + 1, unique(maxDate) - 13, 1)) %>%
    dplyr::select(date, countryCode, estimate, lower, upper, population) %>%
    dplyr::left_join(regionalCaseDeathTS) %>%
    dplyr::select(-country) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(trueCasesMid  = smooth::sma(new_cases/estimate)$fitted,
                  trueCasesLow  = smooth::sma(new_cases/upper)$fitted,
                  trueCasesHigh = smooth::sma(new_cases/lower)$fitted) %>%
    dplyr::mutate(cumulativeIncidenceMid  = cumsum(trueCasesMid)/(population*(1 - asymptomaticPropMid)),
                  cumulativeIncidenceLow  = cumsum(trueCasesLow)/(population*(1 - asymptomaticPropLow)),
                  cumulativeIncidenceHigh = cumsum(trueCasesHigh)/(population*(1 - asymptomaticPropHigh)))
  
  
  return(regionalUnderreportingAndRawData)
  
}

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

getRegionalUKCaseDeathData <- function()
{
  
  # downloading and combining raw UK death data
  hospDeathsByTrust <- getRawUKRegionalDeathData()
  
  
  # cleaning data into simple time-series by region
  ukRegionalDeaths <- hosp_deaths_bytrust %>% 
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
    dplyr::select(date, iso_code, country, new_cases = cases, new_deaths = deaths)
  

    return(ukRegionalData)
  
}

readr::write_csv(UKData, "covid_underreporting/data/regionalCaseDeathTimeSeriesUK.csv")







