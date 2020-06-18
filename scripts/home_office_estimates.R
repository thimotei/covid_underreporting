globalPrevalenceEstimates <- function()
{
  
  asymptomaticEstimateMid <- 0.50
  asymptomaticEstimateLow <- 0.23
  asymptomaticEstimateHigh <- 0.70
  
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
  
  
  data_path <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted_not_age_adjusted/"
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
    dplyr::select(date, everything()) %>%
    dplyr::left_join(countryCodesLookUp) %>%
    dplyr::left_join(allDatRaw) %>%
    dplyr::select(date, country, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
                                             country != "Cote dIvoire" ~ country)) 
  
  
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
  
  allTogether <- dataTmp %>% 
    dplyr::left_join(worldPopulationEstimatesClean) %>%
    dplyr::group_by(country) %>%
    dplyr::select(date, country, countryCode, cases, estimate, lower, upper, population) %>%
    dplyr::arrange(country, date)
  
  
  newCaseEstimatesRecent <- allDatRaw %>%
    dplyr::filter(Sys.Date() - 13 < date) %>% 
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalNewCases = sum(cases)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
                                             country != "Cote dIvoire" ~ country)) 
  
  
  caseEstimatesTotal <- allDatRaw %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalCases = sum(cases)) %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Côte d'Ivoire",
                                             country != "Cote dIvoire" ~ country)) 
  
  
  recentUnderreportingEstimates <- allTogether %>%
    dplyr::filter(date == max(date)) %>% 
    dplyr::group_by(country) %>%
    dplyr::select(estimate, lower, upper)
  
  # turn off scientific notation
  options(scipen=999)
  
  mostRecentEstimatesTogether <- recentUnderreportingEstimates %>% 
    dplyr::left_join(newCaseEstimatesRecent) %>%
    dplyr::right_join(caseEstimatesTotal) %>%
    dplyr::left_join(worldPopulationEstimatesClean) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::mutate(propCurrentlyInfMid  = (totalNewCases/(estimate*asymptomaticEstimateMid)/population),
                  propCurrentlyInfLow  = (totalNewCases/(upper*(1 - asymptomaticEstimateLow))/population),
                  propCurrentlyInfHigh = (totalNewCases/(lower*(1 - asymptomaticEstimateHigh))/population)) %>%
    dplyr::mutate(propCurrentlyInfMid = signif(propCurrentlyInfMid, 2),
                  propCurrentlyInfLow = signif(propCurrentlyInfLow, 2),
                  propCurrentlyInfHigh = signif(propCurrentlyInfHigh, 2)) %>%
    dplyr::select(country, totalCases, totalNewCases, estimate, lower, upper, population, propCurrentlyInfMid, propCurrentlyInfLow, propCurrentlyInfHigh)
 
  return(mostRecentEstimatesTogether)
   
}


tmp <- globalPrevalenceEstimates()

write.csv(tmp, "covid_underreporting/home_office_estimates/currentPrevalenceEstimates_29_05_2020.csv")
