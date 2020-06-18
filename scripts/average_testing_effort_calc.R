here::here() %>% setwd()

source("covid_underreporting/R/data_helper_functions.R")

under_reporting_and_testing_data <- getUnderReportingCaseDeathPopulationAndTestingData()
  
adjusted_cases <- getAdjustedCaseDataNational()

country_code_data <- readr::read_csv("covid_underreporting/data/country_data.csv")
european_countries <- country_code_data %>% 
  dplyr::filter(Continent_Name == "Europe") %>%
  dplyr::rename(iso_code = Three_Letter_Country_Code)

all_testing_effort_data <- under_reporting_and_testing_data %>%
  dplyr::left_join(adjusted_cases) %>%
  dplyr::mutate(testing_effort  = dplyr::na_if(new_tests/new_cases, "Inf")) %>%
  dplyr::select(date, iso_code, country, testing_effort) %>%
  dplyr::left_join(european_countries) %>%
  dplyr::select(date, iso_code, country, testing_effort) %>%
  tidyr::drop_na()
  
uk_early_march_testing <- all_testing_effort_data %>%
  dplyr::filter(date >= "2020-03-01" & date <= "2020-03-13") %>%
  dplyr::filter(country == "United Kingdom") %>% 
  dplyr::summarise(mean_testing_early_march = mean(testing_effort))
  
testing_effort_march <- all_testing_effort_data %>%
  dplyr::filter(date >= "2020-03-01" & date <= "2020-03-31") %>%
  dplyr::summarise(average_testing_effort  = mean(testing_effort)) %>%
  dplyr::mutate(average_testing_effort = signif(average_testing_effort, 3)) %>%
  dplyr::select(average_testing_effort)

testing_effort_april <- all_testing_effort_data %>%
  dplyr::filter(date >= "2020-04-01" & date <= "2020-04-30") %>%
  dplyr::summarise(average_testing_effort  = mean(testing_effort)) %>%
  dplyr::mutate(average_testing_effort = signif(average_testing_effort, 3)) %>%
  dplyr::select(average_testing_effort)

testing_effort_may <- all_testing_effort_data %>%
  dplyr::filter(date >= "2020-05-01" & date <= "2020-05-31") %>%
  dplyr::summarise(average_testing_effort  = mean(testing_effort)) %>%
  dplyr::mutate(average_testing_effort = signif(average_testing_effort, 3)) %>%
  dplyr::select(average_testing_effort)
