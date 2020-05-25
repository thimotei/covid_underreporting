#----------- regional data analysis, for comparison with seroprevalence results where the national estimates are a biased estimate

USRegionalData <- NCoVUtils::get_us_regional_cases()

newYorkData <- USRegionalData %>%
  dplyr::filter(state == "New York")


swissData <- readr::read_csv("covid_19/COVID19_Fallzahlen_CH_total_v2.csv") %>%
  dplyr::arrange(abbreviation_canton_and_fl, date) %>%
  dplyr::group_by(abbreviation_canton_and_fl) %>%
  padr::pad(by = "date")  %>%
  dplyr::arrange(abbreviation_canton_and_fl, date) 