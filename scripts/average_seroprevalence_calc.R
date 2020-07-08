here::here() %>% setwd()

source("covid_underreporting/R/data_helper_functions.R")

adjusted_cases <- getAdjustedCaseDataNational() 

country_code_data <- readr::read_csv("covid_underreporting/data/country_data.csv")
european_countries <- country_code_data %>%
  dplyr::filter(Continent_Name == "Europe") %>%
  dplyr::rename(iso_code = Three_Letter_Country_Code)


european_seroprevalence_estimates <- adjusted_cases %>%
  #dplyr::group_by(country) %>%
  dplyr::filter(iso_code %in% dplyr::pull(european_countries, iso_code)) %>% 
  dplyr::select(date, iso_code, country, cumulative_incidence_mid, cumulative_incidence_low, cumulative_incidence_high) %>%
  tidyr::drop_na()


european_seroprevalence_mean <- european_seroprevalence_estimates %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup(country) %>%
  dplyr::summarise(seroprevalence_mean_low  = signif(min(cumulative_incidence_mid*100), 3),
                   seroprevalence_mean_high  = signif(max(cumulative_incidence_mid*100), 3))


south_american_and_african_countries <- country_code_data %>% 
  dplyr::filter(Continent_Name == "South America" | Continent_Name == "Africa") %>%
  dplyr::rename(iso_code = Three_Letter_Country_Code)

south_american_and_african_seroprevalence_estimates <- adjusted_cases %>%
  #dplyr::group_by(country) %>%
  dplyr::filter(iso_code %in% dplyr::pull(south_american_and_african_countries, iso_code)) %>% 
  dplyr::select(date, iso_code, country, cumulative_incidence_mid, cumulative_incidence_low, cumulative_incidence_high) %>%
  tidyr::drop_na()


south_american_and_african_seroprevalence_mean <- south_american_and_african_seroprevalence_estimates %>%
  dplyr::group_by(country) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup(country) %>%
  dplyr::summarise(seroprevalence_mean_low  = signif(min(cumulative_incidence_mid*100), 3),
                   seroprevalence_mean_high  = signif(max(cumulative_incidence_mid*100), 3))


countries_of_interest <- c("Belgium", "France", "Italy", "Spain")

adjusted_cases %>%
  dplyr::filter(country %in% countries_of_interest) %>%
  dplyr::filter(date > "2020-03-01" &  date > "2020-03-31") %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(cum_inc = max(cumulative_incidence_mid*100))
  
  
topCountries <- adjusted_cases %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(total_cases = cumsum(new_cases)) %>%
  dplyr::filter(total_cases == max(total_cases)) %>%
  dplyr::ungroup(country) %>%
  dplyr::top_n(8)


  