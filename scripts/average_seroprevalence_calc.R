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
  #dplyr::group_by(country) %>%
  dplyr::summarise(seroprevalence_mean_mid  = signif(mean(cumulative_incidence_mid*100), 3),
                   seroprevalence_mean_low  = signif(mean(cumulative_incidence_low*100), 3),
                   seroprevalence_mean_high = signif(mean(cumulative_incidence_high*100, 3)))


south_american_and_african_countries <- country_code_data %>% 
  dplyr::filter(Continent_Name == "South America" | Continent_Name == "Africa") %>%
  dplyr::rename(iso_code = Three_Letter_Country_Code)

south_american_and_african_seroprevalence_estimates <- adjusted_cases %>%
  #dplyr::group_by(country) %>%
  dplyr::filter(iso_code %in% dplyr::pull(south_american_and_african_countries, iso_code)) %>% 
  dplyr::select(date, iso_code, country, cumulative_incidence_mid, cumulative_incidence_low, cumulative_incidence_high) %>%
  tidyr::drop_na()


south_american_and_african_seroprevalence_mean <- south_american_and_african_seroprevalence_estimates %>%
  #dplyr::group_by(country) %>%
  dplyr::summarise(seroprevalence_mean_mid  = signif(mean(cumulative_incidence_mid*100), 3),
                   seroprevalence_mean_low  = signif(mean(cumulative_incidence_low*100), 3),
                   seroprevalence_mean_high = signif(mean(cumulative_incidence_high*100, 3)))
