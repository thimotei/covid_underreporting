here::here() %>% setwd() 

source("covid_underreporting/R/data_helper_functions.R")


underascertainment_data <- all_underascertainment_data()

eu_countries <- dplyr::tibble(iso_code =
                                c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP", "FRA", "HRV", "ITA",
                                  "CYP", "LVA", "LTU", "LUX", "HUN", "MLT", "NLD", "AUT", "POL", "PRT", "ROU", "SVN",
                                  "SVK", "FIN", "SWE")) %>% dplyr::arrange()


# underascertainment_data_europe <- underascertainment_data %>%
#   dplyr::left_join(eu_countries) %>%
#   dplyr::select(date, iso_code, estimate, lower, upper)

average_underascertainment_march <- underascertainment_data %>%
  dplyr::filter(iso_code %in% dplyr::pull(eu_countries)) %>%
  dplyr::filter(date >= "2020-03-01" & date <= "2020-03-31") %>%
  dplyr::summarise(average_underreporting_high  = min(estimate),
                   average_underreporting_low  = max(estimate))

average_underascertainment_april <- underascertainment_data %>%
  dplyr::filter(iso_code %in% dplyr::pull(eu_countries)) %>%
  dplyr::filter(date >= "2020-04-01" & date <= "2020-04-30") %>%
  dplyr::summarise(average_underreporting_high  = min(estimate),
                   average_underreporting_low  = max(estimate))

average_underascertainment_may <- underascertainment_data %>%
  dplyr::filter(iso_code %in% dplyr::pull(eu_countries)) %>%
  dplyr::filter(date >= "2020-05-01" & date <= "2020-05-31") %>%
  dplyr::summarise(average_underreporting_high  = min(estimate),
                   average_underreporting_low  = max(estimate))
