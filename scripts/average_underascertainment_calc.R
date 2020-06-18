all_underascertainment_data <- function()
{

  underReportingPath <- "~/Dropbox/bayesian_underreporting_estimates/current_estimates_extracted_not_age_adjusted/"
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
  
}

underascertainment_data <- all_underascertainment_data()

european_union_two_letter_codes <- c('AT', 'BE', 'BG', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'GR', 'HU', 'HR', 'IE', 'IT', 'LV', 'LT', 'LU', 'MT', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'GB')

country_code_data <- readr::read_csv("covid_underreporting/data/country_data.csv")
european_union_countries <- country_code_data %>% 
  dplyr::filter(Continent_Name == "Europe") %>%
  dplyr::rename(iso_code = Three_Letter_Country_Code)


underascertainment_data_europe <- underascertainment_data %>%
  dplyr::left_join(european_union_countries) %>%
  dplyr::select(date, iso_code, estimate, lower, upper)

average_underascertainment_march <- underascertainment_data_europe %>%
  dplyr::filter(date >= "2020-03-01" & date <= "2020-03-31") %>%
  dplyr::summarise(average_underreporting_mid  = mean(estimate),
                   average_underreporting_low  = mean(lower),
                   average_underreporting_high = mean(upper)) %>%
  dplyr::mutate(average_underreporting = paste0(signif(average_underreporting_mid*100, 3),
                                                "% (", signif(average_underreporting_low*100, 3), 
                                                "% - ", signif(average_underreporting_high*100, 3), "%)")) %>%
  dplyr::select(average_underreporting)

average_underascertainment_april <- underascertainment_data_europe %>%
  dplyr::filter(date >= "2020-04-01" & date <= "2020-04-30") %>%
  dplyr::summarise(average_underreporting_mid  = mean(estimate),
                   average_underreporting_low  = mean(lower),
                   average_underreporting_high = mean(upper)) %>%
  dplyr::mutate(average_underreporting = paste0(signif(average_underreporting_mid*100, 3),
                                                "% (", signif(average_underreporting_low*100, 3), 
                                                "% - ", signif(average_underreporting_high*100, 3), "%)")) %>%
  dplyr::select(average_underreporting)

average_underascertainment_may <- underascertainment_data_europe %>%
  dplyr::filter(date >= "2020-05-01" & date <= "2020-05-31") %>%
  dplyr::summarise(average_underreporting_mid  = mean(estimate),
                   average_underreporting_low  = mean(lower),
                   average_underreporting_high = mean(upper)) %>%
  dplyr::mutate(average_underreporting = paste0(signif(average_underreporting_mid*100, 3),
                                                "% (", signif(average_underreporting_low*100, 3), 
                                                "% - ", signif(average_underreporting_high*100, 3), "%)")) %>%
  dplyr::select(average_underreporting)
